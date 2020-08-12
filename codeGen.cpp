#include <iostream>
#include <sstream>
#include <vector>
#include <list>
#include <set>
#include <string>
#include <map>
#include <bitset>
#include <queue>

#include "eeyore.hpp"
#include "codeGen.hpp"
#include "utility.hpp"
#include "error.hpp"

const int INTMAX = ~(1<<31);

const std::vector<Inst::Register> availableReg = {
    Inst::a0, Inst::a1, Inst::a2, Inst::a3, Inst::a4, Inst::a5, Inst::a6, Inst::a7,
    Inst::t3, Inst::t4, Inst::t5, Inst::t6,
    //Inst::s1, Inst::s2, Inst::s3, Inst::s4,
    //Inst::s5, Inst::s6, Inst::s7, Inst::s8, Inst::s9, Inst::s10, Inst::s11
};

const std::vector<Inst::Register> savedReg = {
    Inst::s1, Inst::s2, Inst::s3, Inst::s4, Inst::s5, Inst::s6, Inst::s7, Inst::s8,
    Inst::s9, Inst::s10, Inst::s11
};

const std::vector<Inst::Register> notSavedReg = {
    Inst::a0, Inst::a1, Inst::a2, Inst::a3, Inst::a4, Inst::a5, Inst::a6, Inst::a7,
    Inst::t3, Inst::t4, Inst::t5, Inst::t6
};

void Inst::conversion(std::list<Quad> &quads, std::ostream &os)
{
    // .text section header
    emitLine(".global main", os);
    emitLine(".text", os);

    // .data section
    std::stringstream data;
    emitLine(".data", data);

    auto quad = quads.begin();
    while (quad != quads.end()) {
        // global definitions
        switch(quad->op) {
        case Quad::DEFVAR:
            emitLabel(quad->arg1, data);
            emitLine(".space 4", data);
            break;
        case Quad::DEFVEC:
            emitLabel(quad->arg1, data);
            emitLine(".space 4 * " + std::string(quad->arg2), data);
            break;
        default:
            buildProc(quad, os);
            break;
        }
        quad++;
    }

    os << data.str();
}


// 0. build stack frame, add all move all function parameters to stack
//    since gcc seem to be doing this a lot...
// 1. convert quads to target code, generate local symtable,
//    build stack frame for local vectors and fill local/global vector reference.
//    special directives are inserted for later use, including parameter list,
//    return value and function exit point.
// 2. build control flow graph, register allocation using linear scan,
//    allocate ONLY locally defined variables
//    that is, excluding vectors, global variables and parameters
// 3. build stack for spilled variables, complete variable reference and final setup
void buildProc(std::list<Quad>::iterator &quad, std::ostream &os)
{
    std::cerr << "buildProc: new proc" << std::endl;
    // part 0, function header
    if (quad->op != Quad::DEFF) throw QUADBUG;
    emitLabel(quad->arg1.name.substr(2), os); // trim the f_ prefix
    
    std::list<Inst> body;
    std::vector<LocalVar> stackFrame;
    std::set<std::string> localSymTab; // DO include parameters

    // fisrt 16 byte for old fp and ra
    size_t frameSize = 16;

    // insert move operations for all funcion parameters
    // i.e. addi pi, ai, 0
    for (int i = 0; i < quad->arg2.constant; ++i) {
        pseudoReg parameter = GENNAME("p", i);
        localSymTab.insert(parameter);
        Inst newInst(Inst::MOV, parameter, GENNAME("a", i), 0);
        newInst.rs1 = Inst::Register(Inst::a0 + i);
        newInst.assigned1 = true;
        body.push_back(newInst);
    }

    std::cerr << "buildProc: parameter processed" << std::endl;
    
    // part 1, code generation
    // function parameter, return value is left until registr allocation
    quad++; // skip DEFF
    instSelect(quad, body, stackFrame, localSymTab, frameSize);
    std::cerr << "buildProc: inst selected" << std::endl;

    // part 2. register allocation
    // with special directive PARAM and RETVAL to restrict allocation
    // 2.1 build CFG
    BasicBlock entry, exit;
    std::vector<BasicBlock> blocks; // does NOT include entry and exit
    buildCFG(body, entry, exit, blocks);
	
    for (auto &b: blocks) {
        auto it = b.begin;
        std::cerr << "block " << b.index << std::endl;
        while (it != b.end) std::cerr >> *it++;
        std::cerr << std::endl;
    }
	
    std::cerr << "buildProc: CFG built" << std::endl;
	
    // 2.2 block ordering
    std::vector<BasicBlock*> orderedBlocks;
    blockOrder(blocks, &entry, orderedBlocks);
	
    std::set<size_t> blockWithOrder;
    for (auto b: orderedBlocks) blockWithOrder.insert(b->index);
	
    for (auto it = blocks.rbegin(); it != blocks.rend(); ++it) {
        // this has to be done backwards
        if (blockWithOrder.find(it->index) == blockWithOrder.end()) {
            body.erase(it->begin, it->end);
            // remove pred link from successors
            for (auto &succ: it->successor) {
                auto pred = succ->predecessor.find(&*it);
                if (pred != succ->predecessor.end())
                    succ->predecessor.erase(pred);
            }
            auto tmp = it;
            tmp++; // the first block is always alive so this is safe
            // previous block's end iterator point to this block's begin
            // so need to manually change that to the next block's begin
            // i.e. this block's end
            tmp->end = it->end;
			
            // blocks.erase(tmp); // WARNING: DO NOT ERASE
            it->successor.clear();
        }
    }
	
    for (auto b: blocks) {
		
    }
	
    std::cerr << "buildProc: block ordered" << std::endl;
    // numbered instruction
    unsigned opId = 0;
    for (auto block: orderedBlocks) {
        std::cerr << "block " << block->index << " pred ";
        for (auto p: block->predecessor) std::cerr << p->index << " ";
        std::cerr << " succ ";
        for (auto p: block->predecessor) std::cerr << p->index << " ";
        std::cerr << std::endl;
        for (auto inst = block->begin; inst != block->end; ++inst) {
            std::cerr >> *inst;
            inst->ID = (opId++)*2;
        }
    }
	
    std::cerr << "buildProc: ????? before" << std::endl;

    // make a map from virtual/physical register to index for quick query
    std::map<std::string, int> vr2index;
    for (auto pair: Inst::reg2name)
        vr2index[pair.second] = pair.first;
    int index = 32; // start from 32 so no conflict between physical regs
    for (auto vr: localSymTab)
        vr2index[vr] = index++;
	
    std::cerr << "buildProc: liveness before" << std::endl;
    
    // 2.3 liveness analysis
    for (auto block: orderedBlocks)
        blockLiveSet(block, vr2index);
    bool change = true;
    while(change) {
        change = false;
        for (auto block = orderedBlocks.rbegin(); block != orderedBlocks.rend(); ++block) {
            auto b = *block;
            for (auto &succ: b->successor) {
                auto tmp = b->liveOut;
                b->liveOut |= succ->liveIn;
                if (tmp != b->liveOut) change = true;
            }
            auto tmp = b->liveIn;
            b->liveIn = (b->liveOut & ~b->liveKill) | b->liveGen;
            if (tmp != b->liveIn) change = true;
        }
    }
	
    for (auto block: orderedBlocks) {
        std::cerr << "block " << block->index << " predecessor: ";
        for (auto pred: block->predecessor) std::cerr << pred->index << " ";
        std::cerr << " successor: ";
        for (auto succ: block->successor) std::cerr << succ->index << " ";
        std::cerr << std::endl;
        std::cerr << " liveIn ";
        for (auto reg: availableReg)
            if (block->liveIn.test(reg))
                std::cerr << Inst::reg2name.at(reg) << " ";
        for (auto sym: localSymTab)
            if (block->liveIn.test(vr2index[sym]))
                std::cerr << sym << " ";
        std::cerr << " liveOut ";
        for (auto reg: availableReg)
            if (block->liveOut.test(reg))
                std::cerr << Inst::reg2name.at(reg) << " ";
        for (auto sym: localSymTab)
            if (block->liveOut.test(vr2index[sym]))
                std::cerr << sym << " ";
        std::cerr << std::endl;
		
        for (auto inst = block->begin; inst != block->end; ++inst)
            std::cerr >> *inst;
		
        std::cerr << std::endl;
    }
	
    // build interval
    std::vector<Interval> intervals;
    intervals.resize(32+localSymTab.size()); // physical regs and vrs
    for (int i = 0; i < 32; ++i) {
        intervals[i].fixed = true;
        intervals[i].assignedReg = (Inst::Register)i;
    }
	
    // intervals with pre-allocated physical regs are fixed
    buildInterval(orderedBlocks, vr2index, intervals, localSymTab.size());
    // for saved regs (s1-11, note: s0(fp) is reserved),
    // live range is entire function, but never used
    for (auto sym: localSymTab) {
        auto &itv = intervals[vr2index[sym]];
        if (!itv.hasRange())
            // in some cases a variable is only set but never used
            // it won't have a range, which causes big trouble
            // manually add some 'ranges' for it
            for (auto position: itv.usePosition) itv.addRange(position, position);
    }
	
    std::cerr << "buildProc: interval built" << std::endl;
	
//
//    for (auto reg: availableReg) {
//        std::cerr << "interval " << Inst::reg2name.at(reg) << ":" << std::endl;
//        std::cerr << intervals[reg] << std::endl;
//    }
//    for (auto sym: localSymTab) {
//        std::cerr << "interval " << sym << " " << std::endl;
//        std::cerr << intervals[vr2index[sym]];
//    }
	
    // 2.4 register allocation
    // register to allocate: a0-7, t3-6, s1-11 (highest priority to lowest)
    // t1-2 is saved for later stage (for possible future need)
    int spillSlot = 0;
    walkInterval(intervals, orderedBlocks, spillSlot);
	
    for (auto reg: availableReg) {
        std::cerr << "interval " << Inst::reg2name.at(reg) << ":" << std::endl;
        std::cerr << intervals[reg] << std::endl;
    }
    for (auto sym: localSymTab) {
        std::cerr << "interval " << sym << " " << std::endl;
        std::cerr << intervals[vr2index[sym]];
    }
	
    // insert move/spill, resolve data flow
    insertInst(body, intervals, orderedBlocks, frameSize);
    
    for (auto &from: blocks) {
        if (!from.predecessor.size()) continue;
        // do not do anything for output dead blocks
        std::vector<std::vector<Inst>> edgeInsts;
        bool isSeq;
        for (auto &to: from.successor) {
            std::cerr << "from " << from.index << " to " << to->index << std::endl;
            if (to->begin == from.end) isSeq = true;
            std::vector<Inst> loads, ordered;
            Dependency dependency;
            for (auto sym: localSymTab) {
                auto opr = vr2index[sym];
                if (!to->liveIn.test(opr)) continue;
                std::cerr << "check " << sym << std::endl;
				
                Interval *parent = &intervals[opr];
                Interval *fromInt = parent->childAtReverse(from.last->ID+2);
                Interval *toInt = parent->childAt(to->begin->ID);
                
                if (fromInt != toInt) {
                    auto fSpill = fromInt->spillSlot;
                    auto tSpill = toInt->spillSlot;
                    auto fReg = (Inst::Register)fromInt->assignedReg;
                    auto tReg = (Inst::Register)toInt->assignedReg;
                    if (fSpill == -1 && tSpill == -1) {
                        if (!(fReg == tReg)) {
                            std::cerr << "insert move between " << from.index
                                      << " and " << to->index << " from " << fReg << " to "
                                      << tReg << std::endl;
                            // simple move
                            dependency.addInst(Inst(Inst::MOV, tReg, fReg, 0));
                        }
                    } else if (fSpill == -1 && tSpill != -1) {
                        std::cerr << "insert spill move between " << from.index
                                  << " and " << to->index << " from " << fReg << std::endl;
                        // spill register to n
                        // do no tdestroy register, just push it
                        ordered.push_back(Inst(Inst::SW, fReg, Inst::fp,
                                               -(int)(frameSize+tSpill*8)));
                    } else if (fSpill != -1 && tSpill == -1) {
                        std::cerr << "insert load move between " << from.index
                                  << " and " << to->index << " to " << tReg << std::endl;
                        // load register from c
                        loads.push_back(Inst(Inst::LW, tReg, Inst::fp,
                                             -(int)(frameSize+fSpill*8)));
                    } else {
                        // load and store, does not destroy register
                        // (but use reserved t0)
                        if (fSpill != tSpill) {
                            std::cerr << "insert load store between " << from.index
                                      << " and " << to->index << std::endl;
                            ordered.push_back(Inst(Inst::LW, Inst::Register::t0,
                                                   Inst::fp, -(int)(frameSize+fSpill*8)));
                            ordered.push_back(Inst(Inst::SW, Inst::Register::t0,
                                                   Inst::fp, -(int)(frameSize+tSpill*8)));
                        }
                    }
                }
            }
            // 1. emit all L/Ss and stores (does not destroy other register)
            // done above
            // 2. resolve moves (use Dependency)
            auto resolved = dependency.resolve();
            ordered.insert(ordered.end(), resolved.begin(), resolved.end());
            // 3. emit all loads (does not depend on any register)
            ordered.insert(ordered.end(), loads.begin(), loads.end());
            edgeInsts.push_back(ordered);
            std::cerr << "ordered inst: " << std::endl;
            for (auto inst: ordered) std::cerr << inst;
        }
		
        switch (from.last->op) {
        case Inst::J:
            // 1 successor after unconditional jump
            // insert insts before j inst (pointer by last)
            body.insert(from.last, edgeInsts[0].begin(), edgeInsts[0].end());
            break;
        case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE: case Inst::BGT:
        case Inst::BGE: case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ:
        case Inst::BLEZ: case Inst::BGTZ: case Inst::BGEZ: {
            // 2 successsor, one sequential, one after jump
            std::stringstream lEnd, lBr;
            auto lOld = from.last->prd;
            lEnd << "lEnd_" << "from" << from.index;
            lBr << "lBr_" << "from" << from.index;
            auto seq = isSeq ? edgeInsts[1] : edgeInsts[0];
            auto jmp = isSeq ? edgeInsts[0] : edgeInsts[1];

            if (jmp.empty()) {
                // if moves for jump target is empty, simply add all sequential moves
                // after bxx inst
                body.insert(from.end, seq.begin(), seq.end());
            } else {
                // resort to this ugly solution
                // replace bxx label to LBR
                from.last->prd = lBr.str();
                // insert seqential inst after bxx inst
                body.insert(from.end, seq.begin(), seq.end());
                // insert a jmp inst to end of inserted inst LEND
                body.insert(from.end, Inst(Inst::J, lEnd.str(), "", ""));
                // insert a new label LBR
                body.insert(from.end, Inst(Inst::LABEL, lBr.str(), "", ""));
                // insert jmp insts
                body.insert(from.end, jmp.begin(), jmp.end());
                // insert a jmp to bxx label LOLD
                body.insert(from.end, Inst(Inst::J, lOld, "", ""));
                // insert label LEND
                body.insert(from.end, Inst(Inst::LABEL, lEnd.str(), "", ""));
            }
        }
            break;
        default:
            // 1 sequential successor
            // insert insts before end/after last of block
            body.insert(from.end, edgeInsts[0].begin(), edgeInsts[0].end());
            break;
        }
    }
	
    std::cerr << "buildProc: Allocation done" << std::endl;

    // part 3. final clean up
    // stack structure:
    // | ra | fp | local vectors | spilled virtual register |
    std::vector<Inst> setupSnippet, exitSnippet;
    size_t totalFrameSize = frameSize + 8*spillSlot;

    setupSnippet.push_back(Inst(Inst::SW, Inst::ra, Inst::sp, -8));
    setupSnippet.push_back(Inst(Inst::SW, Inst::fp, Inst::sp, -16));
    setupSnippet.push_back(Inst(Inst::MOV, Inst::fp, Inst::sp, 0));
    setupSnippet.push_back(Inst(Inst::ADD, Inst::sp, Inst::sp, -totalFrameSize));
    
    // leave function: replace each occurrence of RET
    // clear procedure stack, restore ra fp    
    exitSnippet.push_back(Inst(Inst::MOV, Inst::sp, Inst::fp, 0));
    exitSnippet.push_back(Inst(Inst::LW, Inst::ra, Inst::sp, -8));
    exitSnippet.push_back(Inst(Inst::LW, Inst::fp, Inst::sp, -16));

    for (auto inst = body.begin(); inst != body.end(); ++inst) {
        std::cerr >> *inst;
        // assign registers and resolve global var ref
        bool regd = false, reg1 = false, reg2 = false;
        switch (inst->op) {
        case Inst::RET:
            body.insert(inst, exitSnippet.begin(), exitSnippet.end());
            break;    
        case Inst::ADD: case Inst::SLT: case Inst::SLL: case Inst::SRL: case Inst::SRA:
        case Inst::AND: case Inst::OR: case Inst:: SUB: case Inst::MUL: case Inst::DIV:
        case Inst::REM:
            regd = true;
        case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE: case Inst::BGT:
        case Inst::BGE:
            reg1 = true;
            reg2 = !inst->immOp;
            break;
        case Inst::SEQZ: case Inst::SNEZ: case Inst::SLTZ: case Inst::SGTZ:
        case Inst::SLEZ: case Inst::SGEZ: case Inst::NEG: case Inst::MOV:
            regd = true;
        case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ: case Inst::BLEZ:
        case Inst::BGTZ: case Inst::BGEZ:
            reg1 = true;
            break;
        case Inst::SW: case Inst::LW:
            reg1 = true;
        case Inst::LUI: case Inst::LA: case Inst::LI:
            regd = true;
            break;
        case Inst::LABEL: case Inst::J: case Inst::JAL:
            break;
        }
		
        if (reg1 && !inst->assigned1) {
            auto sym = localSymTab.find(inst->prs1);
            if (sym == localSymTab.end()) {
                // global
                inst->rs1 = Inst::t0;
                body.insert(inst, Inst(Inst::LUI, Inst::t2, Inst::zero,
                                       "%hi("+inst->prs1+")", true, false));
                body.insert(inst, Inst(Inst::LW, Inst::t0, Inst::t2,
                                       "%lo("+inst->prs1+")", true, false));
            } else {
                // local
                int index = vr2index[inst->prs1];
                int reg = intervals[index].childAtReverse(inst->ID)->assignedReg;
                inst->rs1 = (Inst::Register)reg;
            }
        }
        if (reg2 && !inst->assigned2) {
            auto sym = localSymTab.find(inst->prs2);
            if (sym == localSymTab.end()) {
                inst->rs2 = Inst::t1;
                body.insert(inst, Inst(Inst::LUI, Inst::t2, Inst::zero,
                                       "%hi("+inst->prs2+")", true, false));
                body.insert(inst, Inst(Inst::LW, Inst::t1, Inst::t2,
                                       "%lo("+inst->prs2+")", true, false));
            } else {
                // local
                int index = vr2index[inst->prs2];
                int reg = intervals[index].childAtReverse(inst->ID)->assignedReg;
                inst->rs2 = (Inst::Register)reg;
            }
        }
        if (regd && !inst->assignedd) {
            auto sym = localSymTab.find(inst->prd);
            if (sym == localSymTab.end()) {
                inst->rd = Inst::t0;
                auto tmp = inst;
                tmp++;
                body.insert(tmp, Inst(Inst::LUI, Inst::t2, Inst::zero,
                                      "%hi("+inst->prd+")", true, false));
                body.insert(tmp, Inst(Inst::SW, Inst::t0, Inst::t2,
                                      "%lo("+inst->prd+")", true, false));
            } else {
                int index = vr2index[inst->prd];
                int reg;
                if (inst->op == Inst::SW)
                    reg = intervals[index].childAtReverse(inst->ID)->assignedReg;
                else
                    reg = intervals[index].childAt(inst->ID)->assignedReg;
                inst->rd = (Inst::Register)reg;
            }
        }
		
        if (inst->op == Inst::MOV && inst->rd == inst->rs1) {
            // remove redundent movs
            auto tmp = inst;
            inst--;
            body.erase(tmp);
        }
		
    }
	
    body.insert(body.begin(), setupSnippet.begin(), setupSnippet.end());
	
    for (auto inst: body)
        os << inst;
    // print an extra empty line
    emitLine("", os);
}

void insertInst(std::list<Inst> &body, std::vector<Interval> &intervals,
                std::vector<BasicBlock*> &orderedBlocks, size_t frameSize)
{
    for (auto itv: intervals) {
        // if no split, onthing to do
        if (itv.splitPos.empty()) continue;

        // add parent to child group for my sanity when processing intervals
        itv.splitChild.insert(&itv);
        auto child = itv.splitChild.begin();
        auto next = child;
        next++;
        for (auto sPos: itv.splitPos) {
            while (sPos >= (*next)->lastRange().to)
                child++, next++;
            auto c = *child;
            auto n = *next;
            if (c->spillSlot == -1 && n->spillSlot == -1) {
                // simple move
                std::cerr << "insert move between at " << sPos << std::endl;
                insertMove(body, orderedBlocks, sPos,
                           (Inst::Register)c->assignedReg,
                           (Inst::Register)n->assignedReg);
            } else if (c->spillSlot == -1 && n->spillSlot != -1) {
                // spill register in c
                std::cerr << "insert spill move between at " << sPos << std::endl;
                insertMoveSpillOut(body, orderedBlocks, sPos,
                                   (Inst::Register)c->assignedReg,
                                   frameSize + n->spillSlot*8);
                c->spillTo = n->spillSlot;
                n->spillFrom = c->assignedReg;
            } else if (c->spillSlot != -1 && n->spillSlot == -1) {
                // load register in n
                std::cerr << "insert load move between at " << sPos << std::endl;
                insertMoveLoadBack(body, orderedBlocks, sPos,
                                   (Inst::Register)n->assignedReg,
                                   frameSize + c->spillSlot*8);
                c->loadTo = n->assignedReg;
                n->loadFrom = c->spillSlot;
            } else {
                // both sizes are spilled, this should NOT happen
                // (or at least I guess)
                std::cerr << "BuildProc: consecutive intervals are spilled" << std::endl;
                throw REGALLOCERROR;
            }
        }
        
        // remove parent from child, since parent is always the smallest
        itv.splitChild.erase(itv.splitChild.begin());
    }
}

void insertMove(std::list<Inst> &body, std::vector<BasicBlock*> &orderedBlocks,
                pos position, Inst::Register fromReg, Inst::Register toReg)
{
    if (fromReg == toReg) return;
    auto block = orderedBlocks.begin();
    while (block != orderedBlocks.end()) {
        if (position == (*block)->begin->ID) // on boundary
			
            return; // no need for a move operation
        else if (position >= (*block)->last->ID+2) block++;
        else break;
    }

    auto b = *block;
    auto inst = b->begin;
    while (inst->ID <= position) inst++;

    // fisrt inst after  position
    if (inst->ID == position) {
        std::cerr << "Insert Move: even postition not on boundary" << std::endl;
        return;
    }

    Inst newInst(Inst::MOV, Inst::reg2name.at(toReg), Inst::reg2name.at(fromReg), 0);
    newInst.rs1 = fromReg;
    newInst.assigned1 = true;
    newInst.rs2 = toReg;
    newInst.assigned2 = true;
    newInst.ID = position;
    body.insert(inst, newInst);
}

void insertMoveSpillOut(std::list<Inst> &body, std::vector<BasicBlock*> &orderedBlocks,
                        pos position, Inst::Register fromReg, size_t stackPos)
{
    auto block = orderedBlocks.begin();
    while (block != orderedBlocks.end())
        if (position == (*block)->begin->ID) // on boundary
            return; // no need for a move operation
        else if (position >= (*block)->last->ID+2) block++;
        else break;

    auto b = *block;
    auto inst = b->begin;
    while (inst->ID <= position) inst++;

    Inst newInst(Inst::SW, Inst::reg2name.at(fromReg), "fp", -stackPos);
    newInst.rd = fromReg;
    newInst.rs1 = Inst::fp;
    newInst.assignedd = true;
    newInst.assigned1 = true;
    newInst.ID = position;
    body.insert(inst, newInst);
}

void insertMoveLoadBack(std::list<Inst> &body, std::vector<BasicBlock*> &orderedBlocks,
                        pos position, Inst::Register toReg, size_t stackPos)
{
    auto block = orderedBlocks.begin();
    while (block != orderedBlocks.end())
        if (position == (*block)->begin->ID) // on boundary
            return; // no need for a move operation
        else if (position >= (*block)->last->ID+2) block++;
        else break;

    auto b = *block;
    auto inst = b->begin;
    while (inst->ID <= position) inst++;

    Inst newInst(Inst::LW, Inst::reg2name.at(toReg), "fp", -stackPos);
    newInst.rd = toReg;
    newInst.rs1 = Inst::fp;
    newInst.assignedd = true;
    newInst.assigned1 = true;
    newInst.ID = true;
    body.insert(inst, newInst);
}

bool allocateBlocked(Interval *current, std::list<Interval*> &active,
                     std::list<Interval*> &inactive,
                     std::multiset<Interval*, Interval::CompInterval> &unhandled,
                     std::vector<BasicBlock*> &orderedBlocks, int &spillSlot)
{
    int usePos[32] = {0}, blockPos[32] = {0};
    for (auto reg: availableReg) usePos[reg] = blockPos[reg] = INTMAX;
    auto currentUse = current->firstUse();

    pos fromPos = current->firstRange().from;

    for (auto itv: active) {
        if (itv->fixed) {
            setPos(itv, 0, blockPos);
            setPos(itv, 0, usePos);
        } else {
            pos use = INTMAX;
            for (auto up: itv->usePosition)
                if (up == currentUse) {
                    setPos(itv, 0, blockPos);
                    setPos(itv, 0, usePos);
                    // if some reg is used excatly at current's first use,
                    // this reg can't be shared even with spilling
                } else if (up < use && up >= fromPos) use = up;
            setPos(itv, use, usePos);
        }
    }

    for (auto itv: inactive) {
        pos intPos = current->intersect(itv);
        if (intPos == -1) continue;
        if (itv->fixed){
            setPos(itv, intPos, blockPos);
            setPos(itv, intPos, usePos);
        } else {
            pos use = INTMAX;
            for (auto up: itv->usePosition) if (up < use && up >= fromPos) use = up;
            // >= guarantees if some use = fromPos (which makes spilling itv
            // troublesome) will not happen for highest usePos[reg]
            setPos(itv, use, usePos);
        }
    }

    auto reg = availableReg[0];
    for (auto r: availableReg) if (usePos[r] > usePos[reg]) reg = r;

    if (usePos[reg] <= currentUse) {
        std::cerr << "Allocate: spill current" << std::endl;
        if (usePos[reg] == currentUse) {
            // find a reg that is use before current
            for (auto r:availableReg)
                if (usePos[r] < currentUse) {
                    reg = r;
                    break;
                }
        }
        // simply spill current just before use so next time this wont happen
        if (current->inheritedSlot != -1) current->spillSlot = current->inheritedSlot;
        else current->spillSlot = ++spillSlot;
        // the entire interval could be spilled so need to check that
        auto child = current->split(optimalSplit(current->firstRange().from,
                                                 currentUse, orderedBlocks));
        if (child) unhandled.insert(child);
        
        return false;
    } else {
        current->assignedReg = reg;
        // spill active/intersecting inactive intervals for reg
        if (blockPos[reg] < current->lastRange().to)
            unhandled.insert(
                current->split(optimalSplit(current->firstRange().from,
                                            blockPos[reg], orderedBlocks)));

        // since fixed intervals’ blockPos include entire interval,
        // current do not intersect them after split
        for (auto itv: active)
            splitAndSpill(current, itv, orderedBlocks, unhandled, spillSlot);
        for (auto itv: inactive)
            splitAndSpill(current, itv, orderedBlocks, unhandled, spillSlot);

        return true;
    }
}

void splitAndSpill(Interval *current, Interval *itv,
                   std::vector<BasicBlock*> &orderedBlocks,
                   std::multiset<Interval*, Interval::CompInterval> &unhandled,
                   int &spillSlot)
{
    if (current->intersect(itv) != -1) return;
    
    pos use = 0;
    pos fromPos = current->firstRange().from;
    for (auto u:itv->usePosition)
        if (u > use && u <= fromPos) use = u;
    // find the final use in itv before current
    // ues != fromPos is guarnteed
    auto child = itv->split(optimalSplit(use+1, fromPos, orderedBlocks));
    if (child->inheritedSlot != -1) child->spillSlot = child->inheritedSlot;
    else child->spillSlot = ++spillSlot;
    

    use = orderedBlocks.back()->last->ID+2;
    for (auto u: itv->usePosition)
        if (u > fromPos && u < use) use = u;
    // reg need to be restored before next use and as late as possible
    unhandled.insert(child->split(use-1));
    
}

// follow three rules, if split position is odd, a manual move is required
pos optimalSplit(pos fromPos, pos freePos, std::vector<BasicBlock*> &blocks)
{
    // find blocks that overlap with current
    auto bBlock = blocks.begin();
    while ((*bBlock)->last->ID < fromPos) bBlock++;
    auto eBlock = bBlock; // off-the-end
    while (eBlock != blocks.end() && (*eBlock)->begin->ID < freePos) eBlock++;

    // rule one: least loop depth
    // use last smallest one (so split child is smaller, potentially easier to allocate)
    auto opBlock = bBlock;
    for (auto b = bBlock; b != eBlock; ++b)
        if ((*b)->loopDepth <= (*opBlock)->loopDepth) opBlock = b;

    // rule two: split at block boundary
    auto bLast = (*opBlock)->last->ID;
    if (bLast < freePos) return bLast+2; // i.e. next block begin ID
    auto bBegin = (*opBlock)->begin->ID;
    if (bBegin > fromPos) return bBegin;

    // rule three: if not possible (the block include the entire current),
    // split at odd position
    // since [bBegin, bLast] is larger than [fromPos, freePos], split pos
    // won't be after last inst of the block
    if (freePos % 2 || freePos == fromPos) return freePos;
    return freePos-1;
    
}

bool tryAllocate(Interval *current, std::list<Interval*> &active,
                 std::list<Interval*> &inactive,
                 std::multiset<Interval*, Interval::CompInterval> &unhandled,
                 std::vector<BasicBlock*> &orderedBlocks)
{
    int freePos[32] = {0};
    for (auto i: availableReg) freePos[i] = INTMAX;
    for (auto interval: active) setPos(interval, 0, freePos);

    for (auto interval: inactive) {
        pos intPos = current->intersect(interval);
        if (intPos != -1) setPos(interval, intPos, freePos);
    }

    auto reg = availableReg[0];
    for (auto r: availableReg) if (freePos[r] > freePos[reg]) reg = r;
	
    if (freePos[reg] <= current->firstRange().from + 1) return false;
	
    std::cerr << "try allocate reg " << Inst::reg2name.at(reg) << std::endl;

    current->assignedReg = reg;
    if (freePos[reg] < current->lastRange().to)
        // split and insert child
        unhandled.insert(
            current->split(optimalSplit(current->firstRange().from,
                                        freePos[reg], orderedBlocks)));
    
    return true;
}

inline void setPos(Interval *interval, pos position, int posVec[])
{
    auto reg = interval->assignedReg;
    if (reg != -1 && position < posVec[reg]) posVec[reg] = position;
}

#define moveInterval(it, from, to) {            \
        to.push_back(*it);                      \
        auto tmp = it;                          \
        it++;                                   \
        from.erase(tmp);                        \
    }

void walkInterval(std::vector<Interval> &intervals,
                  std::vector<BasicBlock*> &orderedBlocks,
                  int &spillSlot)
{
    std::multiset<Interval*, Interval::CompInterval> unhandled;
    std::list<Interval*> active, inactive, handled;
	
    for (auto &interval: intervals) {
        if (interval.hasRange()) {
            if (!interval.fixed) unhandled.insert(&interval);
            else inactive.push_back(&interval);
        }
    }
	
    while (!unhandled.empty()) {
		
        auto current = *unhandled.begin();
        std::cerr << "processing interval: " << std::endl;
        std::cerr << *current;
        
        unhandled.erase(unhandled.begin());
        pos position = current->firstRange().from;
		
        std::cerr << "current position: " << position << std::endl;
		
        auto aInt = active.begin();
        while (aInt != active.end()) {
            if (!(*aInt)->hasRange()) {
                std::cerr << *aInt << std::endl;
                auto tmp = aInt;
                aInt++;
                active.erase(tmp);
            } else if ((*aInt)->lastRange().to < position) {
                moveInterval(aInt, active, handled);
            } else if (!(*aInt)->cover(position)) {
                moveInterval(aInt, active, inactive);
            } else aInt++;
        }

        auto iInt = inactive.begin();
        while (iInt != inactive.end()) {
            if (!(*iInt)->hasRange()) {
                std::cerr << *iInt << std::endl;
                auto tmp = iInt;
                iInt++;
                inactive.erase(tmp);
            } else if ((*iInt)->lastRange().to < position) {
                moveInterval(iInt, inactive, handled);
            } else if ((*iInt)->cover(position)) {
                moveInterval(iInt, inactive, active);
            } else iInt++;
        }

        if (tryAllocate(current, active, inactive, unhandled, orderedBlocks))
            active.push_back(current);
        else if (allocateBlocked(current, active, inactive, unhandled, orderedBlocks, spillSlot))
            active.push_back(current);
        std::cerr << std::endl;
    }
}

void buildInterval(std::vector<BasicBlock*> &orderedBlocks,
                   std::map<std::string, int> &vr2index,
                   std::vector<Interval> &intervals, size_t symNum)
{
    // NOTE: input/output same reg in same inst (same pos) count as twice
	
    for (auto block = orderedBlocks.rbegin();
         block != orderedBlocks.rend(); ++block) {
        auto b = *block;
        pos blockFrom = b->begin->ID;
        auto tmp = b->last;
        pos blockTo = tmp->ID+2;

        for (auto reg: savedReg)
            intervals[reg].addRange(blockFrom, blockTo);
		
        // skip the 32 physical regs
        for (unsigned i = 32; i < symNum+32; ++i)
            if (b->liveOut.test(i))
                intervals[i].addRange(blockFrom, blockTo);
		
		
        auto inst = b->end; // off-the-end
        while (true) {
            --inst;
			
            int inReg1 = 0, inReg2 = 0;
            bool in1 = false, in2 = false;
            int outReg = 0;
            bool out = false;
			
            switch (inst->op) {
            case Inst::ADD: case Inst::SLT: case Inst::SLL: case Inst::SRL:
            case Inst::SRA: case Inst::AND: case Inst::OR: case Inst::SUB:
            case Inst::MUL: case Inst::DIV: case Inst::REM:
                if (!inst->immOp && inst->prs2 != inst->prs1) {
                    inReg2 = getIndex(inst->prs2, vr2index);
                    in2 = true;
                }
                // use of pseudo reg name is okey since
                // physical regs are also assigned a vr name
                // intentional fall through
            case Inst::SEQZ: case Inst::SNEZ: case Inst::SLTZ: case Inst::SGTZ:
            case Inst::SLEZ: case Inst::SGEZ: case Inst::NEG: case Inst::MOV:
                inReg1 = getIndex(inst->prs1, vr2index);
                outReg = getIndex(inst->prd, vr2index);
                in1 = true;
                out = true;
                break;
					
            case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE:
            case Inst::BGT: case Inst::BGE: case Inst::BGTZ: case Inst::BGEZ:
            case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ: case Inst::BLEZ:
                // branch: BXXZ is build with immOp = true
                inReg1 = getIndex(inst->prs1, vr2index);
                in1 = true;
                if (!inst->immOp) {
                    inReg2 = getIndex(inst->prs2, vr2index);
                    in2 = true;
                }
                break;
					
            case Inst::LUI: case Inst::LA: case Inst::LI:
                // 0-ary operation, as for virtual register
                outReg = getIndex(inst->prd, vr2index);
                out = true;
                break;
					
            case Inst::LW:
                outReg = getIndex(inst->prd, vr2index);
                out = true;
                inReg2 = getIndex(inst->prs1, vr2index);
                in2 = true;
                break;
					
            case Inst::SW:
                inReg1 = getIndex(inst->prd, vr2index);
                in1 = true;
                inReg2 = getIndex(inst->prs1, vr2index);
                in2 = true;
                break;
					
            case Inst::J: case Inst::LABEL:
                break;
					
            case Inst::RET:
                // return value in a0
                inReg1 = Inst::a0;
                in1 = true;
                break;
            case Inst::JAL:
                // function call, all physical reg destroyed
                // function parameter used, number is (unituitively) in imm
                Interval &ia0 = intervals[vr2index["a0"]];
                // since insts are processed backwards
                // if function return value (a0) is used after func call
                if (ia0.firstRange().from < inst->ID+1)
                    ia0.firstRange().from = inst->ID+1;
					
                // block all caller saved registers
                for (auto reg: notSavedReg)
                    intervals[reg].addRange(inst->ID, inst->ID+1);
					
                // use parameters
                for (int i = 0; i < inst->imm; ++i) {
                    Interval &itv = intervals[vr2index[GENNAME("a", i)]];
                    itv.addRange(blockFrom, inst->ID);
                    itv.addUse(inst->ID);
                }
                break;
            }
			
            if (out && outReg != -1) {
                if (!intervals[outReg].hasRange() ||
                    intervals[outReg].firstRange().from > inst->ID) {
                    // the output is never used, directly assign t0 to it
                    inst->prd = "t0";
                    inst->rd = Inst::t0;
                    inst->assignedd = true;
                    // the removal of such instruction should have been done
                    // in dead code elimination
                } else {
                    // proceed normally
                    intervals[outReg].firstRange().from = inst->ID;
                    intervals[outReg].addUse(inst->ID);
                }
                /*
                  if (intervals[outReg].hasRange())
                  intervals[outReg].firstRange().from = inst->ID;
                  intervals[outReg].addUse(inst->ID);
                */
            }
            if (in1 && inReg1 != -1) {
                if (!intervals[inReg1].hasRange() ||
                    intervals[inReg1].firstRange().from >= inst->ID)
                    intervals[inReg1].addRange(blockFrom, inst->ID);
                intervals[inReg1].addUse(inst->ID);
            }
            if (in2 && inReg2 != -1) {
                if (!intervals[inReg2].hasRange() ||
                    intervals[inReg2].firstRange().from >= inst->ID)
                    intervals[inReg2].addRange(blockFrom, inst->ID);
                intervals[inReg2].addUse(inst->ID);
            }
			
            if (inst == b->begin) break;
        }
    }

}

int getIndex(std::string vr, std::map<std::string, int> &vr2index)
{
    // global syms is not in vr2index, so -1
    auto result = vr2index.find(vr);
    if (result == vr2index.end()) return -1;
    return result->second;
}

void genReg(std::string vr, std::map<std::string, int> &vr2index,
            std::bitset<MAXVR> &liveGen, std::bitset<MAXVR> &liveKill)
{
    // global syms index = -1
    int index = getIndex(vr, vr2index);
    if (index == -1) return;
    if (!liveKill.test(index))
        liveGen.set(index);
}

void killReg(std::string vr, std::map<std::string, int> &vr2index,
             std::bitset<MAXVR> &liveKill)
{
    int index = getIndex(vr, vr2index);
    if (index == -1) return;
    liveKill.set(index);
}

void blockLiveSet(BasicBlock* block, std::map<std::string, int> &vr2index)
{
    // physical registers are not processed
    auto &gen = block->liveGen;
    auto &kill = block->liveKill;
    
    gen.reset();
    kill.reset();
    block->liveIn.reset();
    block->liveOut.reset();

    for (auto inst = block->begin; inst != block->end; ++inst) {
        switch (inst->op) {
        case Inst::ADD: case Inst::SLT: case Inst::SLL: case Inst::SRL:
        case Inst::SRA: case Inst::AND: case Inst::OR: case Inst::SUB:
        case Inst::MUL: case Inst::DIV: case Inst::REM:
            if (!inst->immOp && !inst->assigned2)
                genReg(inst->prs2, vr2index, gen, kill);
            // intentional fall through
        case Inst::SEQZ: case Inst::SNEZ: case Inst::SLTZ: case Inst::SGTZ:
        case Inst::SLEZ: case Inst::SGEZ: case Inst::NEG: case Inst::MOV:
            if (!inst->assigned1)
                genReg(inst->prs1, vr2index, gen, kill);
            if (!inst->assignedd)
                killReg(inst->prd, vr2index, kill);
            break;
            
        case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE:
        case Inst::BGT: case Inst::BGE: case Inst::BGTZ: case Inst::BGEZ:
        case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ: case Inst::BLEZ:
            // branch: BXXZ is build with immOp = true
            if (!inst->assigned1)
                genReg(inst->prs1, vr2index, gen, kill);
            if (!inst->immOp && !inst->assigned2)
                genReg(inst->prs2, vr2index, gen, kill);
            break;
            
        case Inst::LUI: case Inst::LA: case Inst::LI:
            // 0-ary operation, as for virtual register
            if (!inst->assignedd)
                killReg(inst->prd, vr2index, kill);
            break;

        case Inst::LW:
            if (!inst->assignedd)
                killReg(inst->prd, vr2index, kill);
            if (!inst->assigned1)
                genReg(inst->prs1, vr2index, gen, kill);
            break;
				
        case Inst::SW:
            if (!inst->assignedd)
                genReg(inst->prd, vr2index, gen, kill);
            if (!inst->assigned1)
                genReg(inst->prs1, vr2index, gen, kill);
            break;
            
        case Inst::J: case Inst::JAL: case Inst::LABEL: case Inst::RET:
            // pseudo version of JXX is used so only ra
            // (which is not available for allocation) is modified
            // so liveness has nothing to do with these inst
            // JAL is handled later during building interval
            break;
        }
    }
}

struct compareBlockPointer {
    bool operator() (BasicBlock* const &b1, BasicBlock* const &b2) const {
        return b1->loopDepth > b2->loopDepth;
    }
};

void blockOrder(std::vector<BasicBlock> &blocks, BasicBlock *entry,
                std::vector<BasicBlock*> &orderedBlocks)
{
    // loop detection
    
    std::set<BasicBlock*> loopEnd;
    unsigned loopCount = 0;
    processBlock(&*blocks.begin(), entry, loopEnd, &loopCount);
    // all blocks should be reachable from first block

    std::vector<std::vector<bool>> reachMat =
        {loopCount, std::vector<bool>(blocks.size(), false)}; // loop x block
    
    for (auto end: loopEnd)
        backwardReach(end, reachMat, end->loopBelonged);

    for (size_t i = 0; i < blocks.size(); ++i) {
        int minLoopIndex = -1;
        for (size_t j = 0; j < loopCount; ++j) {
            if (reachMat[j][i]) {
                blocks[i].loopDepth++;
            	minLoopIndex = compareLoop(reachMat, minLoopIndex, j);
            }
        }
        blocks[i].loopIndex = minLoopIndex;
        blocks[i].incomingForward = blocks[i].inFor.size();
    }

    // compute block order

    // blocks are sorted in decreasing weight
    std::multiset<BasicBlock*, compareBlockPointer> workList;
    workList.insert(&*blocks.begin());
    while(!workList.empty()) {
        BasicBlock* block = *workList.begin();
        workList.erase(workList.begin());
        orderedBlocks.push_back(block);

        for (auto succ: block->successor)
            if (--(succ->incomingForward) == 0)
                workList.insert(succ);
    }  
}

int compareLoop(std::vector<std::vector<bool>> &reachMat, int l1, int l2)
{
    if (l1 == -1) return l2;
    if (l2 == -1) return l1;
    
    for (size_t i = 0; i < reachMat[l1].size(); ++i) {
        if (reachMat[l1][i] && !reachMat[l2][i]) return l2;
        if (!reachMat[l1][i] && reachMat[l2][i]) return l1;
    }
    return l1;
}

void backwardReach(BasicBlock *block, std::vector<std::vector<bool>> &bitMat,
                   int loopIndex)
{
//    if (visited[block->index]) return bitMat[loopIndex][block->index];
//    visited[block->index] = true;
//
//    if (block->loopIndex == loopIndex)
//        return bitMat[loopIndex][block->index] = true;
//
//    bool reachable = false;
//    for (auto &pred: block->predecessor)
//        reachable = backwardReach(pred, bitMat, visited, loopIndex) || reachable;
//    return bitMat[loopIndex][block->index] = reachable;
//
    auto &bitSet = bitMat[loopIndex];
    size_t blockNum = bitSet.size();
    std::queue<BasicBlock*> blockQueue;
    std::vector<bool> resolved(blockNum, false), inQueue(blockNum, false);
	
    blockQueue.push(block);
    inQueue[block->index] = true;
	
    while (!blockQueue.empty()) {
        auto b = blockQueue.front();
        blockQueue.pop();
        inQueue[b->index] = false;
		
        if (b->loopIndex == loopIndex) {
            resolved[b->index] = true;
            bitSet[b->index] = true;
            continue;
        }
		
        bool hasUnresolvedPred = false;
        for (auto pred: b->predecessor) {
            if (resolved[pred->index]) {
                if (bitSet[pred->index]) {
                    resolved[b->index] = true;
                    bitSet[b->index] = true;
                    bitSet[pred->index];
                }
            } else {
                hasUnresolvedPred = true;
                if (!inQueue[pred->index]) {
                    blockQueue.push(pred);
                    inQueue[pred->index] = true;
                }
            }
        }
        if (!hasUnresolvedPred) resolved[b->index] = true;
        else if (!resolved[b->index]) {
            blockQueue.push(b);
            inQueue[b->index] = true;
        }
    }
}

void processBlock(BasicBlock *block, BasicBlock *pred,
                  std::set<BasicBlock*> &loopEnd, unsigned *loopCount)
{
    if (block->visited && !block->active) return; // already processed
    if (block->visited) {
        // active, loop detected
        block->inBack.insert(pred);
        pred->outBack.insert(block);
        loopEnd.insert(pred);
        if (block->loopIndex == -1) // not yet assigned
            block->loopIndex = (*loopCount)++;
        pred->loopBelonged = block->loopIndex;
        return;
    }
    
    block->visited = true;
    block->active = true;
    block->inFor.insert(pred);
    pred->outFor.insert(block);
    for (auto &succ: block->successor)
        processBlock(succ, block, loopEnd, loopCount);   
    block->active = false;
}

void buildCFG(std::list<Inst> &body, BasicBlock &entry,
              BasicBlock &exit, std::vector<BasicBlock> &blocks)
{
    
    std::map<std::string, std::list<Inst>::iterator> potentialTarget;
    std::set<std::string> actualTarget;
    using pair_t = std::pair<std::string, std::list<Inst>::iterator>;

    // identify block head
    body.begin()->isBlockHead = true;
    for (auto inst = body.begin(); inst != body.end(); ++inst) {
        if (Inst::branchInst.find(inst->op) != Inst::branchInst.end()) {
            auto next = inst;
            (++next)->isBlockHead = true;
            actualTarget.insert(inst->prd);
        } /*else if (inst->op == Inst::JAL) {
            auto next = inst;
            next++;
            if (next != body.end()) {
            if (inst->op == Inst::JAL && next->op == Inst::MOV &&
            next->assigned1 && next->rs1 == Inst::a0 &&
            next->immOp && next->imm == 0) {
            // if next inst is mov xx, $a0
            // this inst is generated with jal for reg alloc
            // and should be considered second half of jal
            next++;
            if (next != body.end())
            next->isBlockHead = true;
            } else
            next->isBlockHead = true;
            }
            } */else if (inst->op == Inst::J) {
            auto next = inst;
            next++;
            if (next != body.end()) next->isBlockHead = true;
            actualTarget.insert(inst->prd);
        } else if (inst->op == Inst::RET){
            auto next = inst;
            next++;
            if (next != body.end()) next->isBlockHead = true;
        } else if (inst->op == Inst::LABEL) {
            potentialTarget.insert(pair_t(inst->prd, inst));
        }       
    }
    for (auto target: actualTarget)
        potentialTarget[target]->isBlockHead = true;

    std::cerr << "buildCFG: block head found" << std::endl;
    
    // make blocks
    int blockIndex = 0;
    for (auto inst = body.begin(); inst != body.end(); ++inst) {
        if (inst->isBlockHead) {
            if (!blocks.empty()) {
                blocks.back().last = --inst;
                blocks.back().end = ++inst;
            }
            
            BasicBlock newBlock;
            newBlock.begin = inst;
            newBlock.index = blockIndex++;
            blocks.push_back(newBlock);
        }
    }
    blocks.back().last = --body.end();
    blocks.back().end = body.end();

    std::cerr << "buildCFG: block made" << std::endl;
    
    for (auto block = blocks.begin(); block != blocks.end(); ++block) {
        auto &last = block->last;
        if (last->op == Inst::J) {
            // unconditional jump
            for (auto &nb: blocks) {
                if (nb.begin == potentialTarget[last->prd]) {
                    // if a block’s first inst is jump target
                    block->successor.insert(&nb);
                    nb.predecessor.insert(&*block);
                    break;
                }
            }
            continue;
        }
        if (last->op == Inst::RET) {
            // return
            block->successor.insert(&exit);
            exit.predecessor.insert(&*block);
            continue;
        }
        if (Inst::branchInst.find(last->op) != Inst::branchInst.end()) {
            // conditonal jump
            for (auto &nb: blocks) {
                if (nb.begin == potentialTarget[last->prd]) {
                    // if a block’s first inst is jump target
                    block->successor.insert(&nb);
                    nb.predecessor.insert(&*block);
                    break;
                }
            }
        }
        // sequential
        auto nb = block;
        nb++;
        if (nb == blocks.end()) {
            std::cerr << "control reach end of none void function" << std::endl;
            throw SYNTAXERROR;;
        }
        block->successor.insert(&*nb);
        nb->predecessor.insert(&*block);
    }
    entry.successor.insert(&blocks.front());
    blocks.front().predecessor.insert(&entry);

    std::cerr << "buildCFG: graph built" << std::endl;
}


// left undone: global variable (not vector) reference. 
// return snippet, header snippet.
// WARNING: passing vector name to function is no longer supported
void instSelect(std::list<Quad>::iterator &quad, std::list<Inst> &body,
                std::vector<LocalVar> &stackFrame,
                std::set<std::string> &localSymTab, size_t &frameSize)
{
    unsigned tmpCounter = 0;
    const std::string TMPPREFIX = "r";
    unsigned paramCounter = 0;

    while(quad->op != Quad::ENDF) {	

        switch(quad->op) {
        case Quad::SL:
            // WAARNING: shift operation is generated in strength reduction
            // and has type of ’Register shift Constant’
            body.push_back(Inst(Inst::SLL, quad->result, quad->arg1, quad->arg2.constant));
            break;
        case Quad::SR:
            body.push_back(Inst(Inst::SRA, quad->result, quad->arg1, quad->arg2.constant));
            break;
        case Quad::PLUS: case Quad::MINUS: case Quad::MULT: case Quad::DIV: case Quad::REM: {
            // normal binary operation
            std::string name1, name2;
            if (quad->arg1.type == Quad::Address::NAME)
                name1 = quad->arg1;
            else {
                auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(tmp);
                body.push_back(Inst(Inst::LI, tmp, "", quad->arg1.constant));
                name1 = tmp;
            }

            // two operation that have imm version
            if (quad->op == Quad::PLUS) {
                if (quad->arg2.type == Quad::Address::NAME)
                    body.push_back(Inst(Inst::ADD, quad->result, name1, quad->arg2));
                else
                    body.push_back(Inst(Inst::ADD, quad->result, name1, quad->arg2.constant));
                break;
            } else if (quad->op == Quad::MINUS) {
                if (quad->arg2.type == Quad::Address::NAME)
                    body.push_back(Inst(Inst::SUB, quad->result, name1, quad->arg2));
                else
                    body.push_back(Inst(Inst::ADD, quad->result, name1, -quad->arg2.constant));
                break;
            }

            // operations that does not have imm version
            if (quad->arg2.type == Quad::Address::NAME)
                name2 = quad->arg2;
            else {
                auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(tmp);
                body.push_back(Inst(Inst::LI, tmp, "", quad->arg2.constant));
                name2 = tmp;
            }

            auto op = Inst::MUL;
            if (quad->op == Quad::DIV) op = Inst::DIV;
            else if (quad->op == Quad::REM) op = Inst::REM;

            body.push_back(Inst(op, quad->result, name1, name2));
        }
            break;
            
        case Quad::NOT:
            if (quad->arg1.type == Quad::Address::NAME)
                body.push_back(Inst(Inst::SEQZ, quad->result, quad->arg1, ""));
            else
                body.push_back(Inst(Inst::LI, quad->result, "", quad->arg1.constant == 0));
            break;
            
        case Quad::NEG:
            if (quad->arg1.type == Quad::Address::NAME)
                body.push_back(Inst(Inst::NEG, quad->result, quad->arg1, ""));
            else {
                // constant folding should have ensured such case won’t happen
                auto name = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(name);
                body.push_back(Inst(Inst::LI, name, "", quad->arg1.constant));
                body.push_back(Inst(Inst::NEG, quad->result, name, ""));
            }
            break;

        case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT: {
            // WARNING: assuming constant folding is done and at most one is constant
            if (quad->arg1.type == Quad::Address::CONST) {
                std::swap(quad->arg1, quad->arg2);
                if (quad->op == Quad::LT) quad->op = Quad::GT;
                else if (quad->op == Quad::GT) quad->op = Quad::LT;
            }

            std::map<Quad::Operation, Inst::Op> op2op = {
                {Quad::EQ, Inst::SEQZ}, {Quad::IEQ, Inst::SNEZ}, {Quad::GT, Inst::SGTZ}
            };
            
            if (quad->arg2.type == Quad::Address::CONST) {
                if (quad->op == Quad::LT)
                    body.push_back(Inst(Inst::SLT, quad->result, quad->arg1, quad->arg2.constant));
                else {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::ADD, tmp, quad->arg1, -quad->arg2.constant));
                    body.push_back(Inst(op2op[quad->op], quad->result, tmp, ""));
                }
            } else {
                if (quad->op == Quad::LT)
                    body.push_back(Inst(Inst::SLT, quad->result, quad->arg1, quad->arg2));
                else {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::SUB, tmp, quad->arg1, quad->arg2));
                    body.push_back(Inst(op2op[quad->op], quad->result, tmp, ""));
                }
            }
        }
            break;

        case Quad::GOTO:
            body.push_back(Inst(Inst::J, quad->result, "", ""));
            body.back().jumpTarget = true;
            break;
            
        case Quad::GOTOLT: case Quad::GOTOGT: case Quad::GOTOLE: case Quad::GOTOGE:
        case Quad::GOTOEQ: case Quad::GOTONE: {
            // WARNING: assuming constant folding
            std::map<Quad::Operation, Inst::Op> op2opz = {
                {Quad::GOTOLT, Inst::BLTZ}, {Quad::GOTOGT, Inst::BGTZ},
                {Quad::GOTOLE, Inst::BLEZ}, {Quad::GOTOGE, Inst::BGEZ},
                {Quad::GOTOEQ, Inst::BEQZ}, {Quad::GOTONE, Inst::BNEZ} 
            };

            std::map<Quad::Operation, Inst::Op> op2op = {
                {Quad::GOTOLT, Inst::BLT}, {Quad::GOTOGT, Inst::BGT},
                {Quad::GOTOLE, Inst::BLE}, {Quad::GOTOGE, Inst::BGE},
                {Quad::GOTOEQ, Inst::BEQ}, {Quad::GOTONE, Inst::BNE} 
            };

            if (quad->arg1.type == Quad::Address::CONST) {
                std::swap(quad->arg1, quad->arg2);
                switch(quad->op) {
                case Quad::GOTOLT: quad->op = Quad::GOTOGT; break;
                case Quad::GOTOGT: quad->op = Quad::GOTOLT; break;
                case Quad::GOTOLE: quad->op = Quad::GOTOGE; break;
                case Quad::GOTOGE: quad->op = Quad::GOTOLE; break;
                default: throw QUADBUG;
                }
            }

            if (quad->arg2.type == Quad::Address::CONST) {
                auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(tmp);
                body.push_back(Inst(Inst::ADD, tmp, quad->arg1, -quad->arg2.constant));
                body.push_back(Inst(op2opz[quad->op], quad->result, tmp, 0));
                body.back().jumpTarget = true;
            } else {
                body.push_back(Inst(op2op[quad->op], quad->result, quad->arg1, quad->arg2));
                body.back().jumpTarget = true;
            }
        }
            break;
            

        case Quad::AND: case Quad::OR: {
            // if either side of AND/OR is constant, constant folding should have folded this quad
            // WARNING: the follwing assumes both size of operation is not constant
            auto tmp1 = GENNAME(TMPPREFIX, tmpCounter++);
            localSymTab.insert(tmp1);
            body.push_back(Inst(Inst::SNEZ, tmp1, quad->arg1, ""));
            auto tmp2 = GENNAME(TMPPREFIX, tmpCounter++);
            localSymTab.insert(tmp2);
            body.push_back(Inst(Inst::SNEZ, tmp2, quad->arg2, ""));
            if (quad->op == Quad::AND)
                body.push_back(Inst(Inst::AND, quad->result, tmp1, tmp2));
            else
                body.push_back(Inst(Inst::OR, quad->result, tmp2, tmp2));
        }
            break;

        case Quad::ASSIGN:
            if (quad->arg1.type == Quad::Address::CONST)
                body.push_back(Inst(Inst::LI, quad->result, "", quad->arg1.constant));
            else
                body.push_back(Inst(Inst::MOV, quad->result, quad->arg1, 0));
            break;

        case Quad::SETVEC:

            // R[A] = B
            
            if (localSymTab.find(quad->result) == localSymTab.end()) {
                // if vector is global
                // load address in baseReg
                auto tmp1 = GENNAME(TMPPREFIX, tmpCounter++);
                auto baseReg = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(tmp1);
                localSymTab.insert(baseReg);
                body.push_back(Inst(Inst::LUI, tmp1, "", "%hi("+quad->result+")", true, false));
                body.push_back(Inst(Inst::ADD, baseReg, tmp1, "%lo("+quad->result+")", true, false));

                pseudoReg valueReg;

                if (quad->arg2.type == Quad::Address::NAME)
                    valueReg = quad->arg2;
                else {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::LI, tmp, "", quad->arg2.constant));
                    valueReg = tmp;
                }
                
                if (quad->arg1.type == Quad::Address::NAME) {
                    auto tmp3 = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp3);
                    body.push_back(Inst(Inst::ADD, tmp3, baseReg, quad->arg1));
                    body.push_back(Inst(Inst::SW, valueReg, tmp3, 0));
                } else {
                    body.push_back(Inst(Inst::SW, valueReg, baseReg, quad->arg1.constant));
                }
            } else {
				
                pseudoReg baseReg;
                if (quad->result[0] == 'p') {
                    // is vector is a function parameter
                    // base is stored in variable
                    baseReg = quad->result;
                } else  {
                    // if vector is defined within in function, vector stored in stack
                    baseReg = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(baseReg);
                    size_t base = 0;
                    for (auto &vec: stackFrame)
                        if (vec.name == quad->result) {
                            base = vec.offset;
                            break;
                        }
                    // use frame pointer
                    auto addiInst = Inst(Inst::ADD, baseReg, "fp", -(signed)base);
                    addiInst.rs1 = Inst::fp;
                    addiInst.assigned1 = true;
                    body.push_back(addiInst);
                }
				
                pseudoReg valueReg;

                if (quad->arg2.type == Quad::Address::NAME)
                    valueReg = quad->arg2;
                else {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::LI, tmp, "", quad->arg2.constant));
                    valueReg = tmp;
                }
                
                if (quad->arg1.type == Quad::Address::NAME) {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::ADD, tmp, baseReg, quad->arg1));
                    body.push_back(Inst(Inst::SW, valueReg, tmp, 0));
                } else
                    body.push_back(Inst(Inst::SW, valueReg, baseReg, quad->arg2.constant));
                
            }
            break;
            
        case Quad::GETVEC:

            // R = A[B]
            
            if (localSymTab.find(quad->arg1) == localSymTab.end()) {
                // if vector is global              
                // load address in baseReg
                auto tmp1 = GENNAME(TMPPREFIX, tmpCounter++);
                auto baseReg = GENNAME(TMPPREFIX, tmpCounter++);
                localSymTab.insert(tmp1);
                localSymTab.insert(baseReg);
                body.push_back(Inst(Inst::LUI, tmp1, "", "%hi("+quad->arg1.name+")", true, false));
                body.push_back(Inst(Inst::ADD, baseReg, tmp1, "%lo("+quad->arg1.name+")", true, false));

                
                if (quad->arg2.type == Quad::Address::NAME) {
                    auto tmp3 = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp3);
                    body.push_back(Inst(Inst::ADD, tmp3, baseReg, quad->arg2));
                    body.push_back(Inst(Inst::LW, quad->result, tmp3, 0));
                } else {
                    body.push_back(Inst(Inst::LW, quad->result, baseReg, quad->arg2.constant));
                }
            } else {
                pseudoReg baseReg;
                if (quad->arg1.name[0] == 'p') {
                    baseReg = quad->result;
                } else {
                    // if vector is defined within in function, vector stored in stack
                    baseReg = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(baseReg);
                    size_t base = 0;
                    for (auto &vec: stackFrame)
                        if (vec.name == quad->arg1.name) {
                            base = vec.offset;
                            break;
                        }
					
                    // use frame pointer
                    auto addiInst = Inst(Inst::ADD, baseReg, "fp", -(signed)base);
                    addiInst.rs1 = Inst::fp;
                    addiInst.assigned1 = true;
                    body.push_back(addiInst);
                }
				
                if (quad->arg2.type == Quad::Address::NAME) {
                    auto tmp = GENNAME(TMPPREFIX, tmpCounter++);
                    localSymTab.insert(tmp);
                    body.push_back(Inst(Inst::ADD, tmp, baseReg, quad->arg2));
                    body.push_back(Inst(Inst::LW, quad->result, tmp, 0));
                } else
                    body.push_back(Inst(Inst::LW, quad->result, baseReg, quad->arg2.constant));
                
            }
            break;
            
        case Quad::PARAM:
            if (quad->arg1.type == Quad::Address::CONST) {
                Inst newInst(Inst::LI, GENNAME("a", paramCounter), "", quad->arg1.constant);
                newInst.rd = Inst::Register(Inst::a0 + paramCounter++);
                newInst.assignedd = true;
                body.push_back(newInst);
            } else {
                Inst newInst(Inst::MOV, GENNAME("a", paramCounter), quad->arg1, 0);
                newInst.rd = Inst::Register(Inst::a0 + paramCounter++);
                newInst.assignedd = true;
                body.push_back(newInst);
            }
            break;
            
        case Quad::CALL: case Quad::STMTCALL:
            paramCounter = 0;
            body.push_back(Inst(Inst::JAL, std::string(quad->arg1), "", quad->arg2.constant));
            body.back().jumpTarget = true;
            if (quad->op == Quad::CALL) {
                Inst newInst(Inst::MOV, quad->result, GENNAME("a", 0), 0);
                newInst.rs1 = Inst::a0;
                newInst.assigned1 = true;
                body.push_back(newInst);
            }
            break;
            
        case Quad::RETURN:
            if (quad->arg1.type == Quad::Address::CONST) {
                Inst newInst(Inst::LI, GENNAME("a", 0), "", quad->arg1.constant);
                newInst.rd = Inst::a0;
                newInst.assignedd = true;
                body.push_back(newInst);
                body.push_back(Inst(Inst::RET, "", "", ""));
            } else {
                Inst newInst(Inst::MOV, GENNAME("a", 0), quad->arg1, 0);
                newInst.rd = Inst::a0;
                newInst.assignedd = true;
                body.push_back(newInst);
                body.push_back(Inst(Inst::RET, "", "", ""));
            }
            break;

        case Quad::DEFVAR:
            localSymTab.insert(quad->arg1);
            break;
            
        case Quad::DEFVEC:
            frameSize += quad->arg2.constant;
            stackFrame.push_back(LocalVar(quad->arg1, frameSize, quad->arg2.constant));
            localSymTab.insert(quad->arg1);
            break;

        case Quad::LABEL:
            body.push_back(Inst(Inst::LABEL, quad->result, "", ""));
            break;

        case Quad::NOP:
            break;
            
        case Quad::DEFF: case Quad::ENDF:
            // WARNING: do NOT use default here, so that linter can discover missed cases
            throw QUADBUG;
        }
        
        quad++;
    }
}


void emitLine(std::string str, std::ostream& os) { os << "\t" << str << std::endl; }
void emitLabel(std::string str, std::ostream& os) { os << str << ":" << std::endl; }

const std::map<Inst::Register, std::string> Inst::reg2name = {
    {zero, "zero"}, {ra, "ra"}, {sp, "sp"}, {gp, "gp"}, {tp, "tp"},
    {t0, "t0"}, {t1, "t1"}, {t2, "t2"},
    {fp, "s0"}, {s1, "s1"},
    {a0, "a0"}, {a1, "a1"}, {a2, "a2"}, {a3, "a3"}, {a4, "a4"}, {a5, "a5"},
    {a6, "a6"}, {a7, "a7"},
    {s2, "s2"}, {s3, "s3"}, {s4, "s4"}, {s5, "s5"}, {s6, "s6"}, {s7, "s7"},
    {s8, "s8"}, {s9, "s9"}, {s10, "s10"}, {s11, "s11"},
    {t3, "t3"}, {t4, "t4"}, {t5, "t5"}, {t6, "t6"}
};

const std::set<Inst::Op> Inst::branchInst = {
    BEQ, BNE, BLT, BLE, BGT, BGE, BEQZ, BNEZ, BLTZ, BLEZ, BGTZ, BGEZ,  
};

const std::set<Inst::Op> Inst::jumpInst = {
    J, // JR is not in here because JR does not jump to label
};

const std::map<Inst::Op, std::string> Inst::op2name = {
    {ADD, "add"}, {SLT, "slt"}, {SLL, "sll"}, {SRL, "srl"}, {SRA, "sra"}, {AND, "and"},
    {OR, "or"}, {SUB, "sub"}, {MUL, "mul"}, {DIV, "div"}, {REM, "rem"},
    {SEQZ, "seqz"}, {SNEZ, "snez"}, {SLTZ, "sltz"}, {SGTZ, "sgtz"}, {SLEZ, "slez"},
    {SGEZ, "sgez"}, {NEG, "neg"}, {MOV, "mv"},
    {BEQ, "beq"}, {BNE, "bne"}, {BLT, "blt"}, {BLE, "ble"}, {BGT, "bgt"}, {BGE, "bge"},
    {BEQZ, "beqz"}, {BNEZ, "bnez"}, {BLTZ, "bltz"}, {BLEZ, "blez"}, {BGTZ, "bgtz"}, {BGEZ, "bgez"},
    {SW, "sw"}, {LUI, "lui"}, {LW, "lw"}, {LA, "la"}, {LI, "li"}, {J, "J"}, {JAL, "jal"}, {RET, "ret"}
    
};

std::ostream &operator>>(std::ostream &os, Inst const &inst)
{
    os << "ID\t" << inst.ID << ":\t";
    if (inst.op == Inst::LABEL)
        return os << inst.prd << ":" << std::endl;
	
    os << "\t" << Inst::op2name.at(inst.op) << " ";
    switch (inst.op) {
    case Inst::SEQZ: case Inst::SNEZ: case Inst::SLTZ: case Inst::SGTZ:
    case Inst::SLEZ: case Inst::SGEZ: case Inst::NEG: case Inst::MOV:
        os << inst.prd << ", " << inst.prs1;
        break;
    case Inst::ADD: case Inst::SLT: case Inst::SLL: case Inst::SRL: case Inst::SRA:
    case Inst::AND: case Inst::OR: case Inst:: SUB: case Inst::MUL: case Inst::DIV:
    case Inst::REM:
        os << inst.prd << ", " << inst.prs1;
        if (inst.immOp && !inst.symImm) os << ", " << inst.imm;
        else os << ", " << inst.prs2;
        break;
    case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE: case Inst::BGT:
    case Inst::BGE: case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ: case Inst::BLEZ:
    case Inst::BGTZ: case Inst::BGEZ:
        os << inst.prs1 << ", ";
        if (!inst.immOp) os << inst.prs2 << ", ";
        os << inst.prd;
        break;
    case Inst::SW: case Inst::LW:
        os << inst.prd << ", ";
        if (inst.symImm) os << inst.prs2;
        else os << inst.imm;
        os << "(" << inst.prs1 << ")";
        break;
    case Inst::LUI: case Inst::LA: case Inst::LI:
        os << inst.prd << ", ";
        if (inst.symImm) os << inst.prs2;
        else os << inst.imm;
        break;
    case Inst::J: case Inst::JAL:
        os << inst.prd;
        break;
    case Inst::RET: case Inst::LABEL:
        break;
    }
    return os << std::endl;
}


std::ostream &operator<<(std::ostream &os, Inst const &inst)
{
    if (inst.op == Inst::LABEL)
        return os << inst.prd << ":" << std::endl;
    
    os << "\t" << Inst::op2name.at(inst.op) << " ";
    switch (inst.op) {
    case Inst::SEQZ: case Inst::SNEZ: case Inst::SLTZ: case Inst::SGTZ:
    case Inst::SLEZ: case Inst::SGEZ: case Inst::NEG: case Inst::MOV:
        os << Inst::reg2name.at(inst.rd) << ", " << Inst::reg2name.at(inst.rs1);
        break;
    case Inst::ADD: case Inst::SLT: case Inst::SLL: case Inst::SRL: case Inst::SRA:
    case Inst::AND: case Inst::OR: case Inst:: SUB: case Inst::MUL: case Inst::DIV:
    case Inst::REM:
        os << Inst::reg2name.at(inst.rd) << ", " << Inst::reg2name.at(inst.rs1) << ", ";
        if (inst.immOp)
            if (!inst.symImm) os << inst.imm;
            else os << inst.prs2;
        else os << Inst::reg2name.at(inst.rs2);
        break;
    case Inst::BEQ: case Inst::BNE: case Inst::BLT: case Inst::BLE: case Inst::BGT:
    case Inst::BGE: case Inst::BEQZ: case Inst::BNEZ: case Inst::BLTZ: case Inst::BLEZ:
    case Inst::BGTZ: case Inst::BGEZ:
        os << Inst::reg2name.at(inst.rs1) << ", ";
        if (!inst.immOp) os << Inst::reg2name.at(inst.rs2) << ", ";
        os << inst.prd;
        break;
    case Inst::SW: case Inst::LW:
        os << Inst::reg2name.at(inst.rd) << ", ";
        if (inst.symImm) os << inst.prs2;
        else os << inst.imm;
        os << "(" << Inst::reg2name.at(inst.rs1) << ")";
        break;
    case Inst::LUI: case Inst::LA: case Inst::LI:
        os << Inst::reg2name.at(inst.rd) << ", ";
        if (inst.symImm) os << inst.prs2;
        else os << inst.imm;
        break;
    case Inst::J:
        os << inst.prd;
        break;
    case Inst::JAL: {
        os << inst.prd.substr(2); // trim the f_ prefix
        break;
    }
		
    case Inst::RET: case Inst::LABEL:
        break;
    }
    return os << std::endl;
}
