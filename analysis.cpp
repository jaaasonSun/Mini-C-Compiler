#include <vector>
#include <list>
#include <set>
#include <map>
#include <queue>
#include <fstream>

#include "analysis.hpp"
#include "dataFlow.hpp"
#include "DAG.hpp"
#include "eeyore.hpp"
#include "utility.hpp"

using QuadList = std::list<Quad>;
using qit = QuadList::iterator;

size_t Block::indexCounter = 0;

void analysis(QuadList *quads) {
    qit it = quads->begin();
    std::set<std::string> globals;
    // globals only contain global var (not vector)
    // globals is used in live analysis where vector
    
    it = quads->begin();
    while (it != quads->end()) {
        if (it->op == Quad::DEFF)
			funcAnalysis(quads, it, globals, it->arg2.constant);
        else if (it->op == Quad::DEFVAR)
			globals.insert(it->arg1);
        ++it;
    }
}

void funcAnalysis(QuadList *quads, qit &fit,
				  std::set<std::string> const &globals,
				  size_t argCount)
{
    Block entry, exit;
    entry.index = ENTRY_INDEX;
    exit.index = EXIT_INDEX;
    qit funcBegin = ++fit;

    while (fit->op != Quad::ENDF) ++fit;
    qit funcEnd = fit;

    peepHole(quads, funcBegin, funcEnd);

    // build CFG
    std::vector<Block*> blocks = buildCFG(&entry, &exit, funcBegin, funcEnd);
    quads->erase(funcBegin, funcEnd); // fit is still valid iterator after this

    blocks.push_back(&entry);
    blocks.push_back(&exit);

    // build symtable (with globals and parameter)
    std::map<std::string, size_t> sym2index;
    std::map<size_t, std::string> index2sym;
    
    int symIndex = 0;
    for (auto b: blocks) {
        auto it = b->begin();
		while (it != b->end()) {
            if (it->op == Quad::DEFVAR) {
                sym2index[it->arg1.name] = symIndex;
                index2sym[symIndex++] = it->arg1.name;
            }
			it++;
		}
    }
    for (auto name: globals) {
        sym2index[name] = symIndex;
        index2sym[symIndex++] = name;
    }
	for (int i = 0; i < argCount; ++i) {
		auto name = GENNAME("p", i);
		sym2index[name] = symIndex;
		index2sym[symIndex++] = name;
	}
    
    auto opt0 = std::ofstream("opt0.ee");
    for (auto b: blocks) {
        opt0 << "block " << b->index << std::endl;
        auto it = b->begin();
        while (it != b->end()) opt0 << *it++ << std::endl;
        opt0 << std::endl;
    }

    // constant folding (global)
    ConstantFolding cf(sym2index, index2sym, blocks, globals);
    auto constInfo = cf.analyse();
    
    auto constFile = std::ofstream("const.txt");
    for (auto b: blocks) {
        if (b->index <= 1) continue;
        constFile << "block " << b->index << " pred ";
        for (auto p: b->pred) constFile << p.block->index << " ";
        constFile << "succ ";
        for (auto p: b->succ) constFile << p.block->index << " ";
        constFile << "\nin: ";
        auto &constIn = constInfo[b->index].first;
        auto &constOut = constInfo[b->index].second;
        for (auto sym: sym2index)
            switch (constIn[sym2index[sym.first]].state) {
                case Symbol::NAC:
                    constFile << sym.first << ":NAC ";
                    break;
                case Symbol::CONSTANT:
                    constFile << sym.first << ":CONST "
                    << constIn[sym2index[sym.first]].value << " ";
                    break;
                case Symbol::UNDEFINED:
                    break;
            }
        constFile << "\nout: ";
        for (auto sym: sym2index)
            switch (constOut[sym2index[sym.first]].state) {
                case Symbol::NAC:
                    constFile << sym.first << ":NAC ";
                    break;
                case Symbol::CONSTANT:
                    constFile << sym.first << ":CONST "
                    << constOut[sym2index[sym.first]].value << " ";
                    break;
                case Symbol::UNDEFINED:
                    break;
            }
        constFile << std::endl;
    }
    
    cf.apply();

    // block optimization
    for (auto b: blocks) blockOptimization(b);
	
    // at this point, rebuilding CFG could potentially find more dead blocks
    // since constant folding could eliminate some branch
    // build quaf list from CFG
    QuadList tmpQuadList;
    for (auto b: blocks)
        // blocks are in index order
        tmpQuadList.insert(tmpQuadList.end(), b->begin(), b->end());
    
    entry = Block();
    exit = Block();
    entry.index = ENTRY_INDEX;
    exit.index = EXIT_INDEX;

    // NOTE: entry and exit is NOT in block list
    blocks = buildCFG(&entry, &exit, tmpQuadList.begin(), tmpQuadList.end());
    for (auto b: blocks) {
        // if block has no predecessor it cannot be reached
        // remove block recursively
        // by ’remove’ I mean mark the block as removed
        // instead of actually remove the block, for convenience
        if (!b->isRemoved() && b->isUnreachable()) {
            std::queue<Block *> removeQueue;
            removeQueue.push(b);
            while (removeQueue.size()) {
                auto r = removeQueue.front();
                removeQueue.pop();
                r->remove();          
                // check successors
                // DO NOT add to queue if already removed
                for (auto s: r->succ)
                    if (!s.block->isRemoved() && s.block->isUnreachable())
                        removeQueue.push(s.block);
            }
        }
    }

    // rebuild CFG (yet again!) to actually remove blocks
    // the ’correct’ approach would be to manually remove succ/pred references
    // but it proved to be rather tricky so this is the way it is...
    tmpQuadList.clear();
    for (auto b: blocks) {
        if (!b->isRemoved())
            tmpQuadList.insert(tmpQuadList.end(), b->begin(), b->end());
        delete b;
    }

    entry = Block();
    exit = Block();
    entry.index = ENTRY_INDEX;
    exit.index = EXIT_INDEX;

    blocks = buildCFG(&entry, &exit, tmpQuadList.begin(), tmpQuadList.end());
	
    // live analysis (global)
    LiveAnalysis la(sym2index, index2sym, blocks, globals);
    auto inout = la.analyse();
    // inout is a map from index to block info pair,
    // or in this case, liveIn/liveOut pair
    // live info is used in dead code elimination
    std::ofstream liveFile("live.ee");

    for (auto b: blocks) {
        auto &liveIn = inout[b->index].first;
        auto &liveOut = inout[b->index].second;
        liveFile << "block " << b->index << " in ";
        for (auto sym: sym2index)
            if (liveIn.test(sym.second)) liveFile << sym.first << " ";
        liveFile << "out ";
        for (auto sym: sym2index)
            if (liveOut.test(sym.second)) liveFile << sym.first << " ";
        liveFile << std::endl;
    }
    
    auto opt1 = std::ofstream("opt1.ee");
    for (auto b: blocks) {
        opt1 << "block " << b->index << std::endl;
        auto it = b->begin();
        while (it != b->end()) opt1 << *it++ << std::endl;
        opt1 << std::endl;
    }

    for (auto b: blocks) {
        // build DAG (and eliminate local common subexpression)
        DAG dag = DAG::buildDAG(b, globals);
        // the DAG contains quads in [bBegin, bEnd)
        // that is, every thing between the first Label (if any)
        // and final jmps/ret (if any), in other word, the quads ’essential’
        // to this block
		
        auto dagout = std::ofstream(GENNAME("d", b->index));
		dag.printDAG(dagout);
		
        auto liveOut = inout[b->index].second;
		auto it = b->bEnd();
		// point to off-the-end of non-control-flow
		// i.e. first control flow inst (if any)
		// DAG is build upon non-control-flow inst
		// so values live if control flow insts (GOTOXX/RET)
		// may not be alive on block out but is alive on DAG out
		switch (b->endType) {
			case Block::NJBR: case Block::BR:
				if (it->arg2.type == Quad::Address::NAME)
					liveOut.set(sym2index[it->arg2.name]);
				// intentional fallthrough
			case Block::RET:
				if (it->arg1.type == Quad::Address::NAME)
					liveOut.set(sym2index[it->arg1.name]);
				break;
			case Block::J: case Block::NJ:
				break;
		}
        // dead code elimination (with live info)
        dag.eliminateDeadNode(liveOut, sym2index);
		
        auto dagout2 = std::ofstream(GENNAME("de", b->index));
        dag.printDAG(dagout2);
        
        // build CFG from DAG
        b->erase(b->bBegin(), b->bEnd());
        auto tmp = dag.buildQuads(liveOut, sym2index);
        b->insert(b->bEnd(), tmp.begin(), tmp.end());
    }
    
    auto opt2 = std::ofstream("opt2.ee");
    for (auto b: blocks) {
        opt2 << "block " << b->index << std::endl;
        auto it = b->begin();
        while (it != b->end()) opt2 << *it++ << std::endl;
        opt2 << std::endl;
    }
	
    QuadList quadList;
    
    // add everything back before fit iterator
    for (auto b: blocks) quadList.insert(quadList.end(), b->begin(), b->end());
    peepHole(&quadList, quadList.begin(), quadList.end());
	
    quads->insert(fit, quadList.begin(), quadList.end());

}

void peepHole(QuadList *quads, qit begin, qit end)
{
    size_t windowSize = 1;
    qit it = begin;

    // a map of label -> label
    std::map<std::string, std::string> label2label;

    // first pass: detect and optimization
    while (it != end) {

        if (windowSize >= 2) {
            qit q2 = it, q1 = it;
            q1--;
            
            if (q1->op == Quad::GETVEC && q2->op == Quad::SETVEC &&
                q2->arg2.type == Quad::Address::NAME && q1->result == q2->arg2.name &&
                q1->arg1.name == q2->result &&
                q1->arg2 == q2->arg1) {
                // R = A[B] / A[B] = R
                it = q1;
                quads->erase(q2);
            } else if (q1->op == Quad::LABEL && q2->op == Quad::LABEL) {
                // L1: L2: / GOTO L1
                label2label[q2->result] = q1->result;
                it--;
				windowSize--;
                quads->erase(q2);
            } else if (q1->op == Quad::LABEL && q2->op == Quad::GOTO) {
                // L1: / GOTO L2
                label2label[q1->result] = q2->result;
			} else if (q1->op == Quad::GOTO && q2->op == Quad::LABEL &&
					   q1->result == q2->result) {
				// GOTO L1 / L1:
				quads->erase(q1);
				windowSize--;
				if (windowSize>=2) {
					it--;
					windowSize--;
				}
			}
        }
        
        it++;
        windowSize++;
    }

    // second pass: resolve labels and jumps
    it = begin;
    while (it != end) {
        switch (it->op) {
        case Quad::GOTO: case Quad::GOTOEQ: case Quad::GOTONE:
        case Quad::GOTOLT: case Quad::GOTOLE: case Quad::GOTOGT:
        case Quad::GOTOGE:
            while (label2label.find(it->result) != label2label.end())
                it->result = label2label[it->result];
            break;
        default:
            break;
        }
		it++;
    }
	
    return;
}

#define IS_POWER_OF_2(n) ((n) != 0 && ((n) & ((n) - 1)) == 0)
#define GET_POWER(p, n) {     \
        int m = n;            \
        p = 0;                \
        while(m >>= 1) p++; }    \
    
void blockOptimization(Block *block)
{
    auto it = block->begin();
    while (it != block->end()) {
        // strength reduction
        switch(it->op) {
        case Quad::PLUS:
            if (it->arg1.type == Quad::Address::CONST && it->arg1.constant == 0)
                *it = Quad(it->result, it->arg2, Quad::ASSIGN, 0);
            // intentional fallthrough
        case Quad::MINUS:
            if (it->arg2.type == Quad::Address::CONST && it->arg2.constant == 0)
                *it = Quad(it->result, it->arg1, Quad::ASSIGN, 0);
            break;
        case Quad::MULT:
            if (it->arg1.type == Quad::Address::CONST) {
                if (it->arg1.constant == 1)
                    *it = Quad(it->result, it->arg2, Quad::ASSIGN, 0);
                else if (IS_POWER_OF_2(it->arg1.constant)) {
                    int p;
                    GET_POWER(p, it->arg1.constant);
                    *it = Quad(it->result, it->arg2, Quad::SL, p);
                }
                break;
            }
            // intentional fallthrough
        case Quad::DIV:
            if (it->arg2.type == Quad::Address::CONST) {
                if (it->arg2.constant == 1)
                    *it = Quad(it->result, it->arg1, Quad::ASSIGN, 0);
                else if (IS_POWER_OF_2(it->arg2.constant)) {
                    int p;
                    GET_POWER(p, it->arg2.constant);
                    *it = Quad(it->result, it->arg1,
                               (it->op==Quad::MULT ? Quad::SL : Quad::SR), p);
                }
            }
            break;
        default:
            break;
        }
        it++;
    }
}

std::vector<Block*> buildCFG(Block *entry, Block *exit, qit begin, qit end)
{
    std::set<size_t> blockHead;
    std::map<std::string, size_t> potentialTarget;
    std::set<std::string> jumpTarget;

    size_t size = 0;
    qit it = begin;
    while (it != end) {
		std::cerr << (*it) << std::endl;
        switch(it->op) {
        case Quad::GOTOLT: case Quad::GOTOGT: case Quad::GOTOLE:
        case Quad::GOTOGE: case Quad::GOTOEQ: case Quad::GOTONE: {
            jumpTarget.insert(it->result);
            auto tmp = it;
            tmp++;
            if (tmp->op == Quad::GOTO) {
                // combine GOTOXX and GOTO as a single GOTO L1/L2 statement
                // so no more single GOTO blocks
                jumpTarget.insert(tmp->result);
                blockHead.insert(size+2);
                size++; // skip the GOTO stmt
                it++;
            } else blockHead.insert(size+1);
        }
            break;
        case Quad::GOTO: 
            jumpTarget.insert(it->result); // intentional fallthrough
        case Quad::RETURN:
            blockHead.insert(size+1);
            // if it is the last quad, off-the-end will be inserted
            // this is intended (and guaranteed!) behavior
            break;
        case Quad::LABEL:
            potentialTarget[it->result] = size;
            break;
        default:
            break;
        }
        size++;
        it++;
    }

    for (auto target: jumpTarget)
        blockHead.insert(potentialTarget[target]);
	
	for (auto h: blockHead) std::cerr << h+1 << " ";
	std::cerr << std::endl;

    if (*blockHead.begin() == 0) blockHead.erase(blockHead.begin());
	
    it = begin;
    size_t index = 0;
    size_t blockIndex = 2; // 0 and 1 is reserved for entry and exit;

    std::vector<Block*> blockList;
    std::map<size_t, Block*> index2block;

    while (it != end) {
        auto begin = it;
        auto beginIndex = index;

        size_t nextBlock = *blockHead.begin();
        blockHead.erase(blockHead.begin());
        while (index < nextBlock) index++, it++;
        
        auto end = it;

        Block* nb = new Block(begin, end);
        nb->index = blockIndex++;
        index2block[beginIndex] = nb;
        blockList.push_back(nb);
    }
	
	for (auto b: blockList) {
		std::cerr << "block " << b->index << std::endl;
		auto it = b->begin();
		while (it != b->end()) std::cerr << *it++ << std::endl;
		std::cerr << std::endl;
	}

    ADDREL(entry, *blockList.begin(), Block::SEQ);
    
    for (auto b = blockList.begin(); b != blockList.end(); ++b) {
        auto block = *b;
        switch (block->endType) {
        case Block::RET:
            ADDREL(block, exit, Block::EXIT);
            break;
        case Block::BR: {
            auto jInst = block->end();
            jInst--;
            auto brInst = jInst;
            brInst--;
            Block *trueSucc = index2block[potentialTarget[brInst->result]];
            Block *falseSucc = index2block[potentialTarget[jInst->result]];
            ADDREL(block, trueSucc, Block::BRTRUE);
            ADDREL(block, falseSucc, Block::BRFALSE);
        }
            break;
        case Block::J: {
            auto jInst = block->back();
            Block *succ = index2block[potentialTarget[jInst.result]];
            ADDREL(block, succ, Block::NCJ);
        }
            break;
        case Block::NJBR: {
            auto brInst = block->back();
            Block *trueSucc = index2block[potentialTarget[brInst.result]];
            ADDREL(block, trueSucc, Block::BRTRUE);
        } // intentional fallthrough
        case Block::NJ: {
            auto next = b;
            auto succ = *(++next);
            ADDREL(block, succ, Block::SEQ);
        }
            break;
        }
    }

    return blockList;
}
