#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <bitset>

#ifndef CODEGENH
#define CODEGENH

#define MAXVR 512

#include "eeyore.hpp"
#include "interval.hpp"

typedef std::string pseudoReg;
typedef bool instFlag;
typedef long pos;


class LocalVar {
public:
    std::string name;
    size_t offset; // in bytes
    size_t length;

    LocalVar(std::string _s, size_t _o, unsigned _u = 1):
        name(_s), offset(_o), length(_u){}
};

class Inst {
public:
    enum Register {
        zero, ra, sp, gp, tp,
        t0, t1, t2,
        fp, s1,
        a0, a1, a2, a3, a4, a5, a6, a7,
        s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
        t3, t4, t5, t6, NOREG
    } rd = NOREG, rs1 = NOREG, rs2 = NOREG;

    pseudoReg prd, prs1, prs2;
    bool assignedd = false, assigned1 = false, assigned2 = false;

    int32_t imm;

    enum Op {
        ADD, SLT, SLL, SRL, SRA, AND, OR, // NOTE: AND/OR here is bitwise
        SUB, MUL, DIV, REM, // R type only
        SEQZ, SNEZ, SLTZ, SGTZ, SLEZ, SGEZ, NEG, MOV,  // unary
               
        BEQ, BNE, BLT, BLE, BGT, BGE, // branch
        BEQZ, BNEZ, BLTZ, BLEZ, BGTZ, BGEZ,
        
        SW, LUI, LW, LA, LI, // load/store
        J, JAL,

        LABEL,

        RET, // compile time directive
    } op;

    instFlag bit32Op, immOp, symImm, jumpTarget = false;

    bool isBlockHead = false;

    pos ID = -1; // used in register allocation

    Inst(Op _o, pseudoReg _d, pseudoReg _s1, pseudoReg _s2, instFlag bit32 = false):
        prd(_d), prs1(_s1), prs2(_s2), assigned1(false), assigned2(false),
        op(_o), bit32Op(bit32), immOp(false) {}

    Inst(Op _o, pseudoReg _d, pseudoReg _s1, int32_t _i, instFlag bit32 = false):
        prd(_d), prs1(_s1), assigned1(false), imm(_i),
        op(_o), bit32Op(bit32), immOp(true), symImm(false) {}

    Inst(Op _o, pseudoReg _d, pseudoReg _s1, std::string _sym, bool isSym, instFlag bit32):
        prd(_d), prs1(_s1), prs2(_sym), assigned1(false), assigned2(false),
        op(_o), bit32Op(bit32), immOp(true), symImm(isSym) {}

    Inst(Op _o, Register _d, Register _s1, Register _s2, instFlag bit32 = false):
        rd(_d), rs1(_s1), rs2(_s2), assignedd(true), assigned1(true), assigned2(true),
        op(_o), bit32Op(bit32), immOp(false) {}

    Inst(Op _o, Register _d, Register _s1, int32_t _i, instFlag bit32 = false):
        rd(_d), rs1(_s1), assignedd(true), assigned1(true), imm(_i),
        op(_o), bit32Op(bit32), immOp(true), symImm(false) {}
    
    Inst(Op _o, Register _d, Register _s1, std::string _sym, bool isSym, instFlag bit32):
        rd(_d), rs1(_s1), prs2(_sym), assignedd(true), assigned1(true), assigned2(true),
        op(_o), bit32Op(bit32), immOp(true), symImm(isSym) {}

    static void conversion(std::list<Quad>&, std::ostream&);
    const static std::map<Register, std::string> reg2name;
    const static std::set<Op> branchInst;
    const static std::set<Inst::Op> jumpInst;
    const static std::map<Op, std::string> op2name;
};

std::ostream &operator<<(std::ostream&, Inst const &);
std::ostream &operator>>(std::ostream&, Inst const &);

struct BasicBlock {
    int index = -1;
    std::list<Inst>::iterator begin, last, end; // off-the-end
    std::set<BasicBlock*> successor;
    std::set<BasicBlock*> predecessor;

    BasicBlock() = default;  
    // loop detection
    int loopIndex = -1, loopDepth = 0;
    int loopBelonged = -1;
    bool visited = false, active = false;
    std::set<BasicBlock*> outBack, outFor, inBack, inFor;
    size_t incomingForward = 0;
    
    // live analysis
    std::bitset<MAXVR> liveGen, liveKill, liveIn, liveOut;
};

void emitLine(std::string, std::ostream& = std::cout);
void emitLabel(std::string, std::ostream& = std::cout);

int getIndex(std::string, std::map<std::string, int>&);

void buildProc(std::list<Quad>::iterator&, std::ostream&);
void instSelect(std::list<Quad>::iterator&, std::list<Inst>&, std::vector<LocalVar>&,
                std::set<std::string>&, size_t&);

void buildCFG(std::list<Inst>&, BasicBlock&, BasicBlock&, std::vector<BasicBlock>&);
void blockOrder(std::vector<BasicBlock>&, BasicBlock*, std::vector<BasicBlock*>&);
void processBlock(BasicBlock*, BasicBlock*, std::set<BasicBlock*>&, unsigned*);
void backwardReach(BasicBlock*, std::vector<std::vector<bool>>&, int loopIndex);
int compareLoop(std::vector<std::vector<bool>>&, int, int);

void blockLiveSet(BasicBlock*, std::map<std::string, int>&);
void buildInterval(std::vector<BasicBlock*>&, std::map<std::string, int>&,
                   std::vector<Interval>&, size_t);

void walkInterval(std::vector<Interval>&, std::vector<BasicBlock*>&, int&);
bool tryAllocate(Interval*, std::list<Interval*>&, std::list<Interval*>&,
                 std::multiset<Interval*, Interval::CompInterval>&, std::vector<BasicBlock*>&);
void setPos(Interval*, pos, int[]);
bool allocateBlocked(Interval*, std::list<Interval*>&, std::list<Interval*>&,
                     std::multiset<Interval*, Interval::CompInterval>&,
                     std::vector<BasicBlock*>&, int&);
pos optimalSplit(pos, pos, std::vector<BasicBlock*>&);
void splitAndSpill(Interval*, Interval*, std::vector<BasicBlock*>&,
                   std::multiset<Interval*, Interval::CompInterval>&, int&);

void insertMove(std::list<Inst>&, std::vector<BasicBlock*>&, pos, Inst::Register, Inst::Register);
void insertMoveSpillOut(std::list<Inst>&, std::vector<BasicBlock*>&, pos, Inst::Register, size_t);
void insertMoveLoadBack(std::list<Inst>&, std::vector<BasicBlock*>&, pos, Inst::Register, size_t);

void insertInst(std::list<Inst>&, std::vector<Interval>&, std::vector<BasicBlock*>&, size_t);
void insertInst(std::list<Inst>&, std::vector<Interval>&, std::vector<BasicBlock*>&);


class Dependency {
    int useVec[32] = {0};
    std::list<Inst> insts;

    void useReg(int reg) { if (reg!=Inst::t0 && reg>=0 && reg<32) useVec[reg]++; }
    void deUseReg(int reg) { if (reg!=Inst::t0 && reg>=0 && reg<32) useVec[reg]--; }
    int testReg(int reg) { return reg!=Inst::t0 && reg>=0 && reg<32 ? useVec[reg] : 0; }

public:
    void addInst(Inst inst) {
        insts.push_back(inst);
        useReg(inst.rs1);
    }

    std::vector<Inst> resolve() {
        std::vector<Inst> result;
        while (!insts.empty()) {
            bool emitted = false;
            for (auto inst = insts.begin(); inst != insts.end(); ++inst) {
                if (!testReg(inst->rd)) {
                    // do not destroy reg used by other inst
                    emitted = true;
                    result.push_back(*inst);
                    deUseReg(inst->rs1);
                    insts.erase(inst);
                    break;
                }
            }
            if (emitted) continue;
            // else there is a loop, need a tmp to break it
            // due to the special topology of the dependency graph
            // anything is either a simple sequence or a simple ring
            // by this time any inst is in a ring
            // break ring by select any inst mov $b, $a
            auto tmp = insts.front();
            insts.pop_front();
            deUseReg(tmp.rs1);

            // emit mov $t0, $a (t0 does not count for use)
            // add mov $b, $t0 to list
            result.push_back(Inst(Inst::MOV, Inst::t0, tmp.rs1, 0));
            addInst(Inst(Inst::MOV, tmp.rd, Inst::t0, 0));
        }

        return result;
    }
};

#endif
