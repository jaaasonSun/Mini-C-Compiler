#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <list>

#include "eeyore.hpp"

#define ENTRY_INDEX 0
#define EXIT_INDEX 1

#define MAX_VAR 1024

class Block {
public:
    using QuadList = std::list<Quad>;
    using qit = QuadList::iterator;
    QuadList quads;

    static size_t indexCounter;
    size_t index;
    
    enum SuccType {
        SEQ, NCJ, BRTRUE, BRFALSE, EXIT
    };

    struct Succ {
        Block *block;
        SuccType type;
        Succ();
        Succ(Block* b, SuccType s): block(b), type(s){}
    };

    enum EndType {
        RET, // return
        NJ, // not jump (sequential)
        J, // GOTO
        NJBR, // single GOTOXX without GOTO
        BR // GOTOXX with GOTO
    } endType;

    bool removed = false;

    Block() = default;
    
    Block(qit b, qit e): quads(b, e) {
        qit back = quads.end();
        back--;

        switch(back->op) {
        case Quad::RETURN:
             endType = RET;
             break;
        case Quad::GOTO:
            if (back == quads.begin()) {
                endType = J;
            } else {
                auto tmp = back;
                tmp--;
                switch (tmp->op) {
                case Quad::GOTOLT: case Quad::GOTOGT: case Quad::GOTOLE:
                case Quad::GOTOGE: case Quad::GOTOEQ: case Quad::GOTONE:
                    endType = BR;
                    break;
                default:
                    endType = J;
                    break;
                }
            }
            break;
        case Quad::GOTOLT: case Quad::GOTOGT: case Quad::GOTOLE:
        case Quad::GOTOGE: case Quad::GOTOEQ: case Quad::GOTONE:
            endType = NJBR;
            break;
        default:
            endType = NJ;
            break;
        }

        index = indexCounter++;
    }

    std::vector<Succ> pred, succ;
    
    inline qit begin() { return quads.begin(); }
    inline qit end() { return quads.end(); }
    inline Quad back() { return quads.back(); }
    inline qit bEnd() {
        // return the end of actual stmts,
        // that is, discount GOTO/GOTOXX in the end
        qit actualBack = end();
        switch (endType) {
        case BR: actualBack--;
        case J: case RET: case NJBR: actualBack--;
        case NJ: return actualBack;
        }
    }

    inline qit bBegin() {
        // return the begin of actual stmts,
        // that is, discount initial labels
        // if a label is in the middle of a block
        // the label is useless since nothing jumps to it
        qit actualBegin = begin();
        while (actualBegin->op == Quad::LABEL) actualBegin++;
        return actualBegin;
    }

    // boilerplate utility function
    inline void insert(qit p, qit b, qit e) { quads.insert(p, b, e); }
    inline void erase(qit it) { quads.erase(it); }
    inline void erase(qit b, qit e) { quads.erase(b, e); }
    inline size_t size() { return quads.size(); }
    inline void clear() { quads.clear(); }

    // enclosing remove/isRemoved for better future implementation (if any)
    void remove() {
        removed = true;
    }
    bool isRemoved() {
        return removed;
    }

    bool isUnreachable() {
        if (removed) return true;
        if (!pred.size()) return true;
        for (auto p: pred)
            if (p.block != this && !p.block->isRemoved())
                return false;
        return true;          
    }

    inline void addSucc(Block *b, SuccType t) { succ.push_back(Succ(b, t)); }
    inline void addPred(Block *b, SuccType t) { pred.push_back(Succ(b, t)); }

};

#define ADDREL(pre, sux, type) {                             \
        (pre)->succ.push_back(Block::Succ((sux), (type)));     \
        (sux)->pred.push_back(Block::Succ((pre), (type)));     \
    } 

using QuadList = std::list<Quad>;
using qit = QuadList::iterator;

void analysis(QuadList*);
void funcAnalysis(QuadList*, qit&, std::set<std::string> const&, size_t);
std::vector<Block*> buildCFG(Block*, Block*, qit, qit);
void blockOptimization(Block*);
void peepHole(QuadList*, qit, qit);

#endif
