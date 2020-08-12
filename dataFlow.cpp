#include <set>
#include <map>
#include <bitset>
#include "dataFlow.hpp"


// class DataFlow
template <class Value, class BlockInfo>
std::map<size_t, std::pair<Value, Value>> DataFlow<Value, BlockInfo>::analyse() {
    using InOut = std::pair<Value, Value>;
    inout.clear();
	
    if (direction == FORWARD) {
        for (auto b: blocks) inout[b->index].second = top;
        inout[ENTRY_INDEX].second = edgeValue;
    } else {
        for (auto b: blocks) inout[b->index].first = top;
        inout[EXIT_INDEX].first = edgeValue;
    }
	
    bool change = true;
    while (change) {
        change = false;
        if (direction == FORWARD) {
            for (auto b: blocks) {
                if (b->index == ENTRY_INDEX) continue;
                auto &p = inout[b->index];
                p.first = top;
                for (auto pre: b->pred)
                    p.first = meet(p.first, inout[pre.block->index].second);
                Value tmp = p.second;
                p.second = trans(b, p.first);
                if (!change && tmp != p.second) change = true;
            }
        } else {
            for (auto b: blocks) {
                if (b->index == EXIT_INDEX) continue;
                auto &p = inout[b->index];
                p.second = top;
                for (auto sux: b->succ)
                    p.second = meet(p.second, inout[sux.block->index].first);
                Value tmp = p.first;
                p.first = trans(b, p.second);
                if (!change && tmp != p.first) change = true;
            }
        }
    }
    return inout;
}

bool operator==(Symbol const &s1, Symbol const &s2)
{
    if (s1.state != s2.state) return false;
    if (s1.state == Symbol::CONSTANT && s1.value != s2.value) return false;
    return true;
}

using symVec = std::vector<Symbol>;

int ConstantFolding::unaryCal(Quad::Operation op, int opr) {
    switch (op) {
    case Quad::NOT: return !opr;
    case Quad::NEG: return -opr;
    case Quad::ASSIGN: return opr;
    default: throw DATAFLOWBUG;
    }
}

int ConstantFolding::binaryCal(Quad::Operation op, int opr1, int opr2) {
    switch(op) {
    case Quad::EQ: return opr1 == opr2;
    case Quad::IEQ: return opr1 != opr2;
    case Quad::LT: return opr1 < opr2;
    case Quad::GT: return opr1 > opr2;
    case Quad::AND: return opr1 && opr2;
    case Quad::OR: return opr1 || opr2;
    case Quad::PLUS: return opr1 + opr2;
    case Quad::MINUS: return opr1 - opr2;
    case Quad::MULT: return opr1 * opr2;
    case Quad::DIV: return opr1 / opr2;
    case Quad::REM: return opr1 % opr2;
    case Quad::SL: return opr1 << opr2;
    case Quad::SR: return opr1 >> opr2;
    default: throw DATAFLOWBUG;
    }
}


// class ConstantFloding
void ConstantFolding::apply() {
    for (auto b: blocks) {
        auto info = inout[b->index].first;
        auto it = b->begin();
        while (it != b->end()) {
            switch(it->op) {
            case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
                if (it->arg1.type == Quad::Address::CONST) {
                    info[sym2index[it->result]] =
                        Symbol(Symbol::CONSTANT,
                               unaryCal(it->op, it->arg1.constant));
                    *it = Quad(it->result, unaryCal(it->op, it->arg1.constant),
                               Quad::ASSIGN, 0);
                } else {
                    auto symInfo = info[sym2index[it->arg1.name]];
                    if (symInfo.state == Symbol::CONSTANT) {
                        *it = Quad(it->result, unaryCal(it->op, symInfo.value),
                                   Quad::ASSIGN, 0);
                    }
                }
					
                break;
            case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT:
            case Quad::AND: case Quad::OR: case Quad::PLUS: case Quad::MINUS:
            case Quad::MULT: case Quad::DIV: case Quad::REM:
            case Quad::SETVEC: case Quad::SL: case Quad::SR: {
                Symbol::State s1, s2;
                int v1, v2;
                if (it->arg1.type == Quad::Address::CONST) {
                    s1 = Symbol::CONSTANT;
                    v1 = it->arg1.constant;
                } else {
                    Symbol sym = info[sym2index[it->arg1.name]];
                    if (sym.state == Symbol::CONSTANT) {
                        s1 = Symbol::CONSTANT;
                        v1 = sym.value;
                        it->arg1 = Quad::Address(v1);
                    } else {
                        s1 = sym.state;
                    }
                }
					
                if (it->arg2.type == Quad::Address::CONST) {
                    s2 = Symbol::CONSTANT;
                    v2 = it->arg2.constant;
                } else {
                    Symbol sym = info[sym2index[it->arg2.name]];
                    if (sym.state == Symbol::CONSTANT) {
                        s2 = Symbol::CONSTANT;
                        v2 = sym.value;
                        it->arg2 = Quad::Address(v2);
                    } else {
                        s2 = sym.state;
                    }
                }
					
                if (it->op == Quad::SETVEC) break;
					
                Symbol &result = info[sym2index[it->result]];
                if (s1 == Symbol::CONSTANT && s2 == Symbol::CONSTANT) {
                    int value = binaryCal(it->op, v1, v2);
                    result = Symbol(Symbol::CONSTANT, value);
                    *it = Quad(it->result, value, Quad::ASSIGN, 0);
                } else if ((it->op == Quad::AND || it->op == Quad::OR) &&
                           (s1 == Symbol::CONSTANT || s2 == Symbol::CONSTANT)) {
                    // special case, for && and OR only one oprand is required
                    // to reduce the expression
                    if (s1 == Symbol::CONSTANT) {
                        if ((it->op == Quad::OR && v1) || (it->op == Quad::AND && !v1)) {
                            result = Symbol(Symbol::CONSTANT, !!v1);
                            *it = Quad(it->result, !!v1, Quad::ASSIGN, 0);
                        } else result = info[sym2index[it->arg2.name]];
                    } else {
                        if ((it->op == Quad::OR && v2) || (it->op == Quad::AND && !v2)) {
                            *it = Quad(it->result, !!v2, Quad::ASSIGN, 0);
                            result = Symbol(Symbol::CONSTANT, !!v2);
                        } else result = info[sym2index[it->arg1.name]];
                    }
                } else {
                    if (s1 == Symbol::UNDEFINED || s2 == Symbol::UNDEFINED)
                        result = Symbol(Symbol::UNDEFINED);
                    else result = Symbol(Symbol::NAC);
                }
            }
                break;
            case Quad::RETURN: case Quad::PARAM:
                if (it->arg1.type == Quad::Address::NAME)
                    if (info[sym2index[it->arg1.name]].state == Symbol::CONSTANT)
                        it->arg1 = Quad::Address(info[sym2index[it->arg1.name]].value);
                break;
            case Quad::CALL:
                info[sym2index[it->result]] = Symbol(Symbol::NAC);
                // intentional fallthrough
            case Quad::STMTCALL:
                for (auto g: globals)
                    info[sym2index[g]] = Symbol(Symbol::NAC);
                break;
            case Quad::GETVEC:
                info[sym2index[it->result]] = Symbol(Symbol::UNDEFINED);
                break;
            case Quad::DEFF: case Quad::ENDF:
                throw QUADBUG;
            case Quad::GOTOEQ: case Quad::GOTONE: case Quad::GOTOLT:
            case Quad::GOTOGT: case Quad::GOTOLE: case Quad::GOTOGE: {
                // if condition is determined, the branch can be reduced
                Symbol::State s1, s2;
                int v1 = 0, v2 = 0;
                if (it->arg1.type == Quad::Address::CONST) {
                    s1 = Symbol::CONSTANT;
                    v1 = it->arg1.constant;
                } else {
                    Symbol sym = info[sym2index[it->arg1.name]];
                    if (sym.state == Symbol::CONSTANT) {
                        s1 = Symbol::CONSTANT;
                        v1 = sym.value;
                        it->arg1 = Quad::Address(v1);
                    } else {
                        s1 = sym.state;
                    }
                }
					
                if (it->arg2.type == Quad::Address::CONST) {
                    s2 = Symbol::CONSTANT;
                    v2 = it->arg2.constant;
                } else {
                    Symbol sym = info[sym2index[it->arg2.name]];
                    if (sym.state == Symbol::CONSTANT) {
                        s2 = Symbol::CONSTANT;
                        v2 = sym.value;
                        it->arg2 = Quad::Address(v2);
                    } else {
                        s2 = sym.state;
                    }
                }
					
                if (s1 == Symbol::CONSTANT && s2 == Symbol::CONSTANT) {
                    bool cond = false;
                    switch (it->op) {
                    case Quad::GOTOEQ: cond = v1 == v2; break;
                    case Quad::GOTONE: cond = v1 != v2; break;
                    case Quad::GOTOLT: cond = v1 < v2; break;
                    case Quad::GOTOGT: cond = v1 > v2; break;
                    case Quad::GOTOLE: cond = v1 <= v2; break;
                    case Quad::GOTOGE: cond = v1 >= v2; break;
                    default: throw QUADBUG;
                    }
                    if (cond) *it = Quad(it->result, 0, Quad::GOTO, 0);
                    else *it = Quad("", 0, Quad::NOP, 0);
                }
            }
                break;
            case Quad::NOP: case Quad::LABEL: case Quad::GOTO:
            case Quad::DEFVAR: case Quad::DEFVEC:
                break;
            }
            it++;
        }
    }
}

symVec ConstantFolding::meet(symVec const &c1, symVec const &c2) {
    size_t size = c1.size();
    symVec newSym(size);
    for (size_t i = 0; i < size; ++i) {
        if (c1[i].state == Symbol::NAC || c2[i].state == Symbol::NAC ||
            (c1[i].state == Symbol::CONSTANT && c2[i].state == Symbol::CONSTANT
             && c1[i].value != c2[i].value))
            newSym[i].state = Symbol::NAC;
        else if (c1[i].state == Symbol::UNDEFINED &&
                 c2[i].state == Symbol::UNDEFINED)
            newSym[i].state = Symbol::UNDEFINED;
        else if (c1[i].state == Symbol::CONSTANT) newSym[i] = c1[i];
        else newSym[i] = c2[i];
    }
    return newSym;
}

symVec ConstantFolding::trans(Block *b, symVec const &vec) {
    symVec info(vec);
    auto it = b->begin();
    while (it != b->end()) {
        switch(it->op) {
        case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
            if (it->arg1.type == Quad::Address::CONST) {
                info[sym2index[it->result]] =
                    Symbol(Symbol::CONSTANT,
                           unaryCal(it->op, it->arg1.constant));
            } else info[sym2index[it->result]] =
                       info[sym2index[it->arg1.name]];
            break;
        case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT:
        case Quad::AND: case Quad::OR: case Quad::PLUS: case Quad::MINUS:
        case Quad::MULT: case Quad::DIV: case Quad::REM: case Quad::SL:
        case Quad::SR: {
            Symbol::State s1, s2;
            int v1, v2;
            if (it->arg1.type == Quad::Address::CONST) {
                s1 = Symbol::CONSTANT;
                v1 = it->arg1.constant;
            } else {
                Symbol sym = info[sym2index[it->arg1.name]];
                if (sym.state == Symbol::CONSTANT) {
                    s1 = Symbol::CONSTANT;
                    v1 = sym.value;
                } else {
                    s1 = sym.state;
                }
            }
				
            if (it->arg2.type == Quad::Address::CONST) {
                s2 = Symbol::CONSTANT;
                v2 = it->arg2.constant;
            } else {
                Symbol sym = info[sym2index[it->arg2.name]];
                if (sym.state == Symbol::CONSTANT) {
                    s2 = Symbol::CONSTANT;
                    v2 = sym.value;
                } else {
                    s2 = sym.state;
                }
            }
				
            Symbol &result = info[sym2index[it->result]];
            if (s1 == Symbol::CONSTANT && s2 == Symbol::CONSTANT) {
                result = Symbol(Symbol::CONSTANT, binaryCal(it->op, v1, v2));
            } else if ((it->op == Quad::AND || it->op == Quad::OR) &&
                       (s1 == Symbol::CONSTANT || s2 == Symbol::CONSTANT)) {
                // special case, for && and OR only one oprand is required
                // to reduce the expression
                if (s1 == Symbol::CONSTANT) {
                    if ((it->op == Quad::OR && v1) || (it->op == Quad::AND && !v1))
                        result = Symbol(Symbol::CONSTANT, !!v1);
                    else result = info[sym2index[it->arg2.name]];
                } else {
                    if ((it->op == Quad::OR && v2) || (it->op == Quad::AND && !v2))
                        result = Symbol(Symbol::CONSTANT, !!v2);
                    else result = info[sym2index[it->arg1.name]];
                }
            } else {
                if (s1 == Symbol::UNDEFINED || s2 == Symbol::UNDEFINED)
                    result = Symbol(Symbol::UNDEFINED);
                else result = Symbol(Symbol::NAC);
            }
        }
            break;
        case Quad::CALL:
            info[sym2index[it->result]] = Symbol(Symbol::NAC);
            // intentional fallthrough
        case Quad::STMTCALL:
            for (auto g: globals)
                info[sym2index[g]] = Symbol(Symbol::NAC);
            break;
        case Quad::GETVEC:
            info[sym2index[it->result]] = Symbol(Symbol::NAC);
            break;
        case Quad::DEFF: case Quad::ENDF:
            throw QUADBUG;
        default:
            break;
        }
        it++;
    }
    return info;
}


// class LiveAnalysis
using Vars = std::bitset<MAX_VAR>;

Vars LiveAnalysis::buildExitValue(std::map<std::string, size_t> &sym2index,
                                  std::set<std::string> const &globals) const {
    Vars value;
    for (auto g: globals) value.set(sym2index[g]);
    return value;
}

void LiveAnalysis::genVar(Block *block, Quad::Address add) {
    if (add.type == Quad::Address::CONST) return;
    size_t index = sym2index[add.name];
    if (!kill[block->index].test(index))
        gen[block->index].set(index);
}

void LiveAnalysis::killVar(Block *block, std::string name) {
    kill[block->index].set(sym2index[name]);
}

void LiveAnalysis::calcGenKill(Block *block) {
    auto it = block->begin();
    while (it != block->end()) {
        switch (it->op) {
        case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT:
        case Quad::AND: case Quad::OR: case Quad::PLUS: case Quad::MINUS:
        case Quad::MULT: case Quad::DIV: case Quad::REM: case Quad::GETVEC:
        case Quad::SL: case Quad::SR:
            genVar(block, it->arg1);
            // intentional fallthrough
        case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
            genVar(block, it->arg2);
            killVar(block, it->result);
            break;
        case Quad::GOTOEQ: case Quad::GOTONE: case Quad::GOTOLT:
        case Quad::GOTOGT: case Quad::GOTOLE: case Quad::GOTOGE:
        case Quad::SETVEC:
            genVar(block, it->arg2);
            // intentional fallthrough
        case Quad::PARAM: case Quad::RETURN:
            genVar(block, it->arg1);
            break;
        case Quad::CALL:
            killVar(block, it->result);
            // intentional fallthrough
        case Quad::STMTCALL:
            // function calls require all globals
            for (auto sym: globals) genVar(block, sym);
            break;
        case Quad::GOTO: case Quad::DEFVAR: case Quad::DEFVEC:
        case Quad::LABEL: case Quad::NOP:
            break;
        case Quad::DEFF: case Quad::ENDF:
            throw QUADBUG;
        }
        it++;
    }
	
}

Vars LiveAnalysis::meet(Vars const &v1, Vars const &v2) {
    return v1 | v2;
}

Vars LiveAnalysis::trans(Block *b, Vars const &v) {
    return gen[b->index] | (v & ~kill[b->index]);
}
