#include <vector>
#include <stack>
#include <list>
#include <map>
#include <string>

#include "eeyore.hpp"

#define NO_LE_GE

const std::string INDENTATION = "    ";
const std::string TMPPREFIX = "q";
const std::string FUNCPREFIX = "f_";
const std::string LABELPREFIX = "l";

unsigned Quad::labelCounter = 0;
unsigned Quad::tmpCounter = 0;

void Quad::buildQuad(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildQuad");
    for (auto &stmt: node.child) {
        switch (stmt.type) {
        case TreeNode::STMTS:
            buildQuad(seq, stmt);
            break;
            
        case TreeNode::VARDEF:
            if (stmt.child.size() == 1)
                seq.push_back(Quad("", stmt.child[0].canonicalName, Quad::DEFVAR, 0));
            else
                seq.push_back(Quad("", stmt.child[0].canonicalName, Quad::DEFVEC,
                                   stmt.child[1].value * 4));
            break;
        case TreeNode::FDECL:
            break;
        case TreeNode::FDEF:
            buildFunc(seq, stmt);
            break;
        case TreeNode::STMT:
            buildStmt(seq, stmt);
            break;
        default:
            std::cout << "Unreachable case in buildQuad" << std::endl;
            seq.push_back(Quad("BuildQuad ERROR", 0, Quad::LABEL, 0));
            break;
            
        }
    }
    LEAVE("buildQuad");
}

void Quad::buildStmt(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildNode");
    switch(node.stmt) {
    case TreeNode::BLOCK:
        buildQuad(seq, node);
        break;
    case TreeNode::EMPTY:
        break;
    case TreeNode::WHILE: case TreeNode::IF:
        buildIfWhile(seq, node);
        break;
    case TreeNode::IFELSE:
        buildIfElse(seq, node);
        break;
    case TreeNode::VARASSIGN:
        buildExpr(seq, node.child[1]);
        // directly modify the last quad generated
        POPTMPDEF;
        seq.back().result = node.child[0].canonicalName;
        tmpCounter--;
        break;
    case TreeNode::VECASSIGN: {
        bool isNumA = node.child[1].type == TreeNode::NUM;
        bool isNumB = node.child[2].type == TreeNode::NUM;
        std::string nameA, nameB;
                
        if (!isNumA) {            
            if (node.child[1].type == TreeNode::EXPR) {
                buildExpr(seq, node.child[1]);
                DEFTMP(0);
                nameA = GENNAME(TMPPREFIX, tmpCounter++);
                seq.push_back(Quad(nameA, GENNAME(TMPPREFIX, tmpCounter-2),
                                   Quad::MULT, 4));
            } else {
                DEFTMP(0);
                nameA = GENNAME(TMPPREFIX, tmpCounter++);
                seq.push_back(Quad(nameA, node.child[1].canonicalName,
                                   Quad::MULT, 4));
            }
        }
        if (!isNumB) {
            if (node.child[2].type == TreeNode::EXPR) {
                buildExpr(seq, node.child[2]);
                nameB = seq.back().result;
            } else {
                nameB = node.child[2].canonicalName;
            }
        }
                
        if (isNumA && isNumB)
            seq.push_back(Quad(node.child[0].canonicalName,
                               node.child[1].value*4,
                               Quad::SETVEC, node.child[2].value));
        else if (isNumA && !isNumB)
            seq.push_back(Quad(node.child[0].canonicalName,
                               node.child[1].value*4,
                               Quad::SETVEC, nameB));
        else if (!isNumA && isNumB)
            seq.push_back(Quad(node.child[0].canonicalName, nameA,
                               Quad::SETVEC, node.child[2].value));
        else if (!isNumA && !isNumB)
            seq.push_back(Quad(node.child[0].canonicalName, nameA,
                               Quad::SETVEC, nameB));
    }
        break;
    case TreeNode::RETURN:
        if (node.child[0].type == TreeNode::EXPR) {
            buildExpr(seq, node.child[0]);
            seq.push_back(Quad("", seq.back().result, Quad::RETURN, 0));
        } else if (node.child[0].type == TreeNode::ID)
            seq.push_back(Quad("", node.child[0].canonicalName, Quad::RETURN, 0));
        else seq.push_back(Quad("", node.child[0].value, Quad::RETURN, 0));
        break;
    case TreeNode::SFCALL: 
        buildCall(seq, node);
		POPTMPDEF;
        seq.back().op = Quad::STMTCALL;
        break;
    default:
        std::cout << "Unreachable case in STMT" << std::endl;
        seq.push_back(Quad("BuildQuad ERROR", 0, Quad::LABEL, 0));
        break;
    }
    
    LEAVE("buildStmt");
}

void Quad::buildExpr(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildExpr");
    // buildExpr will always save result in a tmp variable

    // generally these three cases should not happen (especially the last one!)
    // NUMs and IDs should have already been handled in the upper levels to avoid
    // redundent assigns, but these can be handled later with copy propagation
    // and constant propagation
    if (node.type == TreeNode::ID) {
        DEFTMP(0);
        seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                           node.canonicalName,Quad::ASSIGN, 0));
    } else if (node.type == TreeNode::NUM) {
        DEFTMP(0);
        seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                           node.value, Quad::ASSIGN, 0));
    }
    else if (node.type != TreeNode::EXPR) {
        std::cout << "Attempt to build from a non-expr" << std::endl;
        seq.push_back(Quad("BuildExpr ERROR", 0, Quad::LABEL, 0));
    } else switch (node.op) {
        case TreeNode::FCALL:
            buildCall(seq, node);
            break;
        case TreeNode::VECACCESS: {
            if (node.child[1].type == TreeNode::NUM) {
                DEFTMP(0);
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   node.child[0].canonicalName,
                                   Quad::GETVEC, node.child[1].value*4));
                break;
            }
        if (node.child[1].type == TreeNode::EXPR) {
            buildExpr(seq, node.child[1]);
            DEFTMP(0);
            seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter),
                               GENNAME(TMPPREFIX, tmpCounter-1),
                               Quad::MULT, 4));
            tmpCounter++;
        } else {
            DEFTMP(0);
            seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                  node.child[1].canonicalName, Quad::MULT, 4));
        }

        auto tmp = seq.back().result;
        DEFTMP(0);
        seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                           node.child[0].canonicalName,
                           Quad::GETVEC, tmp));
        break;
        }
        case TreeNode::NEG: case TreeNode::NOT: {
            auto op = op2op.at(node.op);
            
            if (node.child[0].type == TreeNode::NUM) {
                DEFTMP(0);
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   node.child[0].value, op, 0));
            } else if (node.child[0].type == TreeNode::ID) {
                DEFTMP(0);
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                               node.child[0].canonicalName, op, 0));
            } else {
                buildExpr(seq, node.child[0]);
                auto tmp = seq.back().result;
                DEFTMP(0);
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   tmp, op, 0));
            }
        }
            break;
        case TreeNode::MULT: case TreeNode::DIV: case TreeNode::REM: case TreeNode::PLUS:
        case TreeNode::MINUS: case TreeNode::LT: case TreeNode::GT: case TreeNode::EQ:
        case TreeNode::IEQ: case TreeNode::AND: case TreeNode::OR: {
            auto op = op2op.at(node.op);
            bool isNumA = node.child[0].type == TreeNode::NUM;
            bool isNumB = node.child[1].type == TreeNode::NUM;
            std::string nameA, nameB;
            
            if (!isNumA) {            
                if (node.child[0].type == TreeNode::EXPR) {
                buildExpr(seq, node.child[0]);
                nameA = seq.back().result;
                } else nameA = node.child[0].canonicalName;
            }
            if (!isNumB) {
                if (node.child[1].type == TreeNode::EXPR) {
                    buildExpr(seq, node.child[1]);
                    nameB = seq.back().result;
                } else nameB = node.child[1].canonicalName;
            }
            
            DEFTMP(0);
            if (isNumA && isNumB)
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                               node.child[0].value,
                                   op, node.child[1].value));
            else if (isNumA && !isNumB)
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   node.child[0].value, op, nameB));
            else if (!isNumA && isNumB)
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   nameA, op, node.child[1].value));
            else if (!isNumA && !isNumB)
                seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                                   nameA, op, nameB));
        }
            break;
        default:
            std::cout << "Unreachable case in buildExpr" << std::endl;
            seq.push_back(Quad("BuildExpr ERROR", 0, Quad::LABEL, 0));
        }
    LEAVE("buildExpr");
}

void Quad::buildCall(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildCall");
	
	std::vector<Quad> params;
	
    for (auto arg: node.child[1].child) {
        if (arg.type == TreeNode::ID)
            params.push_back(Quad("", arg.canonicalName, Quad::PARAM, 0));
        else if (arg.type == TreeNode::NUM)
            params.push_back(Quad("", arg.value, Quad::PARAM, 0));
        else {
            buildExpr(seq, arg);
            params.push_back(Quad("", seq.back().result, Quad::PARAM, 0));
        }
    }
	
    seq.insert(seq.end(), params.begin(), params.end());
    
    DEFTMP(0);
    seq.push_back(Quad(GENNAME(TMPPREFIX, tmpCounter++),
                       FUNCPREFIX+node.child[0].id, Quad::CALL,
                       node.child[1].child.size()));
    LEAVE("buildCall");
}

void Quad::buildFunc(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildFunc");
    // f_func [n]
    seq.push_back(Quad("", FUNCPREFIX+node.child[0].id, Quad::DEFF,
                       node.child[1].child.size()));
    // function body
    buildQuad(seq, node.child[2]);
    // end f_func
    seq.push_back(Quad("", FUNCPREFIX+node.child[0].id, Quad::ENDF, 0));
    LEAVE("buildFunc");
}

void Quad::buildIfWhile(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildIfWhile");
    std::string L1 = GENNAME(LABELPREFIX, labelCounter++);
    std::string L2 = GENNAME(LABELPREFIX, labelCounter++);
    std::string L3 = GENNAME(LABELPREFIX, labelCounter++);

    if (node.stmt == TreeNode::WHILE)
        seq.push_back(Quad(L2, 0, Quad::LABEL, 0));

    if (node.child[0].type == TreeNode::ID)
        seq.push_back(Quad(L1, node.child[0].canonicalName, Quad::GOTOEQ, 0));
    else if (node.child[0].type == TreeNode::NUM)
        seq.push_back(Quad(L1, node.child[0].value, Quad::GOTOEQ, 0));
    else {
        buildExpr(seq, node.child[0]);
        switch(seq.back().op) {
        case LT: case GT: case EQ: case IEQ:
            POPTMPDEF;
#ifdef NO_LE_GE
            seq.back().op = Quad::op2cond.at(seq.back().op);
            seq.back().result = L3;
            seq.push_back(Quad(L1, 0, Quad::GOTO, 0));
            seq.push_back(Quad(L3, 0, Quad::LABEL, 0));
#else
            seq.back().op = Quad::reverse.at(Quad::op2cond.at(seq.back().op));
            seq.back().result = L1;
#endif
            tmpCounter--;
            break;
        default:
            seq.push_back(Quad(L1, seq.back().result, Quad::GOTOEQ, 0));
            break;
        }
    }

    buildStmt(seq, node.child[1]);
    if (node.stmt == TreeNode::WHILE)
        seq.push_back(Quad(L2, 0, Quad::GOTO, 0));
    seq.push_back(Quad(L1, 0, Quad::LABEL, 0));
    LEAVE("buildIfWhile");
}

void Quad::buildIfElse(std::list<Quad> &seq, TreeNode &node)
{
    ENTER("buildIfElse");
    std::string L1 = GENNAME(LABELPREFIX, labelCounter++);
    std::string L2 = GENNAME(LABELPREFIX, labelCounter++);
    
    if (node.child[0].type == TreeNode::ID)
        seq.push_back(Quad(L1, node.child[0].canonicalName, Quad::GOTONE, 0));
    else if (node.child[0].type == TreeNode::NUM)
        seq.push_back(Quad(L1, node.child[0].value, Quad::GOTONE, 0));
    else {
        buildExpr(seq, node.child[0]);
        switch(seq.back().op) {
        case LT: case GT: case EQ: case IEQ:
            POPTMPDEF;
            seq.back().op = Quad::op2cond.at(seq.back().op);
            seq.back().result = L1;
            break;
        default:
            seq.push_back(Quad(L1, seq.back().result, Quad::GOTONE, 0));
            break;
        }
    }
    buildStmt(seq, node.child[2]);
    seq.push_back(Quad(L2, 0, Quad::GOTO, 0));
    seq.push_back(Quad(L1, 0, Quad::LABEL, 0));
    buildStmt(seq, node.child[1]);
    seq.push_back(Quad(L2, 0, Quad::LABEL, 0));
    LEAVE("buildIfElse");
}

bool operator==(Quad::Address const &add1, Quad::Address const &add2)
{
	if (add1.type != add2.type) return false;
	if (add1.type == Quad::Address::CONST && add1.constant != add2.constant)
		return false;
	if (add1.type == Quad::Address::NAME && add1.name != add2.name)
		return false;
	return true;
}

bool operator!=(Quad::Address const &add1, Quad::Address const &add2)
{
	return !(add1==add2);
}


std::ostream& operator<<(std::ostream &os, const Quad::Address &add)
{
    if (add.type == Quad::Address::NAME) os << add.name;
    else os << add.constant;
    return os;
}

std::ostream& operator<<(std::ostream &os, const Quad &quad)
{
    switch (quad.op) {
    case Quad::NOT: case Quad::NEG: case Quad::CALL: case Quad::ASSIGN:
        os << quad.result << " = " << Quad::op2name.at(quad.op) << " " << quad.arg1;
        break;
    case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT:
    case Quad::AND: case Quad::OR: case Quad::PLUS: case Quad::MINUS:
	case Quad::MULT: case Quad::DIV: case Quad::REM: case Quad::SL:
	case Quad::SR:
        os << quad.result << " = " << quad.arg1 << " " <<  Quad::op2name.at(quad.op)
           << " " << quad.arg2;
        break;
    case Quad::SETVEC:
        os << quad.result << "[" << quad.arg1 << "] = " << quad.arg2;
        break;
    case Quad::GETVEC:
        os << quad.result << " = " << quad.arg1 << "[" << quad.arg2 << "]";
        break;
    case Quad::DEFF:
        os << quad.arg1 << " [" << quad.arg2 << "]";
        break;
    case Quad::ENDF:
        os << "end " << quad.arg1;
        break;
    case Quad::PARAM:
        os << "param " << quad.arg1;
        break;
    case Quad::RETURN:
        os << "return " << quad.arg1;
        break;
    case Quad::GOTO:
        os << "goto " << quad.result;
        break;
    case Quad::GOTOLT: case Quad::GOTOLE: case Quad::GOTOGT:
    case Quad::GOTOGE: case Quad::GOTOEQ: case Quad::GOTONE:
        os << "if " << quad.arg1 << " " << Quad::op2name.at(quad.op) << " " << quad.arg2
           << " goto "<< quad.result;
        break;
    case Quad::LABEL:
        os << quad.result << ":";
        break;
    case Quad::DEFVAR:
        os << "var " << quad.arg1;
        break;
    case Quad::DEFVEC:
        os << "var " << quad.arg2 << " " << quad.arg1;
        break;
    case Quad::STMTCALL:
        os << "call " << quad.arg1;
        break;
    case Quad::NOP:
        break;
    }

    return os;
}

std::ostream& operator<<(std::ostream &os, const std::list<Quad> &quads)
{
    bool inFunc = false;
    
    for (auto quad: quads) {
        if (quad.op == Quad::ENDF) inFunc = false;
        if (inFunc && quad.op != Quad::LABEL) os << "    ";
        os << quad << std::endl;
        if (quad.op == Quad::DEFF) inFunc = true;
    }
    
    return os;
}


const std::map<Quad::Operation, std::string> Quad::op2name = {
    {NOT, "!"}, {NEG, "-"}, {EQ, "=="}, {IEQ, "!="}, {LT, "<"}, {GT, ">"},
    {AND, "&&"}, {OR, "||"}, {PLUS, "+"}, {MINUS, "-"}, {MULT, "*"}, {DIV, "/"},
    {REM, "%"}, {CALL, "call "}, {ASSIGN, ""},
    {GOTOLT, "<"}, {GOTOGE, ">="}, {GOTOGT, ">"}, {GOTOLE, "<="},
	{GOTOEQ, "=="}, {GOTONE, "!="}, {SL, "<<"}, {SR, ">>"}
};

const std::map<TreeNode::OpType, Quad::Operation> Quad::op2op = {
    {TreeNode::NEG, NEG}, {TreeNode::NOT, NOT}, {TreeNode::MULT, MULT},
    {TreeNode::DIV, DIV}, {TreeNode::REM, REM}, {TreeNode::PLUS, PLUS},
    {TreeNode::MINUS, MINUS}, {TreeNode::LT, LT}, {TreeNode::GT, GT},
    {TreeNode::EQ, EQ}, {TreeNode::IEQ, IEQ}, {TreeNode::AND, AND},
    {TreeNode::OR, OR} 
};

const std::map<Quad::Operation, Quad::Operation> Quad::op2cond = {
    {LT, GOTOLT}, {GT, GOTOGT}, {EQ, GOTOEQ}, {IEQ, GOTONE}
};

const std::map<Quad::Operation, Quad::Operation> Quad::reverse = {
    {GOTOLT, GOTOGE}, {GOTOLE, GOTOGT}, {GOTOGT, GOTOLE}, {GOTOGE, GOTOLT},
    {GOTONE, GOTOEQ}, {GOTOEQ, GOTONE}
};

