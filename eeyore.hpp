#include <iostream>
#include <string>
#include <map>
#include <list>

#ifndef EEYOREH
#define EEYOREH

#include "parse.hpp"
#include "utility.hpp"

class Quad {
public:
    class Address {
    public:
        enum AddType { NAME, CONST } type;
        std::string name;
        int constant;
        Address(int _c): type(CONST), constant(_c){}
        Address(std::string _n): type(NAME), name(_n){}
        Address(Address const &_a): type(_a.type), name(_a.name),
                                    constant(_a.constant){}

        operator std::string() {
            if (type == NAME) return name;
            return std::to_string(constant);
        }
    };
    Address arg1, arg2;
    std::string result;
    enum Operation {
        // unary op
        NOT, NEG, CALL, ASSIGN,
        // normal binary op
        EQ, IEQ, LT, GT, AND, OR,
        PLUS, MINUS, MULT, DIV, REM,
        SR, SL,
        // binary
        SETVEC, GETVEC,
        // jump
        GOTO, GOTOLT, GOTOGT, GOTOLE, GOTOGE, GOTOEQ, GOTONE,
        // misc
        DEFVAR, DEFVEC,
        DEFF, ENDF, PARAM, RETURN, STMTCALL,
        LABEL,
        // place holder op for convinience
        NOP
    } op;
    const static std::map<Operation, std::string> op2name;

    template<class T1, class T2>
    Quad(std::string _r, T1 _a1, Operation _o, T2 _a2):
        arg1(_a1), arg2(_a2), result(_r), op(_o){}    
    
    static void buildQuad(std::list<Quad>&, TreeNode&);
    static void buildStmt(std::list<Quad>&, TreeNode&);
    static void buildFunc(std::list<Quad>&, TreeNode&);
    static void buildExpr(std::list<Quad>&, TreeNode&);
    static void buildIfWhile(std::list<Quad>&, TreeNode&);
    static void buildIfElse(std::list<Quad>&, TreeNode&);
    static void buildCall(std::list<Quad>&, TreeNode&);
        
    static void printSequence(std::list<Quad>&);

    const static std::map<TreeNode::OpType, Operation> op2op;
    const static std::map<Operation, Operation> op2cond;
    const static std::map<Operation, Operation> reverse;

    static unsigned labelCounter;
    static unsigned tmpCounter;
};

bool operator==(Quad::Address const &add1, Quad::Address const &add2);
bool operator!=(Quad::Address const &add1, Quad::Address const &add2);

std::ostream& operator<<(std::ostream&, const Quad::Address&);
std::ostream& operator<<(std::ostream&, const Quad&);
std::ostream& operator<<(std::ostream&, const std::vector<Quad>&);

#define DEFTMP(n) (seq.push_back(Quad("", TMPPREFIX+std::to_string(tmpCounter+n), Quad::DEFVAR, 0)))

#define POPTMPDEF {                             \
        auto tmp = seq.back();                  \
        seq.pop_back();                         \
        seq.pop_back();                         \
        seq.push_back(tmp);                     \
    }

#endif
