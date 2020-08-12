#include <vector>
#include <map>
#include <set>
#include <iostream>

#define DEBUG

#ifndef PARSEH
#define PARSEH

#include "scan.hpp"
#include "error.hpp"
#include "utility.hpp"

class TreeNode {
public:
    enum NodeType {
        // globals
        GOAL, VARDEF, FDEF, FDECL,
        // function definition
        FARG, VARDECL,
        // statements
        STMTS, STMT,
        // expressions
        EXPR, NUM, ID,
        // function call argument
        FEXPR
    };
    enum OpType {
        FCALL, VECACCESS, NEG, NOT,
        MULT, DIV, REM, PLUS, MINUS,
        LT, GT, EQ, IEQ, AND, OR,
        TMPOP
    };
    enum StmtType {
        BLOCK, EMPTY, WHILE, IF, IFELSE,
        VARASSIGN, VECASSIGN, RETURN, VAR,
        TMPSTMT, SFCALL
    };
    
    static StmtType stmtLookup(Token::TokenType);
    static OpType opLookup(Token::TokenType);
    static bool opPrecedence(int, OpType);

    TreeNode(size_t _l, NodeType _n): lineNo(_l), type(_n){}
    TreeNode(size_t _l, std::string _id): lineNo(_l), type(ID), id(_id){}
    TreeNode(size_t _l, int _i): lineNo(_l), type(NUM), value(_i){}
    TreeNode(size_t _l): lineNo(_l){}
    TreeNode() = default;
    
    std::vector<TreeNode> child;

    size_t lineNo;
    NodeType type;
    OpType op;
    StmtType stmt;

    // if NUM
    int value;

    // if ID
    std::string id;
    // canonical name is generated for Eeyore by symtable
    std::string canonicalName;

    const static std::map<Token::TokenType, StmtType> token2stmt;
    const static std::map<Token::TokenType, OpType> token2op;
    const static std::map<int, std::set<OpType>> precedence;

    const static std::map<TreeNode::NodeType, std::string> nt2str;
    const static std::map<TreeNode::OpType, std::string> op2str;
    const static std::map<TreeNode::StmtType, std::string> stmt2str;
};


std::ostream& operator<<(std::ostream&, const TreeNode&);


class Parser {
    Scanner scanner;

    TreeNode goal();
    TreeNode global();
    TreeNode varDecl();
    TreeNode varDef();
    TreeNode fArg();

    TreeNode stmts();
    TreeNode stmt();

    TreeNode expr();
    TreeNode e8();
    TreeNode e7();
    TreeNode e6();
    TreeNode e5();
    TreeNode e4();
    TreeNode e3();
    TreeNode e2();
    TreeNode e1();
    TreeNode e0();

    TreeNode fExpr();

    TreeNode identifier();
    TreeNode number();

    void syntaxError(std::string msg = "");

    inline Token getToken() { return scanner.getToken();}
    void discard() {
#ifdef DEBUG
        std::cout << "discard " << Token::displayToken(current.type) << std::endl;
#endif
        current = getToken();}
    
    void match(Token::TokenType);
    std::string matchID();
    int matchNUM();

    Token current;

    static void traverse(const TreeNode&);

public:
    Parser(std::istream &_is): scanner(_is) { current = getToken();}
    TreeNode buildTree();
    static void printTree(const TreeNode&);
};


#endif
