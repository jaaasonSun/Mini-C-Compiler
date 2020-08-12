#include <string>
#include <iostream>

//#define DEBUG

#include "parse.hpp"
#include "error.hpp"
#include "utility.hpp"

#define AUTO_NODE(TYPE) auto node = TreeNode(current.lineNo, TreeNode:: TYPE)


TreeNode Parser::buildTree()
{
    return goal();
}

TreeNode Parser::goal()
{
    ENTER("goal");
    AUTO_NODE(GOAL);
    while(current.type != Token::ENDFILE)
        node.child.push_back(global());
    return node;
}

TreeNode Parser::global()
{
    ENTER("global");
    auto node = TreeNode(current.lineNo);
    
    match(Token::INT);
    node.child.push_back(TreeNode(current.lineNo, matchID()));
    
    switch(current.type) {
    case Token::SEMI:
        node.type = TreeNode::VARDEF;
        discard();
        return node;
    case Token::LSQUR:
        node.type = TreeNode::VARDEF;
        discard();
        node.child.push_back(TreeNode(current.lineNo, matchNUM()));
        match(Token::RSQUR);
        match(Token::SEMI);
        return node;
    case Token::LPAREN:
        discard();
        node.child.push_back(fArg());
        match(Token::RPAREN);
        switch(current.type) {
        case Token::SEMI:
            node.type = TreeNode::FDECL;
            discard();
            return node;
        case Token::LCURL:
            node.type = TreeNode::FDEF;
            discard();
            node.child.push_back(stmts());
            match(Token::RCURL);
            return node;
        default:
            syntaxError();
        }
        break;
    default:
        syntaxError();
        break;
    }
    throw SYNTAXERROR;
}


TreeNode Parser::varDecl()
{
    ENTER("varDecl");
    AUTO_NODE(VARDECL);
    match(Token::INT);
    node.child.push_back(TreeNode(current.lineNo, matchID()));
    switch(current.type) {
    case Token::LSQUR:
        discard();
        node.child.push_back(TreeNode(current.lineNo, matchNUM()));
        match(Token::RSQUR); //fallthrough
    default:
        return node;
    }
    
}


TreeNode Parser::varDef()
{
    ENTER("varDef");
    auto node = varDecl();
    node.type = TreeNode::VARDEF;
    match(Token::SEMI);
    return node;
}


TreeNode Parser::fArg()
{
    ENTER("fArg");
    AUTO_NODE(FARG);
    while(current.type != Token::RPAREN) {
        node.child.push_back(varDecl());
        if (current.type == Token::COMMA) discard();
    }
    return node;
}


TreeNode Parser::stmts()
{
    ENTER("stmts");
    AUTO_NODE(STMTS);
    while(current.type != Token::RCURL) {
        node.child.push_back(stmt());
    }
    return node;
}

TreeNode Parser::stmt()
{
    ENTER("stmt");
    AUTO_NODE(STMT);
    node.stmt = TreeNode::stmtLookup(current.type);

    switch(current.type) {
    case Token::LCURL:
        discard();
        node.child.push_back(stmts());
        match(Token::RCURL);
        return node;
    case Token::SEMI:
        discard();
        return node;
    case Token::WHILE:
        discard();
        match(Token::LPAREN);
        node.child.push_back(expr());
        match(Token::RPAREN);
        node.child.push_back(stmt());
        return node;
    case Token::ID:
        node.child.push_back(TreeNode(current.lineNo, matchID()));
        switch(current.type) {
        case Token::ASSIGN:
            discard();
            node.child.push_back(expr());
            match(Token::SEMI);
            return node;
        case Token::LSQUR:
            node.stmt = TreeNode::VECASSIGN;
            discard();
            node.child.push_back(expr());
            match(Token::RSQUR);
            match(Token::ASSIGN);
            node.child.push_back(expr());
            match(Token::SEMI);
            return node;
        case Token::LPAREN:
            node.stmt = TreeNode::SFCALL;
            discard();
            node.child.push_back(fExpr());
            match(Token::RPAREN);
            match(Token::SEMI);
            return node;
        default:
            syntaxError();
        }
        break;
    case Token::INT:
        node = varDef();
        // node.child.push_back(varDef());
        return node;
    case Token::RETURN:
        discard();
        node.child.push_back(expr());
        match(Token::SEMI);
        return node;
    case Token::IF:
        discard();
        match(Token::LPAREN);
        node.child.push_back(expr());
        match(Token::RPAREN);
        node.child.push_back(stmt());
        if (current.type == Token::ELSE) {
            node.stmt = TreeNode::IFELSE;
            discard();
            node.child.push_back(stmt());
        }
        return node;
    default:
        syntaxError();
    }
    throw SYNTAXERROR;
}


inline TreeNode Parser::expr()
{
    ENTER("expr");
    return e8();
}

#define EXPR_N(N, cur, next) TreeNode Parser:: cur ()            \
    {                                                           \
        auto node = next();                                     \
        auto op = TreeNode::opLookup(current.type);              \
                                                                \
        while (TreeNode::opPrecedence(N, op)) {                  \
            discard();                                          \
            auto lchild = node;                                 \
            auto rchild = next();                               \
            node = TreeNode(current.lineNo, TreeNode::EXPR);     \
            node.op = op;                                       \
            node.child.push_back(lchild);                       \
            node.child.push_back(rchild);                       \
            op = TreeNode::opLookup(current.type);               \
        }                                                       \
        return node;                                            \
    }

EXPR_N(8, e8, e7);
EXPR_N(7, e7, e6); // semicolon is only here to make emacs’ indentation happy
EXPR_N(6, e6, e5);
EXPR_N(5, e5, e4);
EXPR_N(4, e4, e3);
EXPR_N(3, e3, e2);

TreeNode Parser::e2()
{
    ENTER("e2");
    AUTO_NODE(EXPR);
    switch(current.type) {
    case Token::NOT: node.op = TreeNode::NOT; break;
    case Token::MINUS: node.op = TreeNode::NEG; break;
    default: return e1();
    }
    discard();
    node.child.push_back(e1());
    return node;
}

TreeNode Parser::e1()
{
    ENTER("e1");
    auto child = e0();
    if (current.type == Token::LSQUR) {
        AUTO_NODE(EXPR);
        node.op = TreeNode::VECACCESS;
        discard();
        node.child.push_back(child);
        node.child.push_back(expr());
        match(Token::RSQUR);
        return node;
    }
    return child;
}

TreeNode Parser::e0()
{
    ENTER("e0");
    AUTO_NODE(EXPR);
    TreeNode idNode;
    switch(current.type) {
    case Token::NUM:
        return TreeNode(current.lineNo, matchNUM());
    case Token::ID:
        idNode = TreeNode(current.lineNo, matchID());
        if (current.type != Token::LPAREN) return idNode;    
        node.op = TreeNode::FCALL;
        node.child.push_back(idNode);
        discard();
        node.child.push_back(fExpr());
        match(Token::RPAREN);
        return node;
    case Token::LPAREN:
        discard();
        node = expr();
        match(Token::RPAREN);
        return node;
    default:
        syntaxError();
    }
    throw SYNTAXERROR;
}

TreeNode Parser::fExpr()
{
    ENTER("fExpr");
    AUTO_NODE(FEXPR);
    while(current.type != Token::RPAREN) {
        node.child.push_back(expr());
        if (current.type == Token::COMMA) discard();
    }
    return node;
}

std::ostream& operator<<(std::ostream &os, const TreeNode &node)
{
    os << TreeNode::nt2str.find(node.type)->second;
    switch (node.type) {
    case TreeNode::STMT:
        os << ":" << TreeNode::stmt2str.find(node.stmt)->second;
        break;
    case TreeNode::EXPR:
        os << ":" << TreeNode::op2str.find(node.op)->second;
        break;
    case TreeNode::NUM:
        os << ":" << node.value;
        break;
    case TreeNode::ID:
        os << ":" << node.id << ", " << node.canonicalName;
        break;
    default: break;
    }
    return os;
}

void Parser::printTree(const TreeNode &node)
{
    traverse(node);
}

void Parser::traverse(const TreeNode &tree)
{
    static std::vector<bool> levels = {};

    for (auto it = levels.begin(); it != levels.end(); ++it) {
        if (it+1 == levels.end()) {
            if (*it) std::cout << " ├───";
            else std::cout << " └───";
        } else {
            if (*it) std::cout << " │   ";
            else std::cout << "     ";
        }
    }
    std::cout << " " << tree <<std::endl;
    
    for (auto it = tree.child.begin(); it != tree.child.end(); ++it) {
        levels.push_back(it+1 != tree.child.end());
        traverse(*it);
        levels.pop_back();
    }
    return;
}


// node helper

const std::map<TreeNode::NodeType, std::string> TreeNode::nt2str = {
    {TreeNode::GOAL, "Goal"}, {TreeNode::VARDEF, "Var Def"},
    {TreeNode::FDEF, "Func Def"}, {TreeNode::FDECL, "Func Declare"},
    {TreeNode::FARG, "Arg Declare"}, {TreeNode::VARDECL, "Var Declare"},
    {TreeNode::STMTS, "Stmts"},  {TreeNode::STMT, "Stmt"},
    {TreeNode::EXPR, "Expr"}, {TreeNode::NUM, "Num"},
    {TreeNode::ID, "Id"},  {TreeNode::FEXPR, "Arg Expr"}
};

const std::map<TreeNode::OpType, std::string> TreeNode::op2str = {
    {TreeNode::FCALL, "()"}, {TreeNode::VECACCESS, "[]"},
    {TreeNode::NEG, "(-)"}, {TreeNode::NOT, "!"},
    {TreeNode::MULT, "*"}, {TreeNode::DIV, "/"}, {TreeNode::REM, "%"},
    {TreeNode::PLUS, "+"}, {TreeNode::MINUS, "-"}, {TreeNode::LT, "<"},
    {TreeNode::GT, ">"}, {TreeNode::EQ, "=="}, {TreeNode::IEQ, "!="},
    {TreeNode::AND, "&&"}, {TreeNode::OR, "||"}, {TreeNode::TMPOP, "?"}, 
};

const std::map<TreeNode::StmtType, std::string> TreeNode::stmt2str = {
    {TreeNode::BLOCK, "Block"}, {TreeNode::EMPTY, ";"},
    {TreeNode::WHILE, "While"}, {TreeNode::IF, "If"},
    {TreeNode::IFELSE, "IfElse"}, {TreeNode::VARASSIGN, "="},
    {TreeNode::VECASSIGN, "[]="}, {TreeNode::RETURN, "Return"},
    {TreeNode::VAR, "Var Def"}, {TreeNode::SFCALL, "();"},
    {TreeNode::TMPSTMT, "?"}
};

const std::map<Token::TokenType, TreeNode::StmtType> TreeNode::token2stmt = {
    {Token::LCURL, BLOCK}, {Token::SEMI, EMPTY}, {Token::WHILE, WHILE},
    {Token::ID, VARASSIGN}, {Token::INT, VAR}, {Token::RETURN, RETURN},
    {Token::IF, IF}
};

TreeNode::StmtType TreeNode::stmtLookup(Token::TokenType token)
{
    auto it = token2stmt.find(token);
    return (it != token2stmt.end()) ? it->second : TMPSTMT;
}

const std::map<Token::TokenType, TreeNode::OpType> TreeNode::token2op = {
    {Token::AND, AND}, {Token::OR, OR}, {Token::EQ, EQ}, {Token::IEQ, IEQ},
    {Token::LT, LT}, {Token::GT, GT}, {Token::PLUS, PLUS}, {Token::MINUS, MINUS},
    {Token::MULT, MULT}, {Token::DIV, DIV}, {Token::REM, REM}
};

TreeNode::OpType TreeNode::opLookup(Token::TokenType token)
{
    auto it = token2op.find(token);
    return (it != token2op.end()) ? it->second : TMPOP;
}

const std::map<int, std::set<TreeNode::OpType>> TreeNode::precedence = {
    {8, {OR}}, {7, {AND}}, {6, {EQ, IEQ}}, {5, {LT, GT}}, {4, {PLUS, MINUS}},
    {3, {MULT, DIV, REM}}
};

bool TreeNode::opPrecedence(int N, OpType op)
{
    auto &ops = precedence.at(N);
    return ops.find(op) != ops.end();
}

// parser helper

inline void Parser::match(Token::TokenType expect)
{
#ifdef DEBUG
    std::cout << "match: " << Token::displayToken(expect);
#endif
    if (expect == current.type){
#ifdef DEBUG
        std::cout << " matched" << std::endl;
#endif
        current = getToken();
        return;
    }
#ifdef DEBUG
    std::cout << " but get" << Token::displayToken(current.type) << std::endl;
#endif
    syntaxError();
    throw SYNTAXERROR;
}

inline std::string Parser::matchID()
{
    auto id = current.strValue;
    match(Token::ID);
    return id;
}

inline int Parser::matchNUM()
{
    int num = current.intValue;
    match(Token::NUM);
    return num;
}

inline void Parser::syntaxError(std::string msg)
{
    std::cout << "Syntax error on line " << current.lineNo << std::endl;
    if (msg != "") std::cout << msg << std::endl;
}
