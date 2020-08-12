#include <vector>
#include <string>
#include <iostream>

#include "parse.hpp"

#ifndef SYMH
#define SYMH

class SymTabNode {
public:
    enum SymType { SUBSCOPE, SYMVAR, SYMVEC, SYMFUNCDEF, SYMFUNCDECL };

    SymType type;

    // symbol
    std::string identifier, canonicalName;
    std::vector<SymTabNode*> functionArgs;
    
    // sub scope
    std::vector<SymTabNode*> symbols;
    
    SymTabNode *previous, *parent;

    SymTabNode(SymTabNode *_p, SymTabNode *_pa):
        type(SUBSCOPE), previous(_p), parent(_pa){}
    SymTabNode(SymTabNode *_p, SymTabNode *_pa, SymType _t, std::string _s):
        type(_t), identifier(_s), previous(_p), parent(_pa){}
    
    static SymTabNode* buildSymTab(TreeNode &, SymTabNode *, SymTabNode *,
                           std::vector<SymTabNode*> fArg = {});
    static SymTabNode* findPrev(std::string, SymTabNode*, bool);
    static bool checkFuncArg(SymTabNode*, std::vector<SymTabNode*>&);
    static void checkExpr(SymTabNode*, TreeNode&, bool noVec = false);
    static void checkStmt(SymTabNode*, SymTabNode*, TreeNode&);
    static void checkFuncCall(SymTabNode*, TreeNode&);

    static void traverse(SymTabNode* node);
};


#define CHECKUNDEF(def, id, lineNo)             \
    /* if pre is NULL, search directly from parent node */                \
    if (!def) {                                                         \
        std::cout << "Undefined symbol \"" << id << "\" on line"            \
                 << lineNo << std::endl;                                   \
        throw SYNTAXERROR;                                              \
    }

#define CHECKTYPE(def, id, lineNo, expectedType)        \
    else if (def->type != expectedType) {                                 \
        std::cout << "Type of symbol \"" << id << "\" on line "             \
                 << lineNo << " does not match definition" << std::endl;    \
        throw SYNTAXERROR;                                              \
    }
#define CHECKEXPR(pre, par, treeNode) \
    pre ? checkExpr(pre, treeNode) : checkExpr(par, treeNode)

#endif
