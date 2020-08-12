#include <iostream>
#include <stack>

#include "symtab.hpp"
#include "utility.hpp"
#include "error.hpp"

//#define DEBUG

#ifdef DEBUG
#define ENTER(NAME) debugEnter(NAME)
#define DEBUGPRINT(str) debugPrint(str)
#else
#define ENTER(NAME)
#define DEBUGPRINT(str)
#endif

const std::string VARPREFIX = "T";
const std::string ARGPREFIX = "p";

void debugPrint(std::string str)
{
    std::cout << str;
}

SymTabNode* SymTabNode::buildSymTab(
    TreeNode &parseTree, SymTabNode *prev, SymTabNode *par,
    std::vector<SymTabNode*> fArg)
{
    ENTER("buildSymTab\n");
    auto base = new SymTabNode(prev, par);

    static std::stack<unsigned> varCount({0});
    varCount.push(varCount.top()); // count start from upper level so names do not overlap
    
    SymTabNode* pre = NULL;
    // copy function arguments to current scope
    for (auto symPtr: fArg) {
        DEBUGPRINT("> function arg ");
        DEBUGPRINT(symPtr->identifier);
        DEBUGPRINT("\n");
        base->symbols.push_back(symPtr);
        symPtr->previous = pre;
        symPtr->parent = base;
        pre = symPtr;
    }

    for (auto &stmt: parseTree.child) {
        DEBUGPRINT("> child\n");
        switch (stmt.type) {
        case TreeNode::VARDEF: {
            DEBUGPRINT("> vardef ");
            DEBUGPRINT(stmt.child[0].id);
            auto t = stmt.child.size() == 1 ? SymTabNode::SYMVAR : SymTabNode::SYMVEC;
            auto varDef = new SymTabNode(pre, base, t, stmt.child[0].id);
            // set the cannonical name in symtable (for reference)
            // and parse tree (for code gen)
            varDef->canonicalName = GENNAME(VARPREFIX, varCount.top()++);
            stmt.child[0].canonicalName = varDef->canonicalName;
            base->symbols.push_back(varDef);

            // search only in this scope so make sure pre is not NULL
            if (pre && findPrev(stmt.child[0].id, pre, false)) {
                // search redefinition in current scope
                std::cout << "Redefinition of symbol \"" << stmt.child[0].id
                         <<  "\"  on line " << stmt.lineNo << std::endl;
                throw SYNTAXERROR;
            }
            
            break;
        }
        case TreeNode::FDECL: {
            DEBUGPRINT("> fdecl ");
            DEBUGPRINT(stmt.child[0].id);
            auto func = new SymTabNode(pre, base, SymTabNode::SYMFUNCDECL, stmt.child[0].id);
            for (auto &arg: stmt.child[1].child) {
                // check for function argument type (var/vec)
                auto t = arg.child.size() == 1 ? SymTabNode::SYMVAR : SymTabNode::SYMVEC;
                auto newArg = new SymTabNode(NULL, NULL, t, arg.child[0].id);
                // and make a tmp SymTabNode for easier checking
                // pre and par does not matter here
                func->functionArgs.push_back(newArg);
            }
            
            if (pre && findPrev(stmt.child[0].id, pre, false)) {
                std::cout << "Redefinition of symbol \"" << stmt.child[0].id
                         <<  "\"  on line " << stmt.lineNo << std::endl;
                throw SYNTAXERROR;
            }

            base->symbols.push_back(func);
            
            break;
        }
            
        case TreeNode::FDEF: {
            DEBUGPRINT("> fdef ");
            DEBUGPRINT(stmt.child[0].id);

            auto func = new SymTabNode(pre, base, SymTabNode::SYMFUNCDEF, stmt.child[0].id);

            int argCount = 0;
            
            for (auto &arg: stmt.child[1].child) {
                auto t = arg.child.size() == 1 ? SymTabNode::SYMVAR : SymTabNode::SYMVEC;
                auto newArg = new SymTabNode(NULL, NULL, t, arg.child[0].id);
                newArg->canonicalName = GENNAME(ARGPREFIX, argCount++);
                arg.child[0].canonicalName = newArg->canonicalName;
                func->functionArgs.push_back(newArg);
            }

            // check for previous declaration, see if arguments match
            if (pre) {
                auto dup = findPrev(stmt.child[0].id, pre, false);
                if (dup) {
                    if (dup->type == SYMFUNCDECL) {
                        if (!checkFuncArg(dup, func->functionArgs)) {
                            std::cout << "Definition of function \"" << stmt.child[0].id
                                     << "\" on line " << stmt.lineNo
                                     << " does not match previous declaration." <<std::endl;
                            throw SYNTAXERROR;
                        }
                    } else throw SYNTAXERROR;
                }
                
            }

            base->symbols.push_back(func);
            base->symbols.push_back(
                // pass argument list to subscope so arguments would be in subscope
                buildSymTab(stmt.child[2], func, base, func->functionArgs));
            
            break;
        }
            
        case TreeNode::STMTS:
            DEBUGPRINT("> stmts\n");
            base->symbols.push_back(buildSymTab(stmt, pre, base));
            break;

        case TreeNode::STMT:
            checkStmt(base, pre, stmt);
            break;
            
        case TreeNode::EXPR:
            CHECKEXPR(pre, base, stmt);
            break;
            
        default:
#ifdef DEBUG
            std::cout << "case: " << stmt.type << std::endl;
            std::cout << TreeNode::GOAL << " " << TreeNode::FARG << " "
                     << TreeNode::VARDECL << " " << TreeNode::NUM << " "
                     << TreeNode::ID << " " << TreeNode::FEXPR << std::endl;
#endif
            std::cout << "Unreachable case in buildsymtab" << std::endl;
            throw PARSERBUG;
        }

        if (base->symbols.size()) pre = base->symbols.back();
    }

    // leave this scope
    varCount.pop();
    return base;
}

void SymTabNode::checkStmt(SymTabNode* base,SymTabNode* pre, TreeNode& stmt)
{
    DEBUGPRINT("> stmt\n");
    switch (stmt.stmt) {
    case TreeNode::BLOCK:
        base->symbols.push_back(buildSymTab(stmt.child[0], pre, base));
        break;
        
    case TreeNode::WHILE: case TreeNode::IF: case TreeNode::IFELSE:
        CHECKEXPR(pre, base, stmt.child[0]);
        checkStmt(base, pre, stmt.child[1]);
        if (stmt.stmt == TreeNode::IFELSE)
            checkStmt(base, pre, stmt.child[2]);
        break;
        
    case TreeNode::VARASSIGN: {
        DEBUGPRINT(">> varAssign\n");
        auto def = pre ? findPrev(stmt.child[0].id, pre, true) :
            findPrev(stmt.child[0].id, base, true);
        CHECKUNDEF(def, stmt.child[0].id, stmt.lineNo)
            CHECKTYPE(def, stmt.child[0].id, stmt.lineNo, SymType::SYMVAR);
        CHECKEXPR(pre, base, stmt.child[1]);
        // set the canonical name for variable in parse tree
        stmt.child[0].canonicalName = def->canonicalName;
        break;
    }
        
    case TreeNode::VECASSIGN: {
        DEBUGPRINT(">> vecAssign\n");
        auto def = pre ? findPrev(stmt.child[0].id, pre, true) :
            findPrev(stmt.child[0].id, base, true);
        CHECKUNDEF(def, stmt.child[0].id, stmt.lineNo)
            CHECKTYPE(def, stmt.child[0].id, stmt.lineNo, SymType::SYMVEC);
        CHECKEXPR(pre, base, stmt.child[1]);
        CHECKEXPR(pre, base, stmt.child[2]);
        stmt.child[0].canonicalName = def->canonicalName;
        break;
    }
        
    case TreeNode::RETURN:
        CHECKEXPR(pre, base, stmt.child[0]);
        break;
        
    case TreeNode::SFCALL:
        pre ? checkFuncCall(pre, stmt) : checkFuncCall(base, stmt);
        break;

    case TreeNode::EMPTY:
        break;
        
    default:
        std::cout << "Unreachable case in STMT" << std::endl;
        throw PARSERBUG;
    }
}

SymTabNode* SymTabNode::findPrev(std::string id, SymTabNode* cur, bool recursive)
{
    ENTER("findPrev");
    DEBUGPRINT(id);
    DEBUGPRINT("\n");

    while (cur && cur->identifier != id) {
        DEBUGPRINT(cur->identifier);
        cur = cur->previous ? cur->previous : (recursive ? cur->parent : NULL);
    }
    return cur;
}

bool SymTabNode::checkFuncArg(SymTabNode* prev, std::vector<SymTabNode*>& args)
{
    ENTER("checkFuncArg");
    auto it1 = prev->functionArgs.begin();
    auto it2 = args.begin();
    
    while (it1 != prev->functionArgs.end() && it2 != args.end())
        if ((*it1++)->type != (*it2++)->type) return false;

    if (it1 != prev->functionArgs.end() || it2 != args.end()) return false;
    return true;
}

void SymTabNode::checkExpr(SymTabNode* prev, TreeNode& expr, bool noVec)
{
    ENTER("checkExpr");
    // prev here is either (semantically) "previous" or "parent"
    // caller have to decide
    switch (expr.type) {
    case TreeNode::EXPR:
        DEBUGPRINT("> expr\n");
        switch (expr.op) {
        case TreeNode::FCALL:
            DEBUGPRINT(">> fcall\n");
            checkFuncCall(prev, expr);
            break;
        case TreeNode::VECACCESS: {
            DEBUGPRINT(">> vecaccess\n");
            auto def = findPrev(expr.child[0].id, prev, true);
            
            CHECKUNDEF(def, expr.child[0].id, expr.lineNo) else if (def->type != SymTabNode::SYMVEC) {
                std::cout << "Vector access on scalar variable \"" << expr.id
                         << "\" on line" << expr.lineNo << std::endl;
                throw SYNTAXERROR;
            }
            checkExpr(prev, expr.child[1], true);
            expr.child[0].canonicalName = def->canonicalName;
            break;
        }
        default: // normal operations, vec is not allowed
            DEBUGPRINT(">> other\n");
            for (auto &sub: expr.child) checkExpr(prev, sub, true);
            break;
        }
        break;
    case TreeNode::ID:{
        DEBUGPRINT("> ID\n");
        auto def = findPrev(expr.id, prev, true);
        CHECKUNDEF(def, expr.id, expr.lineNo);  
        if (noVec && def->type == SYMVEC) {
            std::cout << "Attempt to operate on vector symbol \"" << expr.id
                     <<  "\" on line " << expr.lineNo << std::endl;
            throw SYNTAXERROR;
        }
        expr.canonicalName = def->canonicalName;
        break;
    }
    case TreeNode::NUM:
        DEBUGPRINT("> NUM\n");
        break;
    default:
        std::cout << "Unreachable case in checkExpr" << std::endl;
        throw PARSERBUG;
    }
}

void SymTabNode::checkFuncCall(SymTabNode* prev, TreeNode& stmt)
{
    ENTER("checkFuncCall");
    // check for function itself
    auto def = findPrev(stmt.child[0].id, prev, true);
    CHECKUNDEF(def, stmt.child[0].id, stmt.lineNo);

    // build a tmp vector for type check
    // only type matters
    std::vector<SymTabNode*> args;
    for (auto &arg: stmt.child[1].child) {
        if (arg.type == TreeNode::ID) {
            auto idDef = findPrev(arg.id, prev, true);
            CHECKUNDEF(idDef, arg.id, arg.lineNo);
            // automatically distinguish between vec and var
            args.push_back(idDef);
            arg.canonicalName = idDef->canonicalName;
        } else {
            if (arg.type == TreeNode::EXPR) checkExpr(prev, arg);
            // no pointer operation allowed, so all expr are vars
            // also memory leak :/
            auto newArg = new SymTabNode(NULL, NULL, SymTabNode::SYMVAR, "");
            args.push_back(newArg);   
        }
    }

    // check arguments against declaration
    if (!checkFuncArg(def, args)) {
        std::cout << "Argument of function \"" << stmt.child[0].id
                 << "\" on line " << stmt.lineNo
                 << " does not match previous declaration." <<std::endl;
        throw SYNTAXERROR;
    }
}

void SymTabNode::traverse(SymTabNode* node)
{
    static int level = 0;
    level++;
    if (node->type == SymTabNode::SUBSCOPE)
        for (auto child: node->symbols)
            traverse(child);
    else
        std::cout << level << " " << node->canonicalName << std::endl;
    level--;
}
