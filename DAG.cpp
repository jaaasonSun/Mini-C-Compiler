#include "DAG.hpp"

void DAG::printDAG(std::ostream &os)
{
    for (auto n: nodes) {
        os << n << std::endl;
        if (n->address.type == Quad::Address::CONST)
            os << "value: " << n->address.constant;
        else {
            os << "name: " << n->address.name;
            os << " alias: ";
            for (auto a: n->alias) os << a << " ";
        }
        os << (n->live ? " live " : " dead ")
           << (n->removed ? "" : " not ") << "removed" << std::endl;
        os << "depend on: ";
        for (auto d: n->dependency) os << d << " ";
        os << " used by: ";
        for (auto d: n->usedBy) os << d << " ";
        os << " destroy: ";
        for (auto d: n->destroy) os << d << " ";
        os << " destroyed by: " << n->destroyedBy << std::endl;;
    }
}


#define ADDDEP(newNode, baseNode) {                     \
        (newNode)->dependency.insert((baseNode));        \
        (baseNode)->usedBy.insert((newNode));}

#define ADDDES(newNode, baseNode) {                     \
        if (baseNode) {                                 \
            (newNode)->destroy.insert((baseNode));       \
            (baseNode)->destroyedBy = (newNode);}}

#define ADDGLOBAL(node) {                                       \
        if ((node)->address.type == Quad::Address::NAME &&           \
            globals.find((node)->address.name) != globals.end())  \
            globalNodes.insert((node));                         \
    }

DAG::Node* DAG::makeNode(std::string name)
{
    for (auto &node: liveNodes)
        if (node->address.name == name) {
            std::cerr << "makeNode: node found " << name << std::endl;
            return node;
        }
        else if (node->alias.find(name) != node->alias.end()) {
            std::cerr << "makeNode: alias found " << name << std::endl;
            return node;
        }
	
    auto node = new DAG::Node(name);
    nodes.push_back(node);
    liveNodes.push_back(node);
    std::cerr << "makeNode: make node " << name
             << (node->live?"":"not") << " alive" << std::endl;
    return node;
};

DAG::Node* DAG::makeNode(int value)
{
    // node with constant is always alive but not in live set
    auto node = new DAG::Node(value);
    node->live = true;
    nodes.push_back(node);
    std::cerr << "makeNode: make node " << value
             << (node->live?"":"not") << " alive" << std::endl;
    return node;
}

DAG::Node* DAG::makeNode(Quad::Address add)
{
    if (add.type == Quad::Address::CONST)
        return makeNode(add.constant);
    else return makeNode(add.name);
}

DAG::Node* DAG::killNode(std::string name)
{
    auto it = liveNodes.begin();
    while (it != liveNodes.end()) {
        if ((*it)->address.type == Quad::Address::NAME) {
            if ((*it)->address.name == name) {
                std::cerr << "killNode: kill node " << name << std::endl;
                (*it)->live = false;
                if ((*it)->alias.size()) {
                    // if killed node has other alias
                    // create a new node that gets value and alias from it
                    auto newName = *(*it)->alias.begin();
                    Node *newNode = new Node(newName);
                    
                    newNode->alias = (*it)->alias;
                    newNode->alias.erase(newName);
                    newNode->isOP = true;
                    newNode->op = Quad::ASSIGN;
                    newNode->arg1 = *it;
                    ADDDEP(newNode, *it);

                    (*it)->alias.clear(); // for convenience

                    nodes.push_back(newNode);
                    liveNodes.push_back(newNode);
                    std::cerr << "killNode: make new node " << newName << std::endl;
                }
                auto node = *it;
                liveNodes.erase(it);
                return node;
            } else if ((*it)->alias.find(name) != (*it)->alias.end()) {
                std::cerr << "killNode: kill alias " << name << std::endl;
                // if matched an alias, simply remove the alias
                (*it)->alias.erase(name);
                return NULL;
            }
        }
        it++;
    }
    return NULL;
}

bool DAG::compareNode(Node* node, Quad quad)
{
    // node with different operation obviously cannot be used
    if (!node->isOP || node->op != quad.op) return false;

    int childCount = 0;
    switch (quad.op) {
    case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
        childCount = 1;
        break;
    case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT: case Quad::AND:
    case Quad::OR: case Quad::PLUS: case Quad::MINUS: case Quad::MULT:
    case Quad::DIV: case Quad::REM: case Quad::GETVEC: case Quad::STMTCALL:
    case Quad::CALL: case Quad::SL: case Quad::SR:
        childCount = 2;
        break;
    case Quad::SETVEC:
        childCount = 3;
        break;
    default:
        throw QUADBUG;
    }
    // node with dead child cannot be reused
    // this part is stupidly designed and is to do with code design in Quad
    if (childCount >= 1) {
        if (!node->arg1->live) return false;
        if (quad.arg1.type == Quad::Address::CONST) {
            if (node->arg1->address.constant != quad.arg1.constant)
                return false;
        } else {
            if (node->arg1->address.name != quad.arg1.name &&
                node->arg1->alias.find(quad.arg1.name) == node->arg1->alias.end())
                return false;
        }
    }
	
    if (childCount >= 2) {
        if (!node->arg2->live) return false;
        if (quad.arg2.type == Quad::Address::CONST) {
            if (node->arg2->address.constant != quad.arg2.constant)
                return false;
        } else {
            if (node->arg2->address.name != quad.arg2.name &&
                node->arg2->alias.find(quad.arg2.name) == node->arg2->alias.end())
                return false;
        }
    }
    // this only happens for []=
    if (childCount >= 3) {
        if (!node->arg0->live) return false;
        if (node->arg0->address.name != quad.result) return false;
    }
    return true;
}

std::list<Quad> DAG::buildQuads(Var liveOut, std::map<std::string, size_t> sym2index)
{
    std::list<Quad> quads;

    // replace destroy/destroyedBy relationship with depend/use
    // a node depend on all node that uses the node it klled
    // unless usedBy itself
    for (auto n: nodes)
        for (auto d: n->destroy)
            for (auto u: d->usedBy)
                if (u != n)
                    ADDDEP(n, u);

    // first emit all defs
    quads.insert(quads.end(), frontQuads.begin(), frontQuads.end());
    
    // a node can be emitted if all dependency is emitted
    // this is a very inefficient algorithm
    // hopefully not too many nodes will be in a block
    size_t nodeCount = nodes.size(), emitted = 0;
    while (emitted < nodeCount) {
        for (auto n: nodes) {
            if (n->emitted) continue;
            bool emit = true;
            if (!n->removed && n->isOP) {
                for (auto d: n->dependency)
                    emit &= d->emitted;
                if (emit) {
                    quads.push_back(n->emit());
                    // alias in a not killed node is also be alive if in liveOut
                    // a killed node do not have alias since they are transfered
                    // to the new node created from it
                    // nees to emit a ’alias = name’
                    for (auto alias: n->alias)
                        if (liveOut.test(sym2index[n->address.name]))
                            quads.push_back(Quad(alias, n->address.name, Quad::ASSIGN, 0));
                }
            }
            if (emit) {                
                n->emitted = true;
                emitted++;
            }
        }
    }

    return quads;
}

Quad DAG::Node::emit()
{
    switch (op) {
    case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
        return Quad(address.name, arg1->address, op, 0);
    case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT: case Quad::AND:
    case Quad::OR: case Quad::PLUS: case Quad::MINUS: case Quad::MULT:
    case Quad::DIV: case Quad::REM: case Quad::SL: case Quad::SR:
    case Quad::STMTCALL: case Quad::CALL: case Quad::GETVEC:
        return Quad(address.name, arg1->address, op, arg2->address);
    case Quad::PARAM:
        return Quad("", arg1->address, op, 0);
    case Quad::SETVEC:
        return Quad(arg0->address, arg1->address, op, arg2->address);
    default:
        throw QUADBUG;
    }
}

DAG DAG::buildDAG(Block* block, std::set<std::string> globals)
{
    DAG dag;
    qit it = block->bBegin();
    Node *funcNode = NULL; // previous CALL/PARAM, used to resolve argument
    Node *funcCall = NULL; // previous CALL, used to resolve globals
    std::set<Node*> globalNodes;
    std::map<std::string, Node*> setVecs; // previous SETVEC for all vec
    
    // params and calls have a predefined order
    // need to preserve in the form of dependency
    while (it != block->bEnd()) {
        std::cerr << "buildDAG: " << *it << std::endl;
        switch (it->op) {
        case Quad::LABEL: case Quad::NOP:
            it++;
            continue;
            // label in the middle of a block is useless
        case Quad::DEFVAR: case Quad::DEFVEC:
            dag.frontQuads.push_back(*it);
            it++;
            continue;
        case Quad::DEFF: case Quad::ENDF: case Quad::GOTO: case Quad::GOTOLT:
        case Quad::GOTOGT: case Quad::GOTOLE: case Quad::GOTOGE: case Quad::GOTOEQ:
        case Quad::GOTONE: case Quad::RETURN:
            throw QUADBUG;
        default: {
            bool found = false;
            for (auto node: dag.liveNodes)
                if (compareNode(node, *it)) {
                    Node *killed = dag.killNode(it->result);
                    if (node->address.name != it->result)
                        node->alias.insert(it->result);
                    ADDDES(node, killed);
                    found = true;
                    std::cerr << "buildDAG: Same node found with name "
                             << node->address.name << std::endl;
                    break;
                }
            if (found) break;
            std::cerr << "buildDAG: No node found" << std::endl;
            Node *newNode = new Node(it->result);
            newNode->isOP = true;
            newNode->op = it->op;

            bool arg1 = true, arg2 = true;
            bool result = true;
            bool addLive = true;

            switch (it->op) {
            case Quad::NOT: case Quad::NEG: case Quad::ASSIGN:
                arg2 = false;
                break;
            case Quad::EQ: case Quad::IEQ: case Quad::LT: case Quad::GT: case Quad::AND:
            case Quad::OR: case Quad::PLUS: case Quad::MINUS: case Quad::MULT:
            case Quad::DIV: case Quad::REM: case Quad::SL: case Quad::SR:
                break;
            case Quad::PARAM:
                arg2 = false;
                result = false;
                addLive = false;
                if (funcNode) ADDDEP(newNode, funcNode);
                funcNode = newNode;
                break;
            case Quad::STMTCALL:
                result = false;
                addLive = false;
                // intentional fallthrough
            case Quad::CALL:
                if (funcNode) ADDDEP(newNode, funcNode);
                funcNode = newNode;
					
                // a function call use global value
                if (funcCall) for (auto n: globalNodes) ADDDEP(n, funcCall);
                for (auto n: globalNodes) ADDDEP(newNode, n);
                globalNodes.clear();
                funcCall = newNode;
                // then destroy it
                for (auto name: globals) {
                    Node *g = dag.killNode(name);
                    ADDDES(newNode, g);
                }
                break;
            case Quad::GETVEC: {
                auto lit = setVecs.find(it->result);
                if (lit != setVecs.end()) ADDDEP(newNode, lit->second);
            }
                break;
            case Quad::SETVEC: {
                auto killed = dag.killNode(it->result);
                ADDDES(newNode, killed);
                newNode->arg0 = dag.makeNode(it->result);
                ADDDEP(newNode, newNode->arg0);
                newNode->live = false;
                setVecs[it->result] = newNode;
                result = false;
                addLive = false;
            }
                break;
            default:
                throw QUADBUG;
            }

            if (arg1) {
                newNode->arg1 = dag.makeNode(it->arg1);
                if (it->op == Quad::CALL || it->op == Quad::STMTCALL)
                    dag.killNode(it->arg1.name); // need to kill func node to avoid reuse
                ADDGLOBAL(newNode->arg1);
                ADDDEP(newNode, newNode->arg1);
            }
            if (arg2) {
                newNode->arg2 = dag.makeNode(it->arg2);
                ADDGLOBAL(newNode->arg2);
                ADDDEP(newNode, newNode->arg2);
            }
            if (result) {
                Node* killed = dag.killNode(it->result);
                if (killed) ADDDES(newNode, killed);
                ADDGLOBAL(newNode);
            }
            
            if (addLive) dag.liveNodes.push_back(newNode);
            else newNode->live = false;
            dag.nodes.push_back(newNode);
            
        }
            break;
        }
        it++;
    }
    if (funcCall) for (auto n: globalNodes) ADDDEP(n, funcCall);

    return dag;
}


#define CAN_BE_REMOVED(n) (!(n)->removed && (n)->isUnused() &&              \
                           ((n)->address.type != Quad::Address::NAME ||      \
                            !liveOut.test(sym2index[(n)->address.name]) || \
                            !(n)->live))                                 \
    
void DAG::eliminateDeadNode(Var liveOut, std::map<std::string, size_t> sym2index)
{
    // nodes killed without being used is dead
    // nodes not killed and not in liveOut without being used is dead

    for (auto n: nodes) {
        if (CAN_BE_REMOVED(n)) {
            std::queue<Node *> removeQueue;
            removeQueue.push(n);
            
            while(removeQueue.size()) {
                auto r = removeQueue.front();
                removeQueue.pop();
                
                bool remove = true;
                for (auto alias: r->alias) {
                    if (liveOut.test(sym2index[alias])) {
                        // if an alias is alive, change the address to alias instead
                        remove = false;
                        r->alias.erase(alias);
                        r->address.name = alias;
                        break;
                    }
                }
                if (!remove) continue;
                
                r->removed = true;
                
                for (auto u: r->dependency)
                    if (CAN_BE_REMOVED(u))
                        removeQueue.push(u);
            }
        }
    }
}
