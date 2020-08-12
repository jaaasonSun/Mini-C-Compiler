#include <set>
#include <string>
#include <vector>
#include <map>
#include <list>
#include <queue>
#include <bitset>

#include "analysis.hpp"
#include "error.hpp"

#ifndef DAG_H
#define DAG_H

class DAG {
    using Var = std::bitset<MAX_VAR>;
    
    struct Node {
        
        Quad::Address address;
        std::set<std::string> alias;

        Node(int _c): address(_c), alias({}){}
        Node(std::string _n): address(_n), alias({}){} // alias DO NOT include address
        Node(Node const &n): address(n.address), alias(n.alias), isOP(n.isOP),
                             op(n.op), arg1(n.arg1), arg2(n.arg2), arg0(n.arg0),
                             dependency(n.dependency), usedBy(n.usedBy),
                             destroy(n.destroy), live(n.live) {}
        
        bool isOP = false;
        Quad::Operation op;
        Node *arg1 = NULL, *arg2 = NULL, *arg0 = NULL; // arg0 used specifically in []=

        std::set<Node*> dependency = {};
        std::set<Node*> usedBy = {};
        std::set<Node*> destroy = {};
        Node* destroyedBy = NULL;

        bool live = true;
        bool emitted = false;

        bool removed = false;
        bool isUnused() {
            if (removed) return true;
            if (isOP) switch (op) {
                case Quad::CALL: case Quad::SETVEC: case Quad::STMTCALL:
                case Quad::PARAM:  return false;
                default: break;
                }
            
            if (!usedBy.size()) return true;
            for (auto n: usedBy) if (!n->removed) return false;
            return true;
        }

        Quad emit();
    };

    std::vector<Quad> frontQuads; // for defs

    std::vector<Node*> nodes;
    std::vector<Node*> liveNodes;
    
    Node *makeNode(std::string);
    Node *makeNode(int);
    Node* makeNode(Quad::Address);
    Node* killNode(std::string);
    static bool compareNode(Node*, Quad);

public:
    static DAG buildDAG(Block*, std::set<std::string>);
    std::list<Quad> buildQuads(Var liveOut, std::map<std::string, size_t>);
    void eliminateDeadNode(Var, std::map<std::string, size_t>);
    void printDAG(std::ostream&);
};

#endif
