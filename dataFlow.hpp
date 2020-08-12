#include <set>
#include <map>
#include <bitset>

#ifndef DATAFLOW_H
#define DATAFLOW_H

#include "analysis.hpp"

template<class Value, class BlockInfo>
class DataFlow {
protected:
    enum Direction {
        FORWARD, BACKWARD
    } direction;
 
    Value top, edgeValue;
    std::vector<Block*> blocks;

    virtual Value meet(Value const&, Value const&) = 0;
    virtual Value trans(Block*, Value const&) = 0;

    using InOut = std::pair<Value, Value>;
    std::map<size_t, InOut> inout;
    
public:

    DataFlow(Direction _d, Value _t, Value _e, std::vector<Block*> _b):
        direction(_d), top(_t), edgeValue(_e), blocks(_b) {}
    
	virtual std::map<size_t, InOut> analyse();
    virtual void apply() {}
};


struct Symbol {
    enum State {
        CONSTANT, NAC, UNDEFINED
    } state = UNDEFINED;
    int value = 0;

    Symbol() = default;
    Symbol(State _s, int _v = 0):state(_s), value(_v) {}
};

bool operator==(Symbol const &s1, Symbol const &s2);

using symVec = std::vector<Symbol>;

class ConstantFolding: public DataFlow<symVec, symVec> {
    
    std::map<std::string, size_t> sym2index;
    std::map<size_t, std::string> index2sym;
    std::set<std::string> const globals;

	int unaryCal(Quad::Operation op, int opr);
	int binaryCal(Quad::Operation op, int opr1, int opr2);
    
public:
    ConstantFolding(std::map<std::string, size_t> &s,
                    std::map<size_t, std::string> &i,
                    std::vector<Block*> &blocks,
                    std::set<std::string> const &g):
        DataFlow<symVec, symVec>(FORWARD, symVec(s.size()),
                                 symVec(s.size()), blocks),
        sym2index(s), index2sym(i), globals(g) {}

    // this function is (unfortuately) similar to trans()
    // but I have no idea how to share code between them
	void apply();

private:   
	symVec meet(symVec const &c1, symVec const &c2);
	symVec trans(Block *b, symVec const &vec);
};

using Vars = std::bitset<MAX_VAR>;

class LiveAnalysis: public DataFlow<Vars, Vars> {

    std::map<std::string, size_t> sym2index;
    std::map<size_t, std::string> index2sym;
    std::set<std::string> const globals;

    std::map<size_t, Vars> gen, kill; // index -> live info

public:

    LiveAnalysis(std::map<std::string, size_t> &s,
                    std::map<size_t, std::string> &i,
                    std::vector<Block*> &blocks,
                    std::set<std::string> const &g):
        DataFlow<Vars, Vars>(BACKWARD, Vars(), buildExitValue(s, g), blocks),
        sym2index(s), index2sym(i), globals(g) {
        for (auto b: blocks) calcGenKill(b);
    }
    
private:

    Vars buildExitValue(std::map<std::string, size_t> &sym2index,
						std::set<std::string> const &globals) const;
	void genVar(Block *block, Quad::Address add);
	void killVar(Block *block, std::string name);
	void calcGenKill(Block *block);

	Vars meet(Vars const &v1, Vars const &v2);
	Vars trans(Block *b, Vars const &v);
};
    

#endif
