#include <vector>
#include <deque>
#include <list>
#include <set>
#include <algorithm>

#ifndef INTERVALH
#define INTERVALH

typedef long pos;

class Interval {
public:
    struct CompInterval {
        bool operator() (Interval * const &i1, Interval * const &i2) const {
            return i1->firstRange().from < i2->firstRange().from;
        }
    };
    
    int regNum = -1, assignedReg = -1;
    struct Range {
        pos from, to;
        Range() = default;
        Range(pos f, pos t): from(f), to(t){}
    };
    std::list<Range> rangeList;
    std::list<pos> usePosition;

    Interval *splitParent = NULL;
    std::set<Interval*, CompInterval> splitChild;

    Interval *regHint; //?

    bool fixed = false;
    int spillSlot = -1;
	int inheritedSlot = -1;
    int spillFrom = -1, loadTo = -1; // register
    int spillTo = -1, loadFrom = -1; // spillSlot
    
    std::set<pos> splitPos;

    void addRange(pos, pos);
    void addRangeBack(pos, pos);
    Range const& firstRange() const;
    Range& firstRange();
    Range const& lastRange() const;
    bool hasRange() const;
	
    void addUse(pos);
    void addUseBack(pos);
    pos firstUse();
    Interval* split(pos);
    bool cover(pos) const;
    bool coverReverse(pos) const;
    pos intersect(Interval*) const;
    Interval* childAt(pos);
    Interval* childAtReverse(pos);
};

std::ostream& operator<<(std::ostream&, Interval::Range const&);

std::ostream& operator<<(std::ostream&, Interval const&);

#endif
