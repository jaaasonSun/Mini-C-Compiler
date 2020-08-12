#include "interval.hpp"
#include <iostream>

bool operator<(Interval::Range const &r1, Interval::Range const &r2)
{ return r1.from < r2.from; }

bool operator<(Interval const &i1, Interval const &i2)
{ return i1.firstRange().from < i2.firstRange().from; }

// use push_front so later ranges (subsequently, with lower ID) is in the front
void Interval::addRange(pos from, pos to) { rangeList.push_front(Range(from, to)); }
void Interval::addRangeBack(pos from, pos to) { rangeList.push_back(Range(from, to)); }

Interval::Range& Interval::firstRange() { return *rangeList.begin(); }
Interval::Range const& Interval::firstRange() const { return *rangeList.begin(); }
Interval::Range const& Interval::lastRange() const { return *rangeList.rbegin(); }
bool Interval::hasRange() const {return !rangeList.empty(); }

void Interval::addUse(pos use) { usePosition.push_front(use); }
void Interval::addUseBack(pos use) { usePosition.push_back(use); }
pos Interval::firstUse()
{
	if (usePosition.empty()) return lastRange().to;
	return *std::min_element(usePosition.begin(), usePosition.end());
	
}

// assuming ranges are ordered
// assuming uses are ordered (for possible future use)
Interval* Interval::split(pos position)
{
    auto child = new Interval;
	
	std::cerr << "split interval at " << position << std::endl;

    auto range = rangeList.begin();
    while(range != rangeList.end()) {
        if (range->from >= position) {
            child->addRangeBack(range->from, range->to);
            auto tmp = range;
            range++;
            rangeList.erase(tmp);
        } else {
            if (range->to > position) {
                child->addRangeBack(position, range->to);
                range->to = position;
            } 
            range++;
        }
    }

    auto use = usePosition.begin();
    while (use != usePosition.end()) {
        if (*use > position) {
            child->addUseBack(*use);
            auto tmp = use;
            use++;
            usePosition.erase(tmp);
        } else use++;
    }

    if (child->usePosition.empty() && child->rangeList.empty()) return NULL;

	if (spillSlot != -1) child->inheritedSlot = spillSlot;
	else if (inheritedSlot != -1) child->inheritedSlot = inheritedSlot;
	
    auto p = this;
    while (p->splitParent) p = p->splitParent;
    p->splitChild.insert(child);
    child->splitParent = p;

    p->splitPos.insert(position);
    
    return child;
}

bool Interval::cover(pos position) const
{
    for (auto range: rangeList)
        if (range.from <= position && range.to > position) return true;
		else if (range.from == range.to && range.to == position) return true;
    return false;
}

bool Interval::coverReverse(pos position) const
{
	// cover reverse is used for input registers
	// since all ranges are in the form of [from, to)
	// to is typically the input position if register.
	// e.g. two range [from, to) and [to, to2), if reg is input register at
	// position to, [from, to) covers it, else [to, to2) covers it
	// one problem is [from, from) could cause trouble
	// everything is fine until trying to locate this interval by position
    for (auto range: rangeList)
        if (range.from < position && range.to >= position) return true;
		else if (range.from == range.to && range.to == position) return true;
    return false;
}

// again, assuming ranges are ordered
// return -1 if not intersect, otherwise return intersect position
pos Interval::intersect(Interval *that) const
{
	if (rangeList.empty() || that->rangeList.empty()) return false;
    auto thisR = rangeList.begin();
    auto thatR = that->rangeList.begin();

    while (thisR != rangeList.end() && thatR != that->rangeList.end()) {
        if (thisR->from >= thatR->to) thatR++;
        else if (thisR->to <= thatR->from) thisR++;
        else return std::max(thisR->from, thatR->from);
    }
    return -1;
}

Interval* Interval::childAt(pos id)
{
    if (cover(id)) return this;
    for (auto child: splitChild) if (child->cover(id)) return child;
    return NULL;
}

// including to, excluding from
Interval* Interval::childAtReverse(pos id)
{
    if (coverReverse(id)) return this;
    for (auto child: splitChild) if (child->coverReverse(id)) return child;
    return NULL;
}

std::ostream& operator<<(std::ostream &os, Interval::Range const &r)
{
	return os << "[" << r.from << ", " << r.to << ")";
}

std::ostream& operator<<(std::ostream &os, Interval const &itv)
{
	os  << "assigned reg " << itv.assignedReg
		<< ", spill slot " << itv.spillSlot << std::endl;
	os << "ranges: ";
	for (auto range: itv.rangeList) os << range << " ";
	os << std::endl << "uses: ";
	for (auto use: itv.usePosition) os << use << " ";
	if (!itv.splitChild.empty()) {
		os << std::endl << "split child: " << std::endl;
		for (auto child: itv.splitChild) os << *child;
	}
	return os << std::endl;
}
