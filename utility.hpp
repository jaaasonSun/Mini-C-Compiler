#ifndef UTILITYH
#define UTILITYH

//#define DEBUG

#include <string>
#include <iostream>

#define GENNAME(prefix, num) (prefix+std::to_string(num))

#ifdef DEBUG
#define ENTER(NAME) debugEnter(NAME)
#define LEAVE(NAME) debugLeave(NAME)
#else
#define ENTER(NAME)
#define LEAVE(NAME)
#endif

inline void debugEnter(std::string name)
{
    std::cout << " Enter " << name << std::endl;
}

inline void debugLeave(std::string name)
{
    std::cout << " Leave " << name << std::endl;
}

#endif
