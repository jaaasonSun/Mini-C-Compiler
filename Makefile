CXX = clang++
CXXFLAGS = -std=c++11
DEBUGFLAG =

SRC = scan.cpp parse.cpp symtab.cpp eeyore.cpp codeGen.cpp interval.cpp analysis.cpp dataFlow.cpp DAG.cpp
DRIVER = driver.cpp

MAIN = minicc
OBJS = $(SRC:.cpp=.o)
HEADER = $(SRC:.cpp=.h)

all: $(MAIN)

$(MAIN): $(OBJS) $(DRIVER)
	$(CXX) $(CXXFLAGS) $(OBJS) $(DRIVER) $(DEBUGFLAG) -o $(MAIN)

.cpp.o: $< $(HEADER)
	$(CXX) $(CXXFLAGS) $(DEBUGFLAG) -c $< -o $@

clean:
	$(RM) *.o $(MAIN)
