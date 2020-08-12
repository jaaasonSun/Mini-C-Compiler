#include <iostream>
#include <fstream>
#include <vector>
#include <unistd.h>

#include "error.hpp"
#include "symtab.hpp"
#include "eeyore.hpp"
#include "codeGen.hpp"
#include "analysis.hpp"

void printHelp()
{
    std::cout << "Usage:\n";
    std::cout << "\t-b <binary> [-i [output]] [-O] [-o <output>] [-h]\n";
}

int main(int argc, char* argv[])
{
    opterr = 0;
    char c;
    bool optimizeFlag = false;
    bool intermediateFlag = false;
    char *intermediateFile = NULL;
    char *binaryFile = NULL;
    char *outFile = NULL;
	
    while ((c = getopt(argc, argv, "i:h:b:Oo:")) != -1) {
        switch (c) {
        case 'b':
            binaryFile = optarg;
            break;
        case 'h':
            printHelp();
            return 0;
        case 'i':
            intermediateFlag = true;
            intermediateFile = optarg;
            break;
        case 'O':
            optimizeFlag = true;
            break;
        case 'o':
            outFile = optarg;
            break;
        case '?':
            if (optopt == 'b') std::cerr << "no binary speified\n";
            if (optopt == 'i') intermediateFlag = true;
            else std::cerr << "unknown option " << (char)optopt << std::endl;
            break;
        default:
            break;
        }
    }
	
    if (!binaryFile) {
        std::cerr << "no binary specified\n";
        return -1;
    }
	
    std::ifstream input(binaryFile);
    Parser parser(input);
    auto tree = parser.buildTree();
    SymTabNode::buildSymTab(tree, NULL, NULL);

    std::list<Quad> quads;
    Quad::buildQuad(quads, tree);
	
    if (optimizeFlag) analysis(&quads);
	
    if (intermediateFlag) {
        auto quadOut = std::ofstream(intermediateFile ? intermediateFile : "a.ee");
        for (auto quad: quads) quadOut << quad << std::endl;
    }
	
    std::ofstream of(outFile ? outFile : "a.s");
    Inst::conversion(quads, of);
	
    return 0;
}


