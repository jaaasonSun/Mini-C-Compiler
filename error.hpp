#ifndef ERRORH
#define ERRORH

#include <string>
#include <iostream>

enum Error {
    LEXICALERROR,
    SYNTAXERROR,
    IOERROR,
    PARSERBUG,
    QUADBUG,
    REGALLOCERROR,
    DATAFLOWBUG
};

#endif
