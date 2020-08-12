#include <string>
#include <iostream>
#include <map>

#include "scan.hpp"
#include "error.hpp"

const std::map<char, Scanner::StateType> Scanner::char2state = {
    {'/', INSLASH}, {'=', INEQUAL}, {'!', INEXCL}, {'&', INAMPR}, {'|', INVERT}
};

char Scanner::getNextChar()
{
    // read inpt file in a line-by-line manner to better deal with newlines
    if (linePos == lineLength) {
        if (!getline(is, currentLine).good()) {
            if (is.eof()) return EOF;
            else {
                std::cout << "IO error on parsing input: line "
                         << lineNo << std::endl;
                throw IOERROR;
            }
        }
        linePos = 0;
        lineNo++;
        lineLength = currentLine.length();
        return '\n'; // return \n to indicate new line
    } else return currentLine[linePos++];
}

void Scanner::revertChar()
{
    if (linePos) linePos--;
    // if a newline is just read, revert \n has no effect
}

Token Scanner::getToken()
{
    Token::TokenType current;
    StateType state = START;
	
    std::string idValue = "";
    int intValue = 0;
    std::map<char, StateType>::const_iterator it;
	
    while (state != DONE) {
        char c = getNextChar();
        switch(state) {
        case START:
            if (c == EOF) {
                current = Token::ENDFILE;
                state = DONE;
            } else if (isspace(c)) state = START;
            else if (isdigit(c)) {
                state = INNUM;
                intValue = c - '0';
            } else if (isalpha(c)) {
                state = INID;
                idValue += c;
            } else {
                it = char2state.find(c);
                if (it != char2state.end()) // multi-char operator / = ! & |
                    state = it->second;
                else { // single-char operator + - * etc, done here
                    current = Token::charLookup(c); // ERROR if not exsit
                    state = DONE;
                }
            }
            break;
        case INID:
            if (!isalnum(c)) {
                revertChar();
                state = DONE;
                current = Token::ID;
            } else idValue += c;
            break;
        case INNUM:
            if (!isdigit(c)) {
                revertChar();
                state = DONE;
                current = Token::NUM;
            } else intValue = intValue*10 + (c-'0');
            break;
        case INCOMMENT:
            if (c == '\n') state = START; //getToken() reports a newline as \n
            break;
        case INSLASH:
            if (c == '/') state = INCOMMENT;
            else {
                revertChar();
                state = DONE;
                current = Token::DIV;
            }
            break;
        case INEQUAL:
            if (c == '=') current = Token::EQ;
            else {
                current = Token::ASSIGN;
                revertChar();
            }
            state = DONE;
            break;
        case INEXCL:
            if (c == '=') current = Token::IEQ;
            else {
                current = Token::NOT;
                revertChar();
            }
            state = DONE;
            break;
        case INAMPR:
            if (c == '&') current = Token::AND;
            else current = Token::ERROR;
            state = DONE;
            break;
        case INVERT:
            if (c == '|') current = Token::OR;
            else current = Token::ERROR;
            state = DONE;
            break;
        case DONE: // case DONE should not be reachable
            std::cout << "Scanner had a bug when scanning line "
                      << lineNo << std::endl;
        }
    }
    if (current == Token::ID) current = Token::reserveLookup(idValue);
#ifdef LEX_DEBUG
    static size_t debugLineNo = 1;
    if (lineNo > debugLineNo) {
        std::cout << std::endl;
        debugLineNo = lineNo;
    }
    switch(current) {
    case Token::ID: std::cout << "ID:" << idValue << " "; break;
    case Token::NUM: std::cout << "NUM:" << intValue << " "; break;
    default: std::cout << Token::displayToken(current) << " "; break;
    }
#endif
    if (current == Token::ERROR) throw LEXICALERROR;
    return Token(current, idValue, intValue, lineNo);
}



// helper functions

const std::map<std::string, Token::TokenType> Token::reserveWord = {
    {"if", IF}, {"else", ELSE}, {"while", WHILE}, {"return", RETURN},
    {"int", INT}
};

const std::map<char, Token::TokenType> Token::char2Token = {
    {'+', PLUS}, {'-', MINUS}, {'*', MULT}, {'%', REM}, {'<', LT}, {'>', GT},
    {'(', LPAREN}, {')', RPAREN}, {'{', LCURL}, {'}', RCURL}, {'[', LSQUR},
    {']', RSQUR}, {';', SEMI}, {',', COMMA}
};

const std::map<Token::TokenType, std::string> Token::dispToken = {
    {ENDFILE, "ENDFILE"}, {ERROR, "ERROR"}, {IF, "IF"}, {ELSE, "ELSE"},
    {WHILE, "WHILE"}, {RETURN, "RETURN"}, {INT, "INT"},
    {PLUS, "+"}, {MINUS, "-"}, {MULT, "*"}, {DIV, "/"}, {REM, "%"}, {NOT, "!"},
    {AND, "&&"}, {OR, "||"}, {COMMA, ","}, {LT, "<"}, {GT, ">"}, {EQ, "=="},
    {IEQ, "!="}, {ASSIGN, "="}, {LPAREN, "("}, {RPAREN, ")"}, {LCURL, "{"},
    {RCURL, "}"}, {LSQUR, "["}, {RSQUR, "]"}, {SEMI, ";"}, {ID, "ID"},
    {NUM, "NUM"}
};

std::string Token::displayToken(TokenType type)
{
    auto it = dispToken.find(type);
    return (it != dispToken.end()) ? it->second : "ERROR";
}

Token::TokenType Token::reserveLookup(std::string str)
{
    auto it = reserveWord.find(str);
    return (it != reserveWord.end()) ? it->second : ID;
}

Token::TokenType Token::charLookup(char c)
{
    auto it = char2Token.find(c);
    return (it != char2Token.end()) ? it->second : ERROR;
}
