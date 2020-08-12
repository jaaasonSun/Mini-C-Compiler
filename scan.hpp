#include <string>
#include <map>

#ifndef SCANH
#define SCANH

class Token {
public:
    enum TokenType {
        ENDFILE, ERROR,
        // reserved
        IF, ELSE, WHILE, RETURN, INT,
        // operator
        PLUS, MINUS, MULT, DIV, REM, NOT, AND, OR, COMMA,
        LT, GT, EQ, IEQ, ASSIGN,
        // special symbol
        LPAREN, RPAREN, LCURL, RCURL, LSQUR, RSQUR, SEMI,
	// token with value	
        ID, NUM
    };
	
    Token(TokenType _t, std::string _s, int _i, size_t _l):
	type(_t), strValue(_s), intValue(_i), lineNo(_l){}
    Token() = default;
	
    static TokenType reserveLookup(std::string);
    static TokenType charLookup(char);

    static std::string displayToken(TokenType);

    TokenType type;
    std::string strValue;
    long long intValue;
    size_t lineNo;
private:
    const static std::map<std::string, TokenType> reserveWord;
    const static std::map<char, TokenType> char2Token;
    const static std::map<TokenType, std::string> dispToken;
};



class Scanner {
    enum StateType {
        START, DONE,
        INSLASH, INEQUAL, INEXCL, INAMPR, INVERT,
        INCOMMENT, INNUM, INID
    };
	
    const static std::map<char, StateType> char2state;
	
    std::istream &is;
    int lineNo;
    std::string currentLine;
    size_t lineLength;
    size_t linePos;
    char getNextChar();
    void revertChar();
    bool reserveLookup();
public:
    Scanner(std::istream &_is): is(_is), lineNo(0), currentLine(""),
                               lineLength(0), linePos(0){}
    Token getToken();
};

#endif

#ifdef DEBUG
#undef DEBUG
#endif
