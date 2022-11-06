%option noyywrap
%{
    #include <iostream>
    #include <vector>
    #include <regex>
	#include <string>

    using namespace std;

    extern "C" int yylex();

    enum {Number = 1, Fraction, Comment, EOP};

    struct Position {
        int line, column, index;

        Position() {}

        Position(int _line, int _column, int _index): line(_line), column(_column), index(_index) {}

        void print() {
            cout << line << ":" << column << ":" << index;
        }
    };

    struct Frac {
        unsigned long long a, b;
        Frac() {}
        ~Frac() {}
        Frac(unsigned long long _a, unsigned long long _b): a(_a), b(_b) {}
        friend ostream &operator<<(ostream &out, const Frac &f) {
            out << f.a << "/" << f.b;
            return out;
        }
        Frac& operator=(Frac obj) {
            if (this == &obj) {
                return *this;
            }
            this->a = obj.a;
            this->b = obj.b;
            return *this;
        }
    };

    union Value {
        string comment;
        unsigned long long digit;
        Frac fraction;
        Value(): comment(""){}
        ~Value(){}
    };

    struct Token {
        Position start, finish;
        int kind;
        Value val;
        void print() {
            if (kind == Number) {
                cout << "NUMBER ";
            } else if (kind == Fraction) {
                cout << "FRACTION ";
            } else if (kind == EOP) {
                cout << "EOF ";
            }
            cout << kind << " ";
            start.print();
            cout << "-"; 
            finish.print();
            cout << " ";
            if (kind == Comment) {
                cout << val.comment;
            } else if (kind == Number) {
                cout << val.digit;
            } else if (kind == Fraction) {
                cout << val.fraction;
            }
            cout << endl;
        }
        Token(){}
        Token(const Token& t) {
            start = t.start;
            finish = t.finish;
            kind = t.kind;
            if (kind == 3) {
                val.comment = string(t.val.comment);
            } else if (kind == 1) {
                val.digit = t.val.digit;
            } else if (kind == 2) {
                val.fraction = t.val.fraction;
            }
        }
        ~Token() {
            if (kind == Fraction) {
                val.fraction.~Frac();
            } else if (kind == Comment) {
                val.comment.~string();
            }
        }
    };

    struct Error {
        Position start, finish;
        string msg;
        Error(){}
        Error(Position _start, Position _finish, string _msg): start(_start), finish(_finish), msg(_msg){}
        void print() {
            start.print();
            cout << "-";
            finish.print();
            cout << " " << msg << endl;
        }
    };

    vector <Token> tokens;
    vector <pair<Position, Position>> commentsPos;
    vector <Error> errors;
    Position start, finish;
    Position current(1, 1, 1);

#define YY_USER_ACTION { \
    start = current; \
    for (auto xxtext = yytext; xxtext[0]; xxtext++) { \
        if (xxtext[0] == '\n') { \
            current.line++; \
            current.column = 0; \
        } \
        current.column++; \
        current.index++; \
	} \
    finish = current; \
}
%}
DIGIT [1-9][0-9]*
FRACTION {DIGIT}\/{DIGIT}
%x COMMENT1
%%
{FRACTION} {
    unsigned long long a = 0, b = 0;
    bool first = true;
    string yystr(yytext);
    for (auto c : yystr) {
        if (c == '/') {
            first = false;
            continue;
        }
        if (first) {
            a *= 10;
            a += (c - '0');
        } else {
            b *= 10;
            b += (c - '0');
        }
    }
    Frac fr(a, b);
    Token tok;
    tok.start = start;
    tok.finish = finish;
    tok.kind = Fraction;
    tok.val.fraction = fr;
    tokens.push_back(tok);
}
{DIGIT} {
    Token tok;
    tok.start = start;
    tok.finish = finish;
    tok.kind = Number;
    tok.val.digit = stoll(yytext);
    tokens.push_back(tok);
}
(\(\*)|\{ {
    BEGIN(COMMENT1);
}
<COMMENT1>[^(\*\))\}]*((\*\))|\}) {
    string yystr(yytext);
    if (yystr[yystr.size()-1]=='}') {
        finish.index -= 1;
        if (finish.column > 0)
            finish.column -= 1;
    } else {
        finish.index -= 2;
        if (finish.column > 1)
            finish.column -= 2;
    }
    commentsPos.push_back(make_pair(start, finish));
    BEGIN(0);
}
<COMMENT1>.|[ \n\t] {
    errors.push_back(Error(start, finish, "not closed comment"));
}
[ \n\t]
<<EOF>> {
    Token tok;
    finish.index++;
    finish.column++;
    tok.start = finish;
    tok.finish = finish;
    tok.kind = EOP;
    tokens.push_back(tok);
    return 0;
}
. {
    errors.push_back(Error(start, finish, "unexpected symbol"));
}
%%
int main() {
    int tag = -1;
    while (tag != 0) {
        tag = yylex();
    }

    cout << "TOKENS:\n";
    for (auto t : tokens) {
        t.print();
    }

    cout << "COMMENTS COORDS:\n";
    for (auto p : commentsPos) {
        p.first.print();
        cout << "-";
        p.second.print();
        cout << endl;
    }

    cout << "ERRORS:\n";
    for (auto p : errors) {
        p.print();
    }

    return 0;
}
