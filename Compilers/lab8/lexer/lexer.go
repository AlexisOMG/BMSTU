package lexer

import (
	"io/ioutil"
	"regexp"
)

var (
	// axiomKeywordReg = regexp.MustCompile(`^\$AXIOM`)
	ntermKeywordReg = regexp.MustCompile(`^\$NTERM`)
	termKeywordReg  = regexp.MustCompile(`^\$TERM`)
	ruleKeywordReg  = regexp.MustCompile(`^\$RULE`)
	// epsKeywordReg   = regexp.MustCompile(`^\$EPS`)
	ntermReg       = regexp.MustCompile(`^[A-Z][^ \n]*`)
	termReg        = regexp.MustCompile(`^"[^ \n]+"`)
	equalReg       = regexp.MustCompile(`^=`)
	alternativeReg = regexp.MustCompile(`^\|`)
	open1Reg       = regexp.MustCompile(`^\(`)
	close1Reg      = regexp.MustCompile(`^\)`)
	open2Reg       = regexp.MustCompile(`^\{`)
	close2Reg      = regexp.MustCompile(`^\}`)
	open3Reg       = regexp.MustCompile(`^\[`)
	close3Reg      = regexp.MustCompile(`^\]`)
	// newLineReg      = regexp.MustCompile(`^\n`)
	comment = regexp.MustCompile(`^\*[^\n]*`)

	wsReg = regexp.MustCompile(`^\s`)

	EpsilonToken = Expr{
		Kind: Epsilon,
	}
)

const (
	// AxiomKeyword = iota
	NTermKeyword = iota
	TermKeyword
	RuleKeyword
	// EpsKeyword
	Term
	Nterm
	Equal
	Alternative
	Open1
	Close1
	Open2
	Close2
	Open3
	Close3
	// NewLine
	EOF
	Error
	Epsilon
)

type regWithKind struct {
	kind Kind
	reg  *regexp.Regexp
}

type Kind int

func (k Kind) ToString() string {
	switch k {
	// case AxiomKeyword:
	// 	return "AxiomKeyword"
	case NTermKeyword:
		return "NTermKeyword"
	case TermKeyword:
		return "TermKeyword"
	case RuleKeyword:
		return "RuleKeyword"
	// case EpsKeyword:
	// 	return "EpsKeyword"
	case Term:
		return "OriginalTerm"
	case Nterm:
		return "OriginalNterm"
	case Equal:
		return "Equal"
	case Alternative:
		return "|"
	case Open1:
		return "("
	case Close1:
		return ")"
	case Open2:
		return "{"
	case Close2:
		return "}"
	case Open3:
		return "["
	case Close3:
		return "]"
	// case NewLine:
	// 	return "NewLine"
	case EOF:
		return "EOF"
	case Error:
		return "Error"
	}

	return "unknown kind"
}

type Token struct {
	Kind  Kind
	Value string
	Start int
	End   int
}

func (t *Token) ToExpr() Expr {
	return Expr{
		Kind:  t.Kind,
		Value: t.Value,
	}
}

type Expr struct {
	Kind  Kind
	Value string
}

// func (t *Token) ToExpr() common.Expr {
// 	if t.Kind == EOF {
// 		return common.Dollar
// 	}
// 	return common.Expr{
// 		Kind:  common.Term,
// 		Value: t.Kind.ToString(),
// 	}
// }

type Lexer struct {
	text     string
	regs     []regWithKind
	curIndex int
	tokens   []Token
	filtered bool
	tokIndex int
	hasTerm  bool
}

func (l *Lexer) HasNext() bool {
	if !l.filtered {
		l.buildTokens()
	}
	return l.tokIndex < len(l.tokens)
}

func (l *Lexer) NextToken() Token {
	if !l.filtered {
		l.buildTokens()
	}
	if !l.HasNext() {
		return Token{
			Kind:  EOF,
			Start: l.curIndex + 1,
			End:   l.curIndex + 1,
		}
	}
	tok := l.tokens[l.tokIndex]
	l.tokIndex += 1
	return tok
}

func (l *Lexer) Peek() Token {
	if !l.filtered {
		l.buildTokens()
	}
	if l.tokIndex < len(l.tokens) {
		return l.tokens[l.tokIndex]
	}
	return Token{
		Kind:  EOF,
		Start: l.curIndex + 1,
		End:   l.curIndex + 1,
	}
}

func (l *Lexer) hasNextSymbol() bool {
	return len(l.text) > 0
}

func (l *Lexer) buildTokens() {
	for l.hasNextSymbol() {
		l.tokens = append(l.tokens, l.getNextToken())
	}
}

func (l *Lexer) getNextToken() Token {
	if !l.hasNextSymbol() {
		return Token{
			Kind:  EOF,
			Start: l.curIndex + 1,
			End:   l.curIndex + 1,
		}
	}

	if loc := wsReg.FindStringIndex(l.text); loc != nil {
		l.text = l.text[1:]
		l.curIndex += 1
		return l.getNextToken()
	}

	if loc := comment.FindStringIndex(l.text); loc != nil {
		l.text = l.text[loc[1]:]
		l.curIndex += (loc[1] - loc[0])
		return l.getNextToken()
	}

	for _, r := range l.regs {
		if loc := r.reg.FindStringIndex(l.text); loc != nil {
			if r.kind == TermKeyword {
				if l.hasTerm {
					l.text = l.text[loc[1]:]
					l.curIndex += (loc[1] - loc[0])
					return l.getNextToken()
				} else {
					l.hasTerm = true
				}
			}
			token := Token{
				Kind:  r.kind,
				Value: l.text[loc[0]:loc[1]],
				Start: l.curIndex,
				End:   l.curIndex + loc[1] - loc[0],
			}
			l.text = l.text[loc[1]:]
			l.curIndex += (loc[1] - loc[0])
			return token
		}
	}

	tok := Token{
		Kind:  Error,
		Start: l.curIndex,
		End:   l.curIndex,
	}

	l.curIndex += 1
	l.text = l.text[1:]

	return tok
}

func NewLexer(pathToFile string) (*Lexer, error) {
	data, err := ioutil.ReadFile(pathToFile)
	if err != nil {
		return nil, err
	}

	return &Lexer{
		text:     string(data),
		curIndex: 1,
		filtered: false,
		hasTerm:  false,
		regs: []regWithKind{
			// {
			// 	reg:  axiomKeywordReg.Copy(),
			// 	kind: AxiomKeyword,
			// },
			{
				reg:  ntermKeywordReg.Copy(),
				kind: NTermKeyword,
			},
			{
				reg:  termKeywordReg.Copy(),
				kind: TermKeyword,
			},
			{
				reg:  ruleKeywordReg.Copy(),
				kind: RuleKeyword,
			},
			// {
			// 	reg:  epsKeywordReg.Copy(),
			// 	kind: EpsKeyword,
			// },
			{
				reg:  ntermReg.Copy(),
				kind: Nterm,
			},
			{
				reg:  termReg.Copy(),
				kind: Term,
			},
			{
				reg:  equalReg.Copy(),
				kind: Equal,
			},
			{
				reg:  alternativeReg.Copy(),
				kind: Alternative,
			},
			{
				reg:  open1Reg.Copy(),
				kind: Open1,
			},
			{
				reg:  close1Reg.Copy(),
				kind: Close1,
			},
			{
				reg:  open2Reg.Copy(),
				kind: Open2,
			},
			{
				reg:  close2Reg.Copy(),
				kind: Close2,
			},
			{
				reg:  open3Reg.Copy(),
				kind: Open3,
			},
			{
				reg:  close3Reg.Copy(),
				kind: Close3,
			},
			// {
			// 	reg:  newLineReg.Copy(),
			// 	kind: NewLine,
			// },
		},
	}, nil
}
