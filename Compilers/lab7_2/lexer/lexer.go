package lexer

import (
	"io/ioutil"
	"regexp"

	"github.com/AlexisOMG/compilers-lab7-2/common"
)

var (
	axiomKeywordReg = regexp.MustCompile(`^\$AXIOM`)
	ntermKeywordReg = regexp.MustCompile(`^\$NTERM`)
	termKeywordReg  = regexp.MustCompile(`^\$TERM`)
	ruleKeywordReg  = regexp.MustCompile(`^\$RULE`)
	epsKeywordReg   = regexp.MustCompile(`^\$EPS`)
	ntermReg        = regexp.MustCompile(`^[A-Z][^ \n]*`)
	termReg         = regexp.MustCompile(`^"[^ \n]+"`)
	equalReg        = regexp.MustCompile(`^=`)
	newLineReg      = regexp.MustCompile(`^\n`)
	comment         = regexp.MustCompile(`^\*[^\n]*`)
)

var (
	plusReg  = regexp.MustCompile(`^\+`)
	mulReg   = regexp.MustCompile(`^\*`)
	openReg  = regexp.MustCompile(`^\(`)
	closeReg = regexp.MustCompile(`^\)`)
	numReg   = regexp.MustCompile(`^\d+`)
	wsReg    = regexp.MustCompile(`^\s+`)
)

const (
	AxiomKeyword = iota
	NTermKeyword
	TermKeyword
	RuleKeyword
	EpsKeyword
	Term
	Nterm
	Equal
	NewLine
	EOF
	Error
	Plus
	Mult
	Open
	Close
	Number
)

type regWithKind struct {
	kind Kind
	reg  *regexp.Regexp
}

type Kind int

func (k Kind) ToString() string {
	switch k {
	case AxiomKeyword:
		return "AxiomKeyword"
	case NTermKeyword:
		return "NTermKeyword"
	case TermKeyword:
		return "TermKeyword"
	case RuleKeyword:
		return "RuleKeyword"
	case EpsKeyword:
		return "EpsKeyword"
	case Term:
		return "Term"
	case Nterm:
		return "Nterm"
	case Equal:
		return "Equal"
	case NewLine:
		return "NewLine"
	case EOF:
		return "EOF"
	case Error:
		return "Error"
	case Plus:
		return `+`
	case Mult:
		return `*`
	case Open:
		return `(`
	case Close:
		return `)`
	case Number:
		return `n`
	}

	return "unknown kind"
}

type Token struct {
	Kind  Kind
	Value string
	Start int
	End   int
}

func (t *Token) ToExpr() common.Expr {
	if t.Kind == EOF {
		return common.Dollar
	}
	return common.Expr{
		Kind:  common.Term,
		Value: t.Kind.ToString(),
	}
}

type calcLexer struct {
	text     string
	regs     []regWithKind
	curIndex int
}

func (cl *calcLexer) HasNext() bool {
	return len(cl.text) > 0
}

func (cl *calcLexer) NextToken() Token {
	if !cl.HasNext() {
		return Token{
			Kind:  EOF,
			Start: cl.curIndex + 1,
			End:   cl.curIndex + 1,
		}
	}

	if loc := wsReg.FindStringIndex(cl.text); loc != nil {
		cl.text = cl.text[loc[1]:]
		cl.curIndex += (loc[1] - loc[0])
		return cl.NextToken()
	}

	for _, r := range cl.regs {
		if loc := r.reg.FindStringIndex(cl.text); loc != nil {
			token := Token{
				Kind:  r.kind,
				Value: cl.text[loc[0]:loc[1]],
				Start: cl.curIndex,
				End:   cl.curIndex + loc[1] - loc[0],
			}
			cl.text = cl.text[loc[1]:]
			cl.curIndex += (loc[1] - loc[0])
			return token
		}
	}

	tok := Token{
		Kind:  Error,
		Start: cl.curIndex,
		End:   cl.curIndex,
	}

	cl.curIndex += 1
	cl.text = cl.text[1:]

	return tok

}

type grammarLexer struct {
	text     string
	regs     []regWithKind
	curIndex int
	tokens   []Token
	filtered bool
	tokIndex int
}

func (l *grammarLexer) hasNextSymbol() bool {
	return len(l.text) > 0
}

func (l *grammarLexer) filter() {
	if l.filtered {
		return
	}

	for l.hasNextSymbol() {
		l.tokens = append(l.tokens, l.nextUnfilteredToken())
	}

	filteredTokens := make([]Token, 0, len(l.tokens))

	isRule := false

	for i, t := range l.tokens {
		switch t.Kind {
		case RuleKeyword:
			isRule = true
		case AxiomKeyword, NTermKeyword, TermKeyword:
			isRule = false
		}

		if t.Kind == NewLine {
			if isRule && !(i == len(l.tokens)-1 || (i < len(l.tokens)-1 && l.tokens[i+1].Kind == RuleKeyword)) {
				filteredTokens = append(filteredTokens, t)
			}
		} else {
			filteredTokens = append(filteredTokens, t)
		}
	}

	l.tokens = filteredTokens
	l.filtered = true
}

func (l *grammarLexer) nextUnfilteredToken() Token {
	if !l.hasNextSymbol() {
		return Token{
			Kind:  EOF,
			Start: l.curIndex + 1,
			End:   l.curIndex + 1,
		}
	}

	if l.text[0] == ' ' || l.text[0] == '\t' {
		l.text = l.text[1:]
		l.curIndex += 1
		return l.nextUnfilteredToken()
	}

	if loc := comment.FindStringIndex(l.text); loc != nil {
		l.text = l.text[loc[1]:]
		l.curIndex += (loc[1] - loc[0])
		return l.nextUnfilteredToken()
	}

	for _, r := range l.regs {
		if loc := r.reg.FindStringIndex(l.text); loc != nil {
			value := l.text[loc[0]:loc[1]]
			if r.kind == Term {
				value = l.text[loc[0]+1 : loc[1]-1]
			}
			token := Token{
				Kind:  r.kind,
				Value: value,
				Start: l.curIndex,
				End:   l.curIndex + loc[1] - loc[0],
			}
			if token.Kind == NewLine {
				token.Value = `\n`
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

func (l *grammarLexer) HasNext() bool {
	if !l.filtered {
		l.filter()
	}

	return l.tokIndex < len(l.tokens)
}

func (l *grammarLexer) NextToken() Token {
	if !l.HasNext() {
		return Token{
			Kind:  EOF,
			Start: l.curIndex + 1,
			End:   l.curIndex + 1,
		}
	}

	l.tokIndex += 1
	return l.tokens[l.tokIndex-1]
}

type Lexer interface {
	NextToken() Token
	HasNext() bool
}

func NewLexer(pathToFile string, isCalc bool) (Lexer, error) {
	data, err := ioutil.ReadFile(pathToFile)
	if err != nil {
		return nil, err
	}

	if isCalc {
		return &calcLexer{
			text:     string(data),
			curIndex: 1,
			regs: []regWithKind{
				{
					reg:  plusReg.Copy(),
					kind: Plus,
				},
				{
					reg:  mulReg.Copy(),
					kind: Mult,
				},
				{
					reg:  openReg.Copy(),
					kind: Open,
				},
				{
					reg:  closeReg.Copy(),
					kind: Close,
				},
				{
					reg:  numReg.Copy(),
					kind: Number,
				},
			},
		}, nil
	}

	return &grammarLexer{
		text:     string(data),
		curIndex: 1,
		filtered: false,
		regs: []regWithKind{
			{
				reg:  axiomKeywordReg.Copy(),
				kind: AxiomKeyword,
			},
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
			{
				reg:  epsKeywordReg.Copy(),
				kind: EpsKeyword,
			},
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
				reg:  newLineReg.Copy(),
				kind: NewLine,
			},
		},
	}, nil
}
