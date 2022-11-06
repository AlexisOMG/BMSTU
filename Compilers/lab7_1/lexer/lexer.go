package lexer

import (
	"io/ioutil"
	"regexp"

	"github.com/AlexisOMG/compilers-lab7-1/common"
)

var (
	axiomKeywordReg = regexp.MustCompile(`^\$AXIOM`)
	ntermKeywordReg = regexp.MustCompile(`^\$NTERM`)
	termKeywordReg  = regexp.MustCompile(`^\$TERM`)
	ruleKeywordReg  = regexp.MustCompile(`^\$RULE`)
	epsKeywordReg   = regexp.MustCompile(`^\$EPS`)
	ntermReg        = regexp.MustCompile(`^[A-Z][^ \n]*`)
	termReg         = regexp.MustCompile(`^"[^ \nA-Z]+"`)
	equalReg        = regexp.MustCompile(`^=`)
	newLineReg      = regexp.MustCompile(`^\n`)
	comment         = regexp.MustCompile(`^\*[^\n]*`)
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

type Lexer struct {
	text     string
	regs     []regWithKind
	curIndex int
	tokens   []Token
	filtered bool
	tokIndex int
}

func (l *Lexer) hasNextSymbol() bool {
	return len(l.text) > 0
}

func (l *Lexer) filter() {
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

func (l *Lexer) nextUnfilteredToken() Token {
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
			token := Token{
				Kind:  r.kind,
				Value: l.text[loc[0]:loc[1]],
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

func (l *Lexer) HasNext() bool {
	if !l.filtered {
		l.filter()
	}

	return l.tokIndex < len(l.tokens)
}

func (l *Lexer) NextToken() Token {
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

func NewLexer(pathToFile string) (*Lexer, error) {
	data, err := ioutil.ReadFile(pathToFile)
	if err != nil {
		return nil, err
	}

	return &Lexer{
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
