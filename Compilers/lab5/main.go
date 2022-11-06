package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
)

const (
	WhitSpace    = "White Space"
	Number       = "Number"
	Identifier   = "Identifier"
	Begin        = "Begin"
	End          = "End"
	Str          = "String"
	OpenBracket  = "Open Bracket"
	CloseBracket = "Close Bracket"
	EOP          = "End Of Programm"
	Error        = "Error"
)

var (
	whiteSpaceReg   = regexp.MustCompile(`\s`)
	digitReg        = regexp.MustCompile(`[0-9]`)
	alphabeticReg   = regexp.MustCompile(`[a-z]|[A-Z]`)
	alphanumericReg = regexp.MustCompile(`[a-z]|[A-Z]|[0-9]`)
	bSymbolReg      = regexp.MustCompile(`b`)
	eSymbolReg      = regexp.MustCompile(`e`)
	gSymbolReg      = regexp.MustCompile(`g`)
	iSymbolReg      = regexp.MustCompile(`i`)
	nSymbolReg      = regexp.MustCompile(`n`)
	dSymbolReg      = regexp.MustCompile(`d`)
	openBracketReg  = regexp.MustCompile(`{`)
	closeBracketReg = regexp.MustCompile(`}`)
	dollarReg       = regexp.MustCompile(`\$`)
	strReg          = regexp.MustCompile(`[^\n]`)
	backSlashReg    = regexp.MustCompile(`\\`)

	finalStates = map[int]Kind{
		1:  WhitSpace,
		2:  Number,
		3:  Identifier,
		4:  Identifier,
		5:  Identifier,
		6:  Identifier,
		7:  Identifier,
		8:  Begin,
		9:  Identifier,
		10: Identifier,
		11: End,
		13: Str,
		14: OpenBracket,
		15: CloseBracket,
	}

	dfa = Transitions{
		0: []Transition{
			{1, func(s string) bool { return whiteSpaceReg.MatchString(s) }},
			{2, func(s string) bool { return digitReg.MatchString(s) }},
			{4, func(s string) bool { return bSymbolReg.MatchString(s) }},
			{9, func(s string) bool { return eSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphabeticReg.MatchString(s) }},
			{12, func(s string) bool { return dollarReg.MatchString(s) }},
			{14, func(s string) bool { return openBracketReg.MatchString(s) }},
			{15, func(s string) bool { return closeBracketReg.MatchString(s) }},
		},
		1: []Transition{
			{1, func(s string) bool { return whiteSpaceReg.MatchString(s) }},
		},
		2: []Transition{
			{2, func(s string) bool { return digitReg.MatchString(s) }},
		},
		3: []Transition{
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		4: []Transition{
			{5, func(s string) bool { return eSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		5: []Transition{
			{6, func(s string) bool { return gSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		6: []Transition{
			{7, func(s string) bool { return iSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		7: []Transition{
			{8, func(s string) bool { return nSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		8: []Transition{
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		9: []Transition{
			{10, func(s string) bool { return nSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		10: []Transition{
			{11, func(s string) bool { return dSymbolReg.MatchString(s) }},
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		11: []Transition{
			{3, func(s string) bool { return alphanumericReg.MatchString(s) }},
		},
		12: []Transition{
			{16, func(s string) bool { return backSlashReg.MatchString(s) }},
			{13, func(s string) bool { return dollarReg.MatchString(s) }},
			{12, func(s string) bool { return strReg.MatchString(s) }},
		},
		16: []Transition{
			{12, func(s string) bool { return strReg.MatchString(s) }},
		},
		13: []Transition{},
		14: []Transition{},
		15: []Transition{},
	}
)

type Kind string

type FinalStates map[int]Kind

type Transition struct {
	StateTo int
	Match   func(s string) bool
}

type Transitions map[int][]Transition

type Position struct {
	Line   int
	Column int
	Index  int
}

type Token struct {
	Start Position
	End   Position
	Kind  Kind
	Value string
}

type Lexer interface {
	NextToken() (Token, error)
	HasNext() bool
}

func NewLexer(text string, fStates map[int]Kind, dfa Transitions) Lexer {
	var lex Lexer = &literalLexer{
		text:    text,
		fStates: fStates,
		dfa:     dfa,
		currPos: Position{1, 1, 0},
	}
	return lex
}

type literalLexer struct {
	text      string
	currPos   Position
	prevPos   Position
	dfa       Transitions
	fStates   FinalStates
	currState int
}

func (lt literalLexer) HasNext() bool {
	return lt.currPos.Index < len(lt.text)
}

func (lt *literalLexer) NextToken() (Token, error) {
	res := Token{}
	startState := 0
	lt.currState = startState

	if _, ok := lt.dfa[startState]; !ok {
		return Token{}, fmt.Errorf("no start state")
	}

	lt.prevPos = lt.currPos
	res.Start = lt.currPos

	if !lt.HasNext() {
		return Token{
			Start: lt.currPos,
			End:   lt.currPos,
			Kind:  EOP,
			Value: "EOP",
		}, nil
	}

	currSymb := lt.text[lt.prevPos.Index : lt.prevPos.Index+1]

	if currSymb == "\n" {
		lt.currPos.Index += 1
		lt.currPos.Line += 1
		lt.currPos.Column = 0
		return lt.NextToken()
	}

	err := lt.stepIn(currSymb)
	if err != nil {
		return Token{}, err
	}

	if kind, ok := lt.fStates[lt.currState]; ok {
		res.Kind = kind
		res.End = lt.currPos
		res.Value = lt.text[lt.prevPos.Index:lt.currPos.Index]
	} else {
		res.Start = lt.currPos
		res.End = lt.currPos
		res.Kind = Error

		// lt.currPos = lt.prevPos
		lt.currPos.Index += 1
		lt.currPos.Column += 1
	}

	if res.Kind == WhitSpace {
		return lt.NextToken()
	}

	return res, nil
}

func (lt *literalLexer) stepIn(s string) error {
	if _, ok := lt.dfa[lt.currState]; !ok {
		return fmt.Errorf("unknown state: %d", lt.currState)
	}
	for _, trans := range lt.dfa[lt.currState] {
		if trans.Match(s) {
			lt.currPos.Index += 1
			lt.currPos.Column += 1
			lt.currState = trans.StateTo
			if lt.currPos.Index < len(lt.text) {
				ss := lt.text[lt.currPos.Index : lt.currPos.Index+1]
				return lt.stepIn(ss)
			}
			return nil
		}
	}
	return nil
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	data, err := ioutil.ReadFile(pathToFile)
	if err != nil {
		log.Fatal(err.Error())
	}

	lex := NewLexer(string(data), finalStates, dfa)

	for {
		tok, err := lex.NextToken()
		if err != nil {
			log.Fatal(err.Error())
		}

		fmt.Printf("%s (%v, %v): %s\n", tok.Kind, tok.Start, tok.End, tok.Value)

		if tok.Kind == EOP {
			break
		}
	}
}
