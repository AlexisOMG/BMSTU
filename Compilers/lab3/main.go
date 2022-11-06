package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"regexp"
)

var strRegexp *regexp.Regexp = regexp.MustCompile(`^"[^"\\]*"`)
var numRegexp *regexp.Regexp = regexp.MustCompile(`^\d(\d|_)*`)

type Token struct {
	Name  string
	Line  int
	Shift int
	Value string
}

type Lexer interface {
	NextToken() (Token, error)
	HasNext() bool
}

func NewLexer(text []string) Lexer {
	var lex Lexer = &literalLexer{
		text: text,
	}
	return lex
}

type literalLexer struct {
	text  []string
	pos   int
	shift int
}

func (lt literalLexer) HasNext() bool {
	for lt.pos < len(lt.text) && lt.text[lt.pos] == "" {
		lt.pos += 1
	}
	return lt.pos < len(lt.text)
}

func (lt *literalLexer) NextToken() (tok Token, err error) {
	if lt.text[lt.pos] == "" {
		lt.pos += 1
		lt.shift = 0
	}
	if lt.pos >= len(lt.text) {
		return Token{}, errors.New("empty text")
	}

	line := lt.text[lt.pos]

	for line[0] == ' ' {
		line = line[1:]
		lt.shift += 1
	}

	strLoc := strRegexp.FindStringIndex(line)
	numLoc := numRegexp.FindStringIndex(line)

	if strLoc == nil && numLoc == nil {
		tok := Token{
			Name:  "Error",
			Line:  lt.pos,
			Shift: lt.shift,
		}
		for len(line) > 0 && line[0] != ' ' {
			line = line[1:]
			lt.shift += 1
			lt.text[lt.pos] = line
		}
		return tok, nil
	}

	if numLoc == nil {
		tok := Token{
			Name:  "StringLiteral",
			Line:  lt.pos,
			Shift: lt.shift,
			Value: line[strLoc[0]:strLoc[1]],
		}
		lt.shift += len(line[strLoc[0]:strLoc[1]])
		lt.text[lt.pos] = line[strLoc[1]:]
		return tok, nil
	}

	if strLoc == nil {
		tok := Token{
			Name:  "NumLiteral",
			Line:  lt.pos,
			Shift: lt.shift,
			Value: line[numLoc[0]:numLoc[1]],
		}
		lt.shift += len(line[numLoc[0]:numLoc[1]])
		lt.text[lt.pos] = line[numLoc[1]:]
		return tok, nil
	}

	return Token{}, errors.New("smth went wrong")
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	file, err := os.Open(pathToFile)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if scanner.Err() != nil {
		log.Fatal(scanner.Err().Error())
	}

	lexer := NewLexer(lines)
	for lexer.HasNext() {
		tok, err := lexer.NextToken()
		if err != nil {
			log.Fatal(err.Error())
		}
		fmt.Println(tok)
	}
}
