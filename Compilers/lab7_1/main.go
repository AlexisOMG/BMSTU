package main

import (
	"log"
	"os"

	"github.com/AlexisOMG/compilers-lab7-1/common"
	"github.com/AlexisOMG/compilers-lab7-1/lexer"
	"github.com/AlexisOMG/compilers-lab7-1/parser"
)

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	lex, err := lexer.NewLexer(pathToFile)
	if err != nil {
		log.Fatal(err)
	}

	rules := parser.Rules

	first := common.First(rules)

	follow := common.Follow(rules, common.Expr{
		Kind:  common.NTerm,
		Value: "S",
	}, first)

	table := common.BuildTable(rules, first, follow, parser.Terminals)

	answ, err := parser.Parse(table, lex, common.Expr{
		Kind:  common.NTerm,
		Value: "S",
	})
	if err != nil {
		log.Fatal(err)
	}

	answ.Print(1)
}
