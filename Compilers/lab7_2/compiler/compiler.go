package main

import (
	"fmt"
	"log"
	"os"

	"github.com/AlexisOMG/compilers-lab7-2/common"
	"github.com/AlexisOMG/compilers-lab7-2/lexer"
	"github.com/AlexisOMG/compilers-lab7-2/parser"
)

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	lex, err := lexer.NewLexer(pathToFile, false)
	if err != nil {
		log.Fatal(err)
	}

	rules := parser.Rules
	table := common.BuildTable(rules, common.Expr{
		Kind:  common.NTerm,
		Value: "S",
	}, parser.Terminals)
	err = parser.SaveTableInfo("initial.json", table, common.Expr{
		Kind:  common.NTerm,
		Value: "S",
	})
	if err != nil {
		log.Fatal(err)
	}

	root, err := parser.Parse(lex, "initial.json")
	if err != nil {
		log.Fatal(err)
	}

	calcRules, axiom, terminals, err := parser.BuildRules(root)
	if err != nil {
		log.Fatal(err)
	}

	calcTable := common.BuildTable(calcRules, axiom, terminals)
	err = parser.SaveTableInfo("calctable.json", calcTable, axiom)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Success")
}
