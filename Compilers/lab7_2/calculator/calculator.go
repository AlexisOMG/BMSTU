package main

import (
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/AlexisOMG/compilers-lab7-2/lexer"
	"github.com/AlexisOMG/compilers-lab7-2/parser"
)

func ComputeE(root *parser.Node) (int, error) {
	a, err := computeT(root.Children[0])
	if err != nil {
		return -1, err
	}

	b, err := computeEt(root.Children[1])
	if err != nil {
		return -1, err
	}

	return a + b, nil
}

func computeEt(root *parser.Node) (int, error) {
	if len(root.Children) == 0 {
		return 0, nil
	}

	a, err := computeT(root.Children[1])
	if err != nil {
		return 0, err
	}

	b, err := computeEt(root.Children[2])
	if err != nil {
		return 0, err
	}

	return a + b, nil
}

func computeT(root *parser.Node) (int, error) {
	if len(root.Children) == 0 {
		return 1, nil
	}

	a, err := computeF(root.Children[0])
	if err != nil {
		return 1, err
	}

	b, err := computeTt(root.Children[1])
	if err != nil {
		return 1, err
	}

	return a * b, nil
}

func computeTt(root *parser.Node) (int, error) {
	if len(root.Children) == 0 {
		return 1, nil
	}

	a, err := computeF(root.Children[1])
	if err != nil {
		return 1, err
	}

	b, err := computeTt(root.Children[2])
	if err != nil {
		return 1, err
	}

	return a * b, nil
}

func computeF(root *parser.Node) (int, error) {
	if len(root.Children) == 1 {
		return strconv.Atoi(root.Children[0].Value)
	}
	return ComputeE(root.Children[1])
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	calcLex, err := lexer.NewLexer(pathToFile, true)
	if err != nil {
		log.Fatal(err)
	}

	calcRoot, err := parser.Parse(calcLex, "calctable.json")
	if err != nil {
		log.Fatal(err)
	}

	calcRoot.Print(1)

	res, err := ComputeE(calcRoot)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("RESULT: ", res)
}
