package main

import (
	"fmt"
	"log"
	"os"

	"github.com/AlexisOMG/tfl-lab4/rules"
	"github.com/AlexisOMG/tfl-lab4/tools"
)

func toString(nterms map[rules.Term]struct{}) string {
	res := "{"
	for nterm := range nterms {
		res += string(nterm) + ", "
	}
	if len(res) > 1 {
		res = res[:len(res)-2]
	}
	return res + "}"
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("No test file")
	}
	pathToFile := os.Args[1]

	rls, err := rules.ReadRules(pathToFile)
	if err != nil {
		log.Fatal(err.Error())
	}

	regularR, regularL, closing := tools.GetRegular(rls)
	rls, regularR, regularL, closing = tools.WrapInGuards(rls, regularR, regularL, closing)

	fmt.Println("REGULAR_R: ", toString(regularR))
	fmt.Println("REGULAR_L: ", toString(regularL))

	first := tools.BuildFIRST(rls)
	last := tools.BuildLAST(rls)

	// fmt.Println("FIRST: ", first)
	// fmt.Println("LAST: ", last)

	follow := tools.BuildFOLLOW(rls, first)

	followStr := "{"
	for nterm, set := range follow {
		followStr += "{" + string(nterm) + ": " + toString(set) + "}, "
	}
	if len(followStr) > 1 {
		followStr = followStr[:len(followStr)-2]
	}
	followStr += "}"
	fmt.Println("FOLLOW: ", followStr)

	precede := tools.BuildPRECEDE(rls, last)

	precedeStr := "{"
	for nterm, set := range precede {
		precedeStr += "{" + string(nterm) + ": " + toString(set) + "}, "
	}
	if len(precedeStr) > 1 {
		precedeStr = precedeStr[:len(precedeStr)-2]
	}
	precedeStr += "}"
	fmt.Println("PRECEDE: ", precedeStr)

	tokens, errors := tools.FindTokens(rls, regularR, regularL, closing, follow, precede)
	for _, err := range errors {
		fmt.Println(err.Error())
	}
	fmt.Println("TOKENS: ", toString(tokens))

	regs, err := tools.BuildRegex(rls, regularR, regularL, closing, tokens)
	if err != nil {
		log.Fatal(err)
	}
	regex := "{"
	for k, v := range regs {
		regex += string(k) + ": " + v + ", "
	}
	if len(regex) > 1 {
		regex = regex[:len(regex)-2]
	}
	regex += "}"
	fmt.Println("REGEXS: ", regex)
}
