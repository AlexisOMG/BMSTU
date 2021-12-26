package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type Rule struct {
	Left  string
	Right string
}

func prefixFunc(str string, pi []int, startInd int) ([]int, error) {
	if len(str) != len(pi) {
		return nil, fmt.Errorf("str size = %d, pi size = %d", len(str), len(pi))
	}

	for i := startInd; i < len(str); i++ {
		j := pi[i-1]
		for j > 0 && str[i] != str[j] {
			j = pi[j-1]
		}
		if str[i] == str[j] {
			j += 1
		}
		pi[i] = j
	}

	return pi, nil
}

func checkConfluence(rules []Rule, prefixes [][]int) ([]int, error) {
	for i, ruleI := range rules {
		for j, ruleJ := range rules {
			if i != j && ruleI.Left != "" && ruleJ.Left != "" {
				concat := ruleI.Left + "~" + ruleJ.Left
				pi := make([]int, len(concat))
				for k, v := range prefixes[i] {
					pi[k] = v
				}
				pi, err := prefixFunc(concat, pi, len(ruleI.Left))
				if err != nil {
					return nil, err
				}
				if len(pi) > 0 && pi[len(pi)-1] > 0 {
					return []int{i, j}, nil
				}
			}
		}
	}

	return nil, nil
}

func main() {
	pathToFile := os.Args[1]
	var rules []Rule
	var lines []string
	var prefixes [][]int

	file, err := os.Open(pathToFile)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err.Error())
	}

	for _, line := range lines {
		cleanedLine := strings.ReplaceAll(line, " ", "")
		terms := strings.Split(cleanedLine, "->")
		var rule Rule
		switch {
		case len(terms) == 2:
			rule = Rule{
				Left:  terms[0],
				Right: terms[1],
			}
		case strings.Index(cleanedLine, "->") > 0:
			rule = Rule{
				Left:  terms[0],
				Right: "",
			}
		default:
			rule = Rule{
				Left:  "",
				Right: terms[0],
			}
		}

		rules = append(rules, rule)
		pi := make([]int, len(rule.Left))

		prefix, err := prefixFunc(rule.Left, pi, 1)
		if err != nil {
			log.Fatal(err.Error())
		}
		if len(prefix) > 0 && prefix[len(prefix)-1] > 0 && prefix[len(prefix)-1]*2 != len(rule.Left) {
			fmt.Printf("Problem with confluence: %s\n prefix: %s\n", line, rule.Left[:prefix[len(prefix)-1]])
			os.Exit(0)
		}
		prefixes = append(prefixes, prefix)
	}

	example, err := checkConfluence(rules, prefixes)
	if err != nil {
		log.Fatal(err.Error())
	}

	if len(example) != 0 {
		fmt.Printf("Problem with confluence:\n\t%s\n\t%s\n", lines[example[0]], lines[example[1]])
	} else {
		fmt.Println("Everything is OK")
	}
}
