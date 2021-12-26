package rules

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func IsNterm(s Term) bool {
	ok, err := regexp.Match(`^\[[A-Za-z]+\]$`, []byte(s))
	if err != nil {
		log.Println(fmt.Sprintf("cannot check nterm: %s", err.Error()))
		return false
	}
	return ok
}

func IsTerm(s Term) bool {
	ok, err := regexp.Match(`^([a-z]|[0-9]|_|\*|\+|=|\(|\)|\$|;|:)$`, []byte(s))
	if err != nil {
		log.Println(fmt.Sprintf("cannot check nterm: %s", err.Error()))
		return false
	}
	return ok
}

type Term string

type Rules map[Term][][]Term

func checkUpperCase(s string) bool {
	if s == "" {
		return false
	}

	for _, r := range s {
		if r < 'A' || r > 'Z' {
			return false
		}
	}

	return true
}

func checkLowerCase(s string) bool {
	if s == "" {
		return false
	}

	for _, r := range s {
		if r < 'a' || r > 'z' {
			return false
		}
	}

	return true
}

func checkNumeric(s string) bool {
	if s == "" {
		return false
	}

	for _, r := range s {
		if _, err := strconv.Atoi(string(r)); err != nil {
			return false
		}
	}

	return true
}

func isAlpha(s string) bool {
	return checkLowerCase(s) || checkUpperCase(s)
}

func ReadRules(pathToFile string) (Rules, error) {
	rules := make(Rules)

	var lines []string

	file, err := os.Open(pathToFile)
	if err != nil {
		return rules, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.Replace(scanner.Text(), " ", "", -1)
		line = strings.Replace(line, "\t", "", -1)
		line = strings.Replace(line, "\n", "", -1)
		line = strings.Replace(line, "\r", "", -1)
		lines = append(lines, line)
	}
	if err := scanner.Err(); err != nil {
		return rules, err
	}

	for _, line := range lines {
		ruleStr := strings.SplitN(line, "::=", 2)
		if len(ruleStr) != 2 {
			fmt.Println(ruleStr)
			return rules, fmt.Errorf("invalid rule: %s", line)
		}

		left, right := ruleStr[0], ruleStr[1]
		if !IsNterm(Term(left)) {
			return rules, fmt.Errorf("invalid nterm in left part: %s", line)
		}

		terms, err := ParseRightPart(right)
		if err != nil {
			return rules, fmt.Errorf("cannot parse right part of the rule '%s': %w", line, err)
		}

		rules[Term(left)] = append(rules[Term(left)], terms)
	}

	return rules, nil
}

func ParseRightPart(str string) ([]Term, error) {
	terms := make([]Term, 0)
	current := ""
	collect := false
	for i, s := range str {
		if s == '[' {
			collect = true
		}
		if collect {
			current += str[i : i+1]
		} else {
			terms = append(terms, Term(str[i:i+1]))
		}
		if s == ']' {
			terms = append(terms, Term(current))
			current = ""
			collect = false
		}
	}
	for _, term := range terms {
		if !IsNterm(term) && !IsTerm(term) {
			return []Term{}, fmt.Errorf("invalid right part: %s", str)
		}
	}

	return terms, nil
}

func IsChain(rule []Term) bool {
	return len(rule) == 1 && IsNterm(rule[0])
}

func IsRegularR(rule []Term) bool {
	return len(rule) > 0 && len(rule) <= 2 && IsTerm(rule[0]) && (len(rule) != 2 || IsNterm(rule[1]))
}

func IsRegularL(rule []Term) bool {
	return IsRegularR(reverseRule(rule))
}

func reverseRule(rule []Term) []Term {
	res := make([]Term, 0, len(rule))
	for i := len(rule) - 1; i >= 0; i-- {
		res = append(res, rule[i])
	}
	return res
}
