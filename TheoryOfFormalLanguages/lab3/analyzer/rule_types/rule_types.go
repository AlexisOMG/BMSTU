package rule_types

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

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

func IsNterm(s string) bool {
	return s != "" && len(s) <= 2 && checkUpperCase(s[:1]) && (len(s[1:]) == 0 || checkNumeric(s[1:]))
}

func IsTerm(s string) bool {
	return len(s) == 1 && checkLowerCase(s)
}

type Term struct {
	Name string
}

type Rule map[string][][]Term

func IsRegular(terms []Term) bool {
	return len(terms) > 0 && len(terms) <= 2 && IsTerm(terms[0].Name) && (len(terms) != 2 || IsNterm(terms[1].Name))
}

func ParseRightPart(str string) ([]Term, error) {
	terms := make([]Term, 0)
	current := ""

	for i := 0; i < len(str); i++ {
		switch {
		case isAlpha(str[i : i+1]):
			if current != "" {
				terms = append(terms, Term{current})
			}

			current = str[i : i+1]
			if checkLowerCase(current) {
				terms = append(terms, Term{current})
				current = ""
			}

		case checkNumeric(str[i : i+1]):
			if current == "" {
				return []Term{}, fmt.Errorf("invalid nterm in right part: %s", str)
			}
			current += str[i : i+1]

		default:
			return []Term{}, fmt.Errorf("wrong symbol in right part: %s", str)
		}
	}

	if current != "" {
		terms = append(terms, Term{current})
	}

	return terms, nil
}

func ReadRules(pathToFile string) (Rule, error) {
	rules := make(Rule)

	var lines []string

	file, err := os.Open(pathToFile)
	if err != nil {
		return rules, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, strings.Replace(scanner.Text(), " ", "", -1))
	}
	if err := scanner.Err(); err != nil {
		return rules, err
	}

	for _, line := range lines {
		ruleStr := strings.Split(line, "->")
		if len(ruleStr) != 2 {
			return rules, fmt.Errorf("invalid rule: %s", line)
		}

		left, right := ruleStr[0], ruleStr[1]
		if !IsNterm(left) {
			return rules, fmt.Errorf("invalid nterm in left part: %s", line)
		}

		terms, err := ParseRightPart(right)
		if err != nil {
			return rules, fmt.Errorf("cannot parse right part of the rule '%s': %w", line, err)
		}

		rules[left] = append(rules[left], terms)
	}

	return rules, nil
}
