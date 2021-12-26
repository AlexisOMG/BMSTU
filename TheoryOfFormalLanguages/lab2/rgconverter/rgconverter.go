package rgconverter

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/AlexisOMG/tfl-lab2/rgsystem"
)

func init() {
	rgsystem.LeftEqutionVars = make(map[string]struct{})
}

type RightPart struct {
	Term  string
	NTerm string
}

func intersec(a, b map[string]struct{}) map[string]struct{} {
	intersection := make(map[string]int)

	for k := range a {
		intersection[k] += 1
	}
	for k := range b {
		intersection[k] += 1
	}

	res := make(map[string]struct{})
	for k, v := range intersection {
		if v > 1 {
			res[k] = struct{}{}
		}
	}

	return res
}

func dfs(graph map[string][]string, used *map[string]struct{}, v string) {
	(*used)[v] = struct{}{}
	if _, ok := graph[v]; !ok {
		return
	}

	for _, rp := range graph[v] {
		if _, ok := (*used)[rp]; !ok {
			dfs(graph, used, rp)
		}
	}
}

func graphFromRules(rules map[string][]RightPart) map[string][]string {
	res := make(map[string][]string)

	for l, rps := range rules {
		for _, rp := range rps {
			if rp.NTerm != "" {
				res[l] = append(res[l], rp.NTerm)
			}
		}
	}

	return res
}

func getReachableNTerms(rules map[string][]RightPart) map[string]struct{} {
	used := make(map[string]struct{})
	dfs(graphFromRules(rules), &used, "S")
	return used
}

func getFinishingNTerms(rules map[string][]RightPart) map[string]struct{} {
	starts := getNonProducingNTerms(rules)
	rgraph := reverse(graphFromRules(rules))
	used := make(map[string]struct{})

	for s := range starts {
		if _, ok := used[s]; !ok {
			dfs(rgraph, &used, s)
		}
	}

	return used
}

func getNonProducingNTerms(rules map[string][]RightPart) map[string]struct{} {
	res := make(map[string]struct{})
	for l, rps := range rules {
		for _, rp := range rps {
			if rp.NTerm == "" {
				res[l] = struct{}{}
			}
		}
	}
	return res
}

func reverse(graph map[string][]string) map[string][]string {
	res := make(map[string][]string)
	for l, rps := range graph {
		for _, rp := range rps {
			res[rp] = append(res[rp], l)
		}
	}
	return res
}

func getGoodNTerms(rules map[string][]RightPart) map[string]struct{} {
	reachableNTerms := getReachableNTerms(rules)
	finishingNTerms := getFinishingNTerms(rules)
	return intersec(reachableNTerms, finishingNTerms)
}

func BuildEquotions(rules map[string][]RightPart, goodNTerms map[string]struct{}) []string {
	var res []string

	for l, rps := range rules {
		if _, ok := goodNTerms[l]; !ok {
			continue
		}

		eq := l + "="
		coef := make(map[string]string)
		for _, rp := range rps {
			vr := "free"
			if rp.NTerm != "" {
				if _, ok := goodNTerms[rp.NTerm]; !ok {
					continue
				}
				vr = rp.NTerm
			}

			vk := coef[vr]
			switch {
			case vk == "":
				coef[vr] = rp.Term
			case len(vk) == 1:
				coef[vr] = "(" + vk + "|" + rp.Term + ")"
			default:
				coef[vr] = "(" + vk[1:len(vk)-1] + "|" + rp.Term + ")"
			}
		}

		for k, v := range coef {
			if k == "free" {
				continue
			}
			if eq[len(eq)-1] == '=' {
				eq += v
			} else {
				eq += "+" + v
			}

			eq += k
		}

		if eq[len(eq)-1] == '=' {
			eq += coef["free"]
		} else {
			eq += "+" + coef["free"]
		}

		res = append(res, eq)
	}

	return res
}

func AddAlt(coeff string, term string) string {
	if coeff == "" {
		return term
	}
	if len(coeff) == 1 {
		return "(" + coeff + "|" + term + ")"
	}
	return "(" + coeff[1:len(coeff)-1] + "|" + term + ")"
}

func Run(pathToFile string) (string, error) {
	var lines []string

	file, err := os.Open(pathToFile)
	if err != nil {
		return "", err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, strings.Replace(scanner.Text(), " ", "", -1))
	}
	if err := scanner.Err(); err != nil {
		return "", err
	}

	rules := make(map[string][]RightPart)

	for _, line := range lines {
		parts := strings.Split(line, "->")
		if len(parts) != 2 {
			return "", fmt.Errorf("invalid rule: %s", line)
		}
		if !rgsystem.CheckUpperCase(parts[0]) {
			return "", fmt.Errorf("left part is not nterm: %s", line)
		}

		switch len(parts[1]) {
		case 1:
			if !rgsystem.CheckLowerCase(parts[1]) {
				return "", fmt.Errorf("right part is not term: %s", line)
			}
			rules[parts[0]] = append(rules[parts[0]], RightPart{
				Term: parts[1],
			})
		case 2:
			if !rgsystem.CheckLowerCase(parts[1][:1]) || !rgsystem.CheckUpperCase(parts[1][1:]) {
				return "", fmt.Errorf("right rule is not term + nterm: %s", line)
			}
			rules[parts[0]] = append(rules[parts[0]], RightPart{
				Term:  parts[1][:1],
				NTerm: parts[1][1:],
			})
		default:
			return "", fmt.Errorf("invalid right part of rule: %s", line)
		}

	}

	goodNTerms := getGoodNTerms(rules)
	eqs := BuildEquotions(rules, goodNTerms)
	convertedEqs, err := rgsystem.EqutionsFromStrings(eqs)
	if err != nil {
		return "", err
	}

	answs, err := rgsystem.Solve(convertedEqs)
	if err != nil {
		return "", err
	}

	for _, answ := range answs {
		if answ.EqutionVar.Name == "S" {
			return rgsystem.PrintSolving([]rgsystem.Eqution{answ})[0], nil
		}
	}

	return "", fmt.Errorf("no solution")
}
