package tools

import (
	"github.com/AlexisOMG/tfl-lab3/analyzer/rule_types"
)

func Dfs(graph map[string][]string, used *map[string]struct{}, v string) {
	(*used)[v] = struct{}{}
	if _, ok := graph[v]; !ok {
		return
	}

	for _, rp := range graph[v] {
		if _, ok := (*used)[rp]; !ok {
			Dfs(graph, used, rp)
		}
	}
}

func GetReachable(rules rule_types.Rule) map[string]struct{} {
	used := make(map[string]struct{})
	Dfs(GraphFromRules(rules), &used, "S")
	return used
}

func Reverse(graph map[string][]string) map[string][]string {
	res := make(map[string][]string)
	for l, rps := range graph {
		for _, rp := range rps {
			res[rp] = append(res[rp], l)
		}
	}
	return res
}

func GraphFromRules(rules rule_types.Rule) map[string][]string {
	res := make(map[string][]string)

	for left, rights := range rules {
		for _, right := range rights {
			for _, term := range right {
				if _, ok := rules[term.Name]; ok {
					res[left] = append(res[left], term.Name)
				}
			}
		}
	}

	return res
}

func GetAllNTerms(rules rule_types.Rule) map[string]struct{} {
	res := make(map[string]struct{})

	for nterm := range rules {
		res[nterm] = struct{}{}
		for _, terms := range rules[nterm] {
			for _, term := range terms {
				if rule_types.IsNterm(term.Name) {
					res[term.Name] = struct{}{}
				}
			}
		}
	}

	return res
}

func GetAllIrregularNTerms(rules rule_types.Rule) map[string]struct{} {
	res := make(map[string]struct{})

	for nterm := range rules {
		for _, terms := range rules[nterm] {
			if !rule_types.IsRegular(terms) {
				res[nterm] = struct{}{}
			}
		}
	}

	return res
}

func GetAllRegularNTerms(rules rule_types.Rule) map[string]struct{} {
	nterms := GetAllNTerms(rules)
	used := make(map[string]struct{})
	irregularNTerms := GetAllIrregularNTerms(rules)
	rgraph := Reverse(GraphFromRules(rules))

	for nterm := range irregularNTerms {
		if _, ok := used[nterm]; !ok {
			Dfs(rgraph, &used, nterm)
		}
	}

	return complement(nterms, used)
}

func complement(a map[string]struct{}, b map[string]struct{}) map[string]struct{} {
	res := make(map[string]struct{})

	for k := range a {
		if _, ok := b[k]; !ok {
			res[k] = struct{}{}
		}
	}

	return res
}
