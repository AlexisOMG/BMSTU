package analyzer

import (
	"strings"

	"github.com/AlexisOMG/tfl-lab3/analyzer/pumptree"
	"github.com/AlexisOMG/tfl-lab3/analyzer/rule_types"
	"github.com/AlexisOMG/tfl-lab3/analyzer/tools"
)

func Analyze(pathToFile string) (string, error) {
	rules, err := rule_types.ReadRules(pathToFile)
	if err != nil {
		return "", err
	}
	regularNTerms := tools.GetAllRegularNTerms(rules)
	pumpings := pumptree.BuildLeftPumpingTree(rules)
	maybeRegularNTerms := make(map[string]struct{})
	badNTerms := make(map[string]struct{})

	for nterm, pumping := range pumpings {
		if _, ok := regularNTerms[nterm]; ok {
			continue
		}

		pumpingValue := pumping.GetPumping()
		f1, f2 := strings.SplitN(pumpingValue, nterm, 2)[0], strings.SplitN(pumpingValue, nterm, 2)[1]
		terms, err := rule_types.ParseRightPart(f2)
		if err != nil {
			return "", err
		}
		isRegular := true
		for _, term := range terms {
			if _, ok := regularNTerms[term.Name]; rule_types.IsNterm(term.Name) && !ok {
				isRegular = false
			}
		}

		if !isRegular {
			continue
		}

		if !checkBelonging(f1, rules, terms, terms) {
			badNTerms[nterm] = struct{}{}
			continue
		}

		words := terminateNTerm(rule_types.Term{nterm}, rules, make(map[string]struct{}))
		for w := range words {
			isRegular = isRegular && checkBelonging(w, rules, terms, terms)
		}

		if isRegular {
			maybeRegularNTerms[nterm] = struct{}{}
		}
	}

	regularNTerms, maybeRegularNTerms = checkAnotherNTerms(rules, regularNTerms, maybeRegularNTerms)

	res := ""

	if _, ok := regularNTerms["S"]; ok {
		res += "Regular Language\n"
	} else if _, ok := maybeRegularNTerms["S"]; ok {
		res += "Possible Regular Language\n"
	} else if _, ok := badNTerms["S"]; ok {
		res += "Suspicious Language\n"
	} else {
		res += "Unable to check language\n"
	}

	res += "Regular NTerms: "
	for nterm := range regularNTerms {
		res += nterm + " "
	}
	res += "\nPossible regular NTerms: "
	for nterm := range maybeRegularNTerms {
		res += nterm + " "
	}
	res += "\nPossible bad NTerms >:-§ : "
	for nterm := range badNTerms {
		res += nterm + " "
	}
	res += "\n"
	for n, p := range pumpings {
		if _, ok := regularNTerms[n]; !ok {
			label := 0
			res += p.Print(&label, -1) + "\n"
		}
	}

	return res, nil
}

func addWord(base map[string]struct{}, word string) map[string]struct{} {
	res := make(map[string]struct{})

	for w := range base {
		res[w+word] = struct{}{}
	}

	return res
}

func crossProduct(first, second map[string]struct{}) map[string]struct{} {
	res := make(map[string]struct{})

	for f := range first {
		for s := range second {
			res[f+s] = struct{}{}
		}
	}

	return res
}

func getWordsWithMinLen(words map[string]struct{}) map[string]struct{} {
	res := make(map[string]struct{})
	min := -1

	for w := range words {
		if min == -1 || len(w) < min {
			min = len(w)
		}
	}

	for w := range words {
		if len(w) == min {
			res[w] = struct{}{}
		}
	}

	return res
}

func terminateNTerm(nterm rule_types.Term, rules rule_types.Rule, used map[string]struct{}) map[string]struct{} {
	res := make(map[string]struct{})
	if _, ok := used[nterm.Name]; ok {
		return res
	}

	for _, rule := range rules[nterm.Name] {
		words := map[string]struct{}{
			"": {},
		}

		for _, term := range rule {
			if rule_types.IsTerm(term.Name) {
				words = addWord(words, term.Name)
			} else {
				used[nterm.Name] = struct{}{}
				suff := terminateNTerm(term, rules, used)
				words = crossProduct(words, suff)
			}
		}

		for w := range words {
			res[w] = struct{}{}
		}
	}

	return getWordsWithMinLen(res)
}

func checkBelonging(word string, rules rule_types.Rule, suffix, initial []rule_types.Term) bool {
	if len(suffix) == 0 {
		if word == "" {
			return true
		}
		return checkBelonging(word, rules, initial, initial)
	}

	if word == "" {
		return false
	}

	term := suffix[0]

	if rule_types.IsTerm(term.Name) {
		if word[:1] != term.Name {
			return false
		}
		return checkBelonging(word[1:], rules, suffix[1:], initial)
	}

	for _, rule := range rules[term.Name] {
		if checkBelonging(word, rules, append(rule, suffix[1:]...), initial) {
			return true
		}
	}

	return false
}

func checkAnotherNTerms(rules rule_types.Rule, regularNTerms, maybeRegularNTerms map[string]struct{},
) (map[string]struct{}, map[string]struct{}) {
	needToRepeat := true
	for needToRepeat {
		needToRepeat = false
		for nterm := range rules {
			if _, ok := regularNTerms[nterm]; ok {
				continue
			}
			if _, ok := maybeRegularNTerms[nterm]; ok {
				continue
			}

			isRegular, isMaybeRegular := true, true
			for _, rule := range rules[nterm] {
				for _, term := range rule {
					if rule_types.IsTerm(term.Name) {
						continue
					}

					if _, ok := regularNTerms[term.Name]; !ok {
						isRegular = false
					}
					if _, ok := maybeRegularNTerms[term.Name]; !ok {
						isMaybeRegular = false
					}
				}
			}

			if isRegular {
				regularNTerms[nterm] = struct{}{}
				needToRepeat = true
			} else if isMaybeRegular {
				maybeRegularNTerms[nterm] = struct{}{}
				needToRepeat = true
			}
		}
	}

	return regularNTerms, maybeRegularNTerms
}
