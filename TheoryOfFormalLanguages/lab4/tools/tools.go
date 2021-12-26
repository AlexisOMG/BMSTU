package tools

import (
	"fmt"
	"strings"

	rgsystem "github.com/AlexisOMG/tfl-lab2/rgsystem"
	"github.com/AlexisOMG/tfl-lab4/rules"
	"github.com/google/uuid"
)

type Graph map[rules.Term][]rules.Term

func GraphFromRules(rls rules.Rules) Graph {
	res := make(Graph)

	for left, rights := range rls {
		children := make(map[rules.Term]struct{})
		for _, right := range rights {
			for _, term := range right {
				if rules.IsNterm(term) {
					children[term] = struct{}{}
				}
			}
		}
		for k := range children {
			res[left] = append(res[left], k)
		}
	}

	return res
}

func (graph Graph) Reverse() Graph {
	res := make(Graph)
	for l, rps := range graph {
		for _, rp := range rps {
			res[rp] = append(res[rp], l)
		}
	}
	return res
}

func Dfs(graph Graph, used *map[rules.Term]struct{}, v rules.Term) {
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

func GetIrregular(rls rules.Rules, mode string) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})
	for nterm, rule := range rls {
		for _, r := range rule {
			isChain := rules.IsChain(r)
			isRegularR := rules.IsRegularR(r)
			isRegularL := rules.IsRegularL(r)
			if !(isChain || (mode == "L" && isRegularL) || (mode == "R" && isRegularR)) {
				res[nterm] = struct{}{}
			}
		}
	}
	return res
}

func getRegular(rls rules.Rules, mode string) map[rules.Term]struct{} {
	used := make(map[rules.Term]struct{})
	graph := GraphFromRules(rls)
	rgraph := graph.Reverse()
	irregulars := GetIrregular(rls, mode)

	for irregular := range irregulars {
		if _, ok := used[irregular]; !ok {
			Dfs(rgraph, &used, irregular)
		}
	}

	nterms := make(map[rules.Term]struct{})
	for nterm := range rls {
		nterms[nterm] = struct{}{}
	}

	return complement(nterms, used)
}

func GetRegular(rls rules.Rules) (regularR, regularL, closing map[rules.Term]struct{}) {
	regularR = getRegular(rls, "R")
	regularL = getRegular(rls, "L")
	closing = buildClosing(rls, regularR, regularL)

	return regularR, regularL, closing
}

func complement(a map[rules.Term]struct{}, b map[rules.Term]struct{}) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})

	for k := range a {
		if _, ok := b[k]; !ok {
			res[k] = struct{}{}
		}
	}

	return res
}

func inClosing(nterm rules.Term, closing, regularR, regularL map[rules.Term]struct{}) bool {
	_, isRegularR := regularR[nterm]
	_, isRegularL := regularL[nterm]
	_, isInClosing := closing[nterm]

	return isInClosing || isRegularL || isRegularR
}

func buildClosing(rls rules.Rules, regularR, regularL map[rules.Term]struct{}) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})

	updated := true

	for updated {
		updated = false

		for nterm := range rls {
			if inClosing(nterm, res, regularR, regularL) {
				continue
			}

			isRegular := true

			for _, rule := range rls[nterm] {
				for _, r := range rule {
					if !inClosing(r, res, regularR, regularL) && !rules.IsTerm(r) {
						isRegular = false
					}
				}
			}

			if isRegular {
				res[nterm] = struct{}{}
				updated = true
			}
		}
	}

	return res
}

func makeGuard() rules.Term {
	return rules.Term(fmt.Sprintf("[GuardianAngel%s]", uuid.NewString()))
}

func copy(a *map[rules.Term]struct{}, b map[rules.Term]struct{}) {
	(*a) = make(map[rules.Term]struct{})
	for k := range b {
		(*a)[k] = struct{}{}
	}
}

func copyRules(a *rules.Rules, b rules.Rules) {
	(*a) = make(rules.Rules)
	for k, v := range b {
		(*a)[k] = v
	}
}

func WrapInGuards(rls rules.Rules, regularR, regularL, closing map[rules.Term]struct{},
) (nrls rules.Rules, nregularR, nregularL, nclosing map[rules.Term]struct{}) {
	copy(&nregularL, regularL)
	copy(&nregularR, regularR)
	copy(&nclosing, closing)
	copyRules(&nrls, rls)
	// nrls, nregularR, nregularL, nclosing = rls, regularR, regularL, closing

	guarded := make(map[rules.Term]rules.Term)

	for nterm := range nrls {
		_, isRegularR := nregularR[nterm]
		_, isRegularL := nregularL[nterm]
		if !isRegularR && !isRegularL {
			for i, rule := range nrls[nterm] {
				for j, term := range rule {
					if rules.IsTerm(term) {
						if _, ok := guarded[term]; !ok {
							guarded[term] = makeGuard()
						}
						nrls[nterm][i][j] = guarded[term]
					}
				}
			}
		}
	}

	for nterm, guard := range guarded {
		nrls[guard] = [][]rules.Term{{nterm}}
		nregularL[guard] = struct{}{}
		nregularR[guard] = struct{}{}
	}
	return
}

func BuildFIRST(rls rules.Rules) map[rules.Term]map[rules.Term]struct{} {
	res := make(map[rules.Term]map[rules.Term]struct{})

	for nterm, rule := range rls {
		for _, r := range rule {
			if rules.IsTerm(r[0]) {
				if _, ok := res[nterm]; !ok {
					res[nterm] = make(map[rules.Term]struct{})
				}
				res[nterm][r[0]] = struct{}{}
			}
		}
	}

	updated := true

	for updated {
		updated = false

		for nterm, rule := range rls {
			for _, r := range rule {
				if rules.IsNterm(r[0]) {
					a := res[nterm]
					if a == nil {
						a = make(map[rules.Term]struct{})
					}
					b := res[r[0]]
					if b == nil {
						b = make(map[rules.Term]struct{})
					}
					u := union(a, b)
					if len(complement(b, a)) > 0 {
						updated = true
					}
					res[nterm] = u
				}
			}
		}
	}

	return res
}

func BuildLAST(rls rules.Rules) map[rules.Term]map[rules.Term]struct{} {
	res := make(map[rules.Term]map[rules.Term]struct{})

	for nterm, rule := range rls {
		for _, r := range rule {
			if rules.IsTerm(r[len(r)-1]) {
				if _, ok := res[nterm]; !ok {
					res[nterm] = make(map[rules.Term]struct{})
				}
				res[nterm][r[len(r)-1]] = struct{}{}
			}
		}
	}

	updated := true

	for updated {
		updated = false

		for nterm, rule := range rls {
			for _, r := range rule {
				if rules.IsNterm(r[len(r)-1]) {
					a := res[nterm]
					if a == nil {
						a = make(map[rules.Term]struct{})
					}
					b := res[r[len(r)-1]]
					if b == nil {
						b = make(map[rules.Term]struct{})
					}
					u := union(a, b)
					if len(complement(b, a)) > 0 {
						updated = true
					}
					res[nterm] = u
				}
			}
		}
	}

	return res
}

func union(a, b map[rules.Term]struct{}) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})
	for k := range a {
		res[k] = struct{}{}
	}
	for k := range b {
		res[k] = struct{}{}
	}
	return res
}

func BuildFOLLOW(rls rules.Rules, first map[rules.Term]map[rules.Term]struct{}) map[rules.Term]map[rules.Term]struct{} {
	res := map[rules.Term]map[rules.Term]struct{}{
		rules.Term("[S]"): {
			"$": {},
		},
	}

	updated := true

	for updated {
		updated = false

		for nterms, rule := range rls {
			for _, r := range rule {
				for j, term := range r {
					if rules.IsTerm(term) {
						continue
					}

					a := res[term]
					if a == nil {
						a = make(map[rules.Term]struct{})
					}

					b := make(map[rules.Term]struct{})

					ind := j + 1

					if len(r) == j+1 {
						if _, ok := res[nterms]; ok {
							b = res[nterms]
						} else {
							b = make(map[rules.Term]struct{})
						}
					} else if rules.IsTerm(r[ind]) {
						b = map[rules.Term]struct{}{
							r[ind]: {},
						}
					} else {
						if _, ok := first[r[ind]]; ok {
							b = first[r[ind]]
						} else {
							b = make(map[rules.Term]struct{})
						}
					}
					if len(complement(b, a)) > 0 {
						updated = true
					}
					res[term] = union(a, b)
				}
			}
		}
	}

	return res
}

func BuildPRECEDE(rls rules.Rules, last map[rules.Term]map[rules.Term]struct{}) map[rules.Term]map[rules.Term]struct{} {
	res := map[rules.Term]map[rules.Term]struct{}{
		rules.Term("[S]"): {
			"^": {},
		},
	}

	updated := true

	for updated {
		updated = false

		for nterms, rule := range rls {
			for _, r := range rule {
				for j, term := range r {
					if rules.IsTerm(term) {
						continue
					}

					a := res[term]
					if a == nil {
						a = make(map[rules.Term]struct{})
					}

					b := make(map[rules.Term]struct{})

					ind := j - 1

					if j == 0 {
						if _, ok := res[nterms]; ok {
							b = res[nterms]
						} else {
							b = make(map[rules.Term]struct{})
						}
					} else if rules.IsTerm(r[ind]) {
						b = map[rules.Term]struct{}{
							r[ind]: {},
						}
					} else {
						if _, ok := last[r[ind]]; ok {
							b = last[r[ind]]
						} else {
							b = make(map[rules.Term]struct{})
						}
					}
					if len(complement(b, a)) > 0 {
						updated = true
					}
					res[term] = union(a, b)
				}
			}
		}
	}

	return res
}

func FindTokens(rls rules.Rules, regularR, regularL, closing map[rules.Term]struct{},
	follow, precede map[rules.Term]map[rules.Term]struct{}) (map[rules.Term]struct{}, []error) {
	regulars := union(union(regularL, regularR), closing)
	res := make(map[rules.Term]struct{})

	var errors []error

	for nterm := range regulars {
		flw, ok1 := follow[nterm]
		prc, ok2 := precede[nterm]
		if !ok1 {
			flw = make(map[rules.Term]struct{})
		}
		if !ok2 {
			prc = make(map[rules.Term]struct{})
		}

		terms := union(flw, prc)

		produce := producing(rls, nterm, make(map[rules.Term]struct{}))

		inter := intersec(terms, produce)

		if len(inter) == 0 {
			res[nterm] = struct{}{}
		} else {
			errors = append(errors, fmt.Errorf("conflict for nterm %s", nterm))
		}
	}

	return res, errors
}

func producing(rls rules.Rules, nterm rules.Term, used map[rules.Term]struct{}) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})

	rule := rls[nterm]

	for _, r := range rule {
		if len(r) == 1 {
			if rules.IsTerm(r[0]) {
				res[r[0]] = struct{}{}
			} else if _, ok := used[r[0]]; !ok {
				used[nterm] = struct{}{}
				res = union(res, producing(rls, r[0], used))
			}
		}
	}

	return res
}

func intersec(a, b map[rules.Term]struct{}) map[rules.Term]struct{} {
	intersection := make(map[rules.Term]int)

	for k := range a {
		intersection[k] += 1
	}
	for k := range b {
		intersection[k] += 1
	}

	res := make(map[rules.Term]struct{})
	for k, v := range intersection {
		if v > 1 {
			res[k] = struct{}{}
		}
	}

	return res
}

func getProducing(rls rules.Rules) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})

	finding := true

	for finding {
		finding = false
		for nterm, rule := range rls {
			for _, r := range rule {
				isProduce := true
				for _, t := range r {
					if _, ok := res[t]; !(rules.IsTerm(t) || ok) {
						isProduce = false
					}
				}
				if _, ok := res[nterm]; isProduce && !ok {
					finding = true
					res[nterm] = struct{}{}
				}
			}
		}
	}

	return res
}

func toDict(eqs []rgsystem.Eqution) map[rules.Term]rgsystem.Eqution {
	res := make(map[rules.Term]rgsystem.Eqution)
	for _, eq := range eqs {
		res[rules.Term(eq.EqutionVar.Name)] = eq
	}
	return res
}

func BuildRegex(rls rules.Rules, regularR, regularL, closing, tokens map[rules.Term]struct{}) (map[rules.Term]string, error) {
	rgsystem.LeftEqutionVars = make(map[string]struct{})

	res := make(map[rules.Term]string)

	prod := getProducing(rls)
	eqs := buildEqutions(rls, intersec(regularR, prod))

	for _, eq := range eqs {
		rgsystem.LeftEqutionVars[eq.EqutionVar.Name] = struct{}{}
	}

	solving, err := rgsystem.Solve(eqs)
	if err != nil {
		return nil, err
	}

	solutions := toDict(solving)

	for nterm := range regularR {
		if _, ok := prod[nterm]; ok {
			res[nterm] = strings.Split(rgsystem.PrintSolving([]rgsystem.Eqution{solutions[nterm]})[0], "= ")[1]
		} else {
			res[nterm] = "non-productive"
		}
	}

	for nterm := range complement(regularL, regularR) {
		if _, ok := prod[nterm]; ok {
			result := make(rules.Rules)
			convertToRegularR(nterm, "", rls, &result, make(map[rules.Term]struct{}))
			product := getProducing(result)
			eqs = buildEqutions(result, intersec(getNterms(result), product))
			for _, eq := range eqs {
				rgsystem.LeftEqutionVars[eq.EqutionVar.Name] = struct{}{}
			}
			solving, err = rgsystem.Solve(eqs)
			if err != nil {
				return nil, err
			}
			solutions = toDict(solving)
			res[nterm] = strings.Split(rgsystem.PrintSolving([]rgsystem.Eqution{solutions["S'"]})[0], "= ")[1]
		} else {
			res[nterm] = "non-productive"
		}
	}

	for nterm := range closing {
		if _, ok := prod[nterm]; !ok {
			res[nterm] = "non-productive"
		} else {
			if _, ok := res[nterm]; !ok {
				buildRegexpClosing(nterm, rls, prod, &res)
			}
		}
	}

	answ := make(map[rules.Term]string)

	for nterm := range tokens {
		answ[nterm] = res[nterm]
	}

	return answ, nil
}

func buildRegexpClosing(nterm rules.Term, rls rules.Rules, prod map[rules.Term]struct{}, regs *map[rules.Term]string,
) {
	res := ""
	for _, rule := range rls[nterm] {
		curr := ""
		find := true
		for _, term := range rule {
			if rules.IsTerm(term) {
				curr += string(term)
			} else if _, ok := prod[term]; ok {
				if _, ok := (*regs)[term]; !ok {
					buildRegexpClosing(term, rls, prod, regs)
				}
				curr += (*regs)[term]
			} else {
				find = false
				break
			}
		}
		curr = fmt.Sprintf("(%s)", curr)
		if find {
			if res == "" {
				res = curr
			} else {
				res = fmt.Sprintf("(%s+%s)", res, curr)
			}
		}
	}
	(*regs)[nterm] = res
}

func convertToRegularR(nterm, start rules.Term, rls rules.Rules, res *rules.Rules, used map[rules.Term]struct{}) {
	if _, ok := used[nterm]; ok {
		return
	}

	if start == "" {
		convertToRegularR(nterm, nterm, rls, res, used)
		return
	}

	for _, rule := range rls[nterm] {
		if len(rule) == 1 {
			if rules.IsTerm(rule[0]) {
				(*res)[rules.Term("S'")] = append((*res)[rules.Term("S'")], []rules.Term{rule[0], nterm})
				if nterm == start {
					(*res)[rules.Term("S'")] = append((*res)[rules.Term("S'")], []rules.Term{rule[0]})
				}
			} else {
				used[nterm] = struct{}{}
				convertToRegularR(rule[0], start, rls, res, used)
				(*res)[rule[0]] = append((*res)[rule[0]], []rules.Term{nterm})
			}
		} else {
			used[nterm] = struct{}{}
			convertToRegularR(rule[0], start, rls, res, used)
			(*res)[rules.Term(rule[0])] = append((*res)[rule[0]], []rules.Term{rule[1], nterm})
			if nterm == start {
				(*res)[rule[0]] = append((*res)[rule[0]], []rules.Term{rule[1]})
			}
		}
	}
}

func buildEqutions(rls rules.Rules, nterms map[rules.Term]struct{}) []rgsystem.Eqution {
	var res []rgsystem.Eqution

	for nterm := range nterms {
		eq := rgsystem.Eqution{
			EqutionVar: rgsystem.Var{
				Name: string(nterm),
			},
		}
		for _, r := range rls[nterm] {
			if len(r) == 1 {
				if rules.IsTerm(r[0]) {
					eq.EqutionArgs = append(eq.EqutionArgs, rgsystem.EqutionArg{
						Regexp: rgsystem.RegExpr{
							Expr: []rgsystem.Expression{
								{
									Value: string(escape(r[0])),
								},
							},
						},
					})
				} else if _, ok := nterms[r[0]]; ok {
					eq.EqutionArgs = append(eq.EqutionArgs, rgsystem.EqutionArg{
						Regexp: rgsystem.RegExpr{
							Expr: []rgsystem.Expression{
								{
									Value: "()",
								},
							},
						},
						ArgVar: rgsystem.Var{
							Name: string(r[0]),
						},
					})
				}
			} else if _, ok := nterms[r[1]]; ok {
				eq.EqutionArgs = append(eq.EqutionArgs, rgsystem.EqutionArg{
					Regexp: rgsystem.RegExpr{
						Expr: []rgsystem.Expression{
							{
								Value: string(escape(r[0])),
							},
						},
					},
					ArgVar: rgsystem.Var{
						Name: string(r[1]),
					},
				})
			}
		}
		res = append(res, eq)
	}

	return res
}

func escape(t rules.Term) rules.Term {
	esc := map[rules.Term]rules.Term{
		rules.Term("*"): rules.Term("(\\*)"),
		rules.Term("+"): rules.Term("(\\+)"),
		rules.Term("("): rules.Term("(\\()"),
		rules.Term(")"): rules.Term("(\\))"),
		rules.Term("$"): rules.Term("(\\$)"),
	}

	res, ok := esc[t]
	if !ok {
		return t
	}

	return res
}

func getNterms(rls rules.Rules) map[rules.Term]struct{} {
	res := make(map[rules.Term]struct{})
	for nterm := range rls {
		res[nterm] = struct{}{}
	}
	return res
}
