package common

const (
	Eps   = "eps"
	Term  = "term"
	NTerm = "nterm"
)

type Expr struct {
	Value string `json:"value"`
	Kind  string `json:"kind"`
}

var (
	Epsilon = Expr{
		Value: Eps,
		Kind:  Eps,
	}

	Dollar = Expr{
		Value: "Dollar",
		Kind:  Term,
	}

	Error = Expr{
		Value: "Error",
		Kind:  "Error",
	}
)

type Rules map[Expr][][]Expr

func unionWithEps(a, b map[Expr]struct{}) map[Expr]struct{} {
	res := make(map[Expr]struct{}, len(a)-1+len(b))
	for e := range a {
		if e != Epsilon {
			res[e] = struct{}{}
		}
	}

	for e := range b {
		res[e] = struct{}{}
	}

	return res
}

func F(seq []Expr, first map[Expr]map[Expr]struct{}) map[Expr]struct{} {
	if len(seq) == 0 || len(seq) > 0 && seq[0] == Epsilon {
		return map[Expr]struct{}{
			Epsilon: {},
		}
	}

	if seq[0].Kind == Term {
		return map[Expr]struct{}{
			seq[0]: {},
		}
	}

	if _, ok := first[seq[0]][Epsilon]; !ok {
		// copy?
		return first[seq[0]]
	}

	return unionWithEps(first[seq[0]], F(seq[1:], first))
}

func First(rls Rules) map[Expr]map[Expr]struct{} {
	res := make(map[Expr]map[Expr]struct{}, len(rls))

	for l := range rls {
		res[l] = make(map[Expr]struct{})
	}

	changed := true

	for changed {
		changed = false

		for l := range res {
			for _, exprs := range rls[l] {
				f := F(exprs, res)
				for e := range f {
					if _, ok := res[l][e]; !ok {
						res[l][e] = struct{}{}
						changed = true
					}
				}
			}
		}
	}

	return res
}

func Follow(rls Rules, axiom Expr, first map[Expr]map[Expr]struct{}) map[Expr]map[Expr]struct{} {
	res := make(map[Expr]map[Expr]struct{}, len(rls))

	for l := range rls {
		res[l] = make(map[Expr]struct{})
	}

	res[axiom][Dollar] = struct{}{}

	for l := range rls {
		for _, exprs := range rls[l] {
			for i, e := range exprs {
				if e.Kind == NTerm {
					for j := i + 1; j < len(exprs); j++ {
						if exprs[j] == Epsilon {
							continue
						}

						if exprs[j].Kind == Term {
							res[e][exprs[j]] = struct{}{}
							break
						} else {
							firstV := first[exprs[j]]
							for t := range firstV {
								if t != Epsilon {
									res[e][t] = struct{}{}
								}
							}
							if _, ok := firstV[Epsilon]; !ok {
								break
							}
						}
					}
				}
			}
		}
	}

	changed := true

	for changed {
		changed = false

		for l := range rls {
			for _, exprs := range rls[l] {
				for j, e := range exprs {
					if e.Kind == NTerm {
						if j == len(exprs)-1 {
							for t := range res[l] {
								if _, ok := res[e][t]; !ok {
									res[e][t] = struct{}{}
									changed = true
								}
							}
						} else {
							allEps := true
							for k := j + 1; k < len(exprs); k++ {
								if exprs[k].Kind == Term {
									break
								}
								if _, ok := first[exprs[k]][Epsilon]; !ok {
									allEps = false
								}
							}
							if allEps {
								for t := range res[l] {
									if _, ok := res[e][t]; !ok {
										res[e][t] = struct{}{}
										changed = true
									}
								}
							}
						}
					}
				}
			}
		}
	}

	return res
}

type Table map[Expr]map[Expr][][]Expr

func BuildTable(rls Rules, axiom Expr, terminals []Expr) Table {
	first := First(rls)
	follow := Follow(rls, axiom, first)
	res := make(Table, len(rls))
	terminals = append(terminals, Dollar)

	for l := range rls {
		res[l] = make(map[Expr][][]Expr, len(terminals))
		for _, t := range terminals {
			res[l][t] = [][]Expr{
				{Error},
			}
		}
	}

	for l := range rls {
		for _, exprs := range rls[l] {
			allEps := true
			if !(len(exprs) == 1 && exprs[0] == Epsilon) {
				for _, e := range exprs {
					if e.Kind == Term {
						if res[l][e][0][0] == Error {
							res[l][e] = [][]Expr{exprs}
						} else {
							res[l][e] = append(res[l][e], exprs)
						}
						allEps = false
						break
					} else {
						firstV := first[e]
						if _, ok := firstV[Epsilon]; !ok {
							allEps = false
						}
						for t := range firstV {
							if t != Epsilon {
								if res[l][t][0][0] == Error {
									res[l][t] = [][]Expr{exprs}
								} else {
									res[l][t] = append(res[l][t], exprs)
								}
							}
						}
						if !allEps {
							break
						}
					}
				}
			}
			if allEps {
				for t := range follow[l] {
					if res[l][t][0][0] == Error {
						res[l][t] = [][]Expr{exprs}
					} else {
						res[l][t] = append(res[l][t], exprs)
					}
				}
			}
		}
	}
	return res
}
