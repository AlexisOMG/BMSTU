package parser

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/AlexisOMG/compilers-lab7-2/common"
	"github.com/AlexisOMG/compilers-lab7-2/lexer"
)

var (
	Rules = common.Rules{
		common.Expr{
			Kind:  common.NTerm,
			Value: "S",
		}: [][]common.Expr{
			{
				{"AxiomKeyword", common.Term}, {"Nterm", common.Term}, {"NTermKeyword", common.Term}, {"Nterm", common.Term}, {"N", common.NTerm}, {"T", common.NTerm}, {"R", common.NTerm},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "N",
		}: [][]common.Expr{
			{
				{"Nterm", common.Term}, {"N", common.NTerm},
			},
			{
				common.Epsilon,
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "T",
		}: [][]common.Expr{
			{
				{"TermKeyword", common.Term}, {"Term", common.Term}, {"T1", common.NTerm},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "T1",
		}: [][]common.Expr{
			{
				{"Term", common.Term}, {"T1", common.NTerm},
			},
			{
				common.Epsilon,
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "R",
		}: [][]common.Expr{
			{
				{"R'", common.NTerm}, {"R1", common.NTerm},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "R1",
		}: [][]common.Expr{
			{
				{"R'", common.NTerm}, {"R1", common.NTerm},
			},
			{
				common.Epsilon,
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "R'",
		}: [][]common.Expr{
			{
				{"RuleKeyword", common.Term}, {"Nterm", common.Term}, {"Equal", common.Term}, {"V", common.NTerm},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "V",
		}: [][]common.Expr{
			{
				{"V1", common.NTerm}, {"V2", common.NTerm},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "V1",
		}: [][]common.Expr{
			{
				{"Term", common.Term}, {"V3", common.NTerm},
			},
			{
				{"Nterm", common.Term}, {"V3", common.NTerm},
			},
			{
				{"EpsKeyword", common.Term},
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "V3",
		}: [][]common.Expr{
			{
				{"Term", common.Term}, {"V3", common.NTerm},
			},
			{
				{"Nterm", common.Term}, {"V3", common.NTerm},
			},
			{
				common.Epsilon,
			},
		},
		common.Expr{
			Kind:  common.NTerm,
			Value: "V2",
		}: [][]common.Expr{
			{
				{"NewLine", common.Term}, {"V", common.NTerm},
			},
			{
				common.Epsilon,
			},
		},
	}

	Terminals = []common.Expr{
		{"AxiomKeyword", common.Term},
		{"NTermKeyword", common.Term},
		{"TermKeyword", common.Term},
		{"RuleKeyword", common.Term},
		{"EpsKeyword", common.Term},
		{"Equal", common.Term},
		{"NewLine", common.Term},
		{"Term", common.Term},
		{"Nterm", common.Term},
	}
)

type Transition struct {
	Term   common.Expr   `json:"term"`
	Nterms []common.Expr `json:"nterms"`
}

type Rule struct {
	Nterm       common.Expr  `json:"nterm"`
	Transitions []Transition `json:"transitions"`
}

type TableInfo struct {
	Axiom common.Expr `json:"axiom"`
	Rules []Rule      `json:"rules"`
}

func SaveTableInfo(pathToFile string, table common.Table, axiom common.Expr) error {
	tInfo := TableInfo{
		Axiom: axiom,
	}
	var rls []Rule
	for nterm := range table {
		rl := Rule{
			Nterm: nterm,
		}
		var trans []Transition
		for t := range table[nterm] {
			trans = append(trans, Transition{
				Term:   t,
				Nterms: table[nterm][t][0],
			})
		}
		rl.Transitions = trans
		rls = append(rls, rl)
	}
	tInfo.Rules = rls
	data, err := json.Marshal(tInfo)
	if err != nil {
		return err
	}

	ioutil.WriteFile(pathToFile, data, 0777)
	return err
}

func LoadTableFromFile(pathToFile string) (common.Table, common.Expr, error) {
	var tableInfo TableInfo
	data, err := ioutil.ReadFile(pathToFile)
	if err != nil {
		return nil, common.Expr{}, err
	}

	if err := json.Unmarshal(data, &tableInfo); err != nil {
		return nil, common.Expr{}, err
	}

	res := make(common.Table)

	for _, rls := range tableInfo.Rules {
		res[rls.Nterm] = make(map[common.Expr][][]common.Expr)
		for _, trans := range rls.Transitions {
			res[rls.Nterm][trans.Term] = append(res[rls.Nterm][trans.Term], trans.Nterms)
		}
	}

	return res, tableInfo.Axiom, nil
}

type Node struct {
	Expr     common.Expr
	Rule     []common.Expr
	Value    string
	Children []*Node
}

func (n *Node) Print(depth int) {
	fmt.Print(n.Expr.Value, " ")
	if n.Expr.Kind == common.NTerm {
		fmt.Print("-> ")
		for _, r := range n.Rule {
			fmt.Print(r.Value, " ")
		}
		// fmt.Print("\n\tChildren: ")
		// for _, child := range n.Children {
		// 	fmt.Print(child.Expr.Value, " ")
		// }
		fmt.Println()
		for _, child := range n.Children {
			fmt.Print(strings.Repeat(" ", depth))
			child.Print(depth + 1)
		}
	} else {
		fmt.Println(n.Value)
	}
}

type stackItem struct {
	expr   common.Expr
	parent *Node
}

type stack []stackItem

func Parse(lex lexer.Lexer, pathToFile string) (*Node, error) {
	table, axiom, err := LoadTableFromFile(pathToFile)
	if err != nil {
		return nil, err
	}
	var st stack
	fakeRoot := Node{
		Expr: common.Expr{
			Value: "S'",
			Kind:  common.NTerm,
		},
	}
	st = append(st, stackItem{
		expr:   common.Dollar,
		parent: &fakeRoot,
	},
		stackItem{
			expr:   axiom,
			parent: &fakeRoot,
		},
	)

	a := lex.NextToken()
	if a.Kind == lexer.Error {
		return nil, fmt.Errorf("syntax error: %v", a)
	}
	for st[len(st)-1].expr != common.Dollar {
		// fmt.Println(a.ToExpr())
		x := st[len(st)-1]
		// fmt.Println("STACK: ", stack)
		// fmt.Println("A: ", a, a.Kind.ToString())
		st = st[:len(st)-1]
		if x.expr.Kind == common.Term {
			if x.expr.Value == a.Kind.ToString() {
				x.parent.Children = append(x.parent.Children, &Node{
					Expr:  a.ToExpr(),
					Value: a.Value,
				})
				a = lex.NextToken()
				if a.Kind == lexer.Error {
					return nil, fmt.Errorf("syntax error: %v", a)
				}
			} else {
				return nil, fmt.Errorf("unexpected %s, expected: %s", a.Kind.ToString(), x.expr.Value)
			}
		} else if exprs := table[x.expr][a.ToExpr()]; exprs[0][0] != common.Error {
			node := Node{
				Expr: x.expr,
				Rule: exprs[0],
			}
			x.parent.Children = append(x.parent.Children, &node)
			for i := len(exprs[0]) - 1; i >= 0; i-- {
				if exprs[0][i] != common.Epsilon {
					st = append(st, stackItem{
						expr:   exprs[0][i],
						parent: &node,
					})
				}
			}
		} else {
			return nil, fmt.Errorf("unexpected %s, expected: %s", a.Kind.ToString(), x.expr.Value)
		}
	}

	// fmt.Println("LAST STACK: ", stack)
	return fakeRoot.Children[0], nil
}

func getAllNterms(node *Node, nterms map[common.Expr]struct{}) {
	if node.Expr.Value == "Nterm" {
		nterms[common.Expr{
			Kind:  common.NTerm,
			Value: node.Value,
		}] = struct{}{}
	}
	if len(node.Children) == 0 {
		return
	}

	for _, child := range node.Children {
		if child.Expr.Value == "Nterm" {
			nterms[common.Expr{
				Kind:  common.NTerm,
				Value: child.Value,
			}] = struct{}{}
		} else {
			getAllNterms(child, nterms)
		}
	}
}

func getAllTerms(node *Node, terms map[common.Expr]struct{}) {
	if node.Expr.Value == "Term" {
		terms[common.Expr{
			Kind:  common.Term,
			Value: node.Value,
		}] = struct{}{}
	}
	if len(node.Children) == 0 {
		return
	}

	for _, child := range node.Children {
		if child.Expr.Value == "Term" {
			terms[common.Expr{
				Kind:  common.Term,
				Value: child.Value,
			}] = struct{}{}
		} else {
			getAllTerms(child, terms)
		}
	}
}

func parseRule(node *Node, nterms, terms map[common.Expr]struct{}) ([][]common.Expr, error) {
	if len(node.Children) == 0 {
		return [][]common.Expr{}, nil
	}

	var res [][]common.Expr
	var exprs []common.Expr
	v1 := node.Children[0]
	v2 := node.Children[1]
	if len(v2.Children) != 0 {
		v2 = v2.Children[1]
	}
	for {
		if len(v1.Children) == 0 {
			break
		}
		if v1.Children[0].Value == "$EPS" {
			exprs = append(exprs, common.Epsilon)
		} else {
			term := common.Expr{
				Kind:  common.Term,
				Value: v1.Children[0].Value,
			}
			nterm := common.Expr{
				Kind:  common.NTerm,
				Value: v1.Children[0].Value,
			}
			if _, ok := terms[term]; ok {
				exprs = append(exprs, term)
			} else if _, ok := nterms[nterm]; ok {
				exprs = append(exprs, nterm)
			} else {
				return [][]common.Expr{}, fmt.Errorf("unknown token: %v", v1.Children[0])
			}
		}
		if len(v1.Children) > 1 {
			v1 = v1.Children[1]
		} else {
			break
		}
	}
	res = append(res, exprs)
	rls, err := parseRule(v2, nterms, terms)
	if err != nil {
		return [][]common.Expr{}, err
	}
	return append(res, rls...), nil
}

func BuildRules(root *Node) (common.Rules, common.Expr, []common.Expr, error) {
	calcRules := make(common.Rules)
	axiom := common.Expr{
		Kind:  common.NTerm,
		Value: root.Children[1].Value,
	}

	nterms := make(map[common.Expr]struct{})
	getAllNterms(root, nterms)

	terms := make(map[common.Expr]struct{})
	getAllTerms(root, terms)
	ruleRoot := root.Children[6]
	err := buildRules(ruleRoot, calcRules, nterms, terms)
	if err != nil {
		return nil, common.Expr{}, nil, err
	}

	terminals := make([]common.Expr, 0, len(terms))
	for t := range terms {
		terminals = append(terminals, t)
	}

	return calcRules, axiom, terminals, nil
}

func buildRules(node *Node, rules common.Rules, nterms, terms map[common.Expr]struct{}) error {
	if len(node.Children) == 0 {
		return nil
	}
	rule := node.Children[0]
	lhs := common.Expr{
		Kind:  common.NTerm,
		Value: rule.Children[1].Value,
	}
	rhs, err := parseRule(rule.Children[3], nterms, terms)
	if err != nil {
		return err
	}
	rules[lhs] = rhs
	return buildRules(node.Children[1], rules, nterms, terms)
}
