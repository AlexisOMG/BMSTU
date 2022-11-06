package parser

import (
	"fmt"
	"strings"

	"github.com/AlexisOMG/compilers-lab7-1/common"
	"github.com/AlexisOMG/compilers-lab7-1/lexer"
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

func Parse(table map[common.Expr]map[common.Expr][][]common.Expr, lex *lexer.Lexer, axiom common.Expr) (*Node, error) {
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
		x := st[len(st)-1]
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

	return fakeRoot.Children[0], nil
}
