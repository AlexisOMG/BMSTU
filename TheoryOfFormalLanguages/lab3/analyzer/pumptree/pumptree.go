package pumptree

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/AlexisOMG/tfl-lab3/analyzer/rule_types"
	"github.com/AlexisOMG/tfl-lab3/analyzer/tools"
)

var errorNoPumping = errors.New("no pumping for the nterm")

type Node struct {
	Term  rule_types.Term
	pumps []Node
}

func (n *Node) Print(label *int, parent_label int) string {
	res := fmt.Sprintf("\t%d [label=\"%s\"]\n", *label, n.Term.Name)
	if parent_label != -1 {
		res += fmt.Sprintf("\t%d -> %d\n", parent_label, *label)
	}
	tmp_label := *label
	*label += 1
	for _, child := range n.pumps {
		res += child.Print(label, tmp_label)
	}
	if parent_label == -1 {
		res = fmt.Sprintf("digraph %s {\n%s}\n", n.Term.Name, res)
	}
	return res
}

func (n Node) GetPumping() string {
	if len(n.pumps) == 0 {
		return n.Term.Name
	}

	res := ""
	for _, c := range n.pumps {
		res += c.GetPumping()
	}

	return res
}

func BuildLeftPumpingTree(rules rule_types.Rule) map[string]Node {
	res := make(map[string]Node)
	reachable := tools.GetReachable(rules)

	for ntermName := range reachable {
		nterm := rule_types.Term{
			Name: ntermName,
		}
		node, err := PumpNterm(nterm, rules, nterm, make(map[string]struct{}))
		if err == nil {
			res[ntermName] = *node
		}
	}

	return res
}

func PumpNterm(nterm rule_types.Term, rules rule_types.Rule, start rule_types.Term,
	used map[string]struct{}) (*Node, error) {
	if rule_types.IsTerm(nterm.Name) {
		return &Node{
			Term: nterm,
		}, errorNoPumping
	}

	if _, ok := used[nterm.Name]; ok {
		if reflect.DeepEqual(nterm, start) {
			return &Node{
				Term: nterm,
			}, nil
		}
		return nil, errorNoPumping
	}

	for _, rule := range rules[nterm.Name] {
		err := errorNoPumping
		children := make([]Node, 0)
		var node *Node
		for _, term := range rule {
			if err == nil {
				children = append(children, Node{
					Term: term,
				})
			} else {
				// tmpUsed := used
				used[nterm.Name] = struct{}{}
				node, err = PumpNterm(term, rules, start, used)
				if node == nil {
					break
				}
				children = append(children, *node)
			}
		}
		if err == nil {
			return &Node{
				Term:  nterm,
				pumps: children,
			}, nil
		}
	}

	return nil, errorNoPumping
}
