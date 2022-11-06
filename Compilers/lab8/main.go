package main

import (
	"fmt"
	"log"
	"os"

	"github.com/AlexisOMG/compilers-lab8/lexer"
)

type Node struct {
	token     *lexer.Token
	children  []*Node
	iteration string
	ruleType  string
}

// $RULE Init = Nterms Terms Rules
func ParseInit(lex *lexer.Lexer) (*Node, error) {
	root := &Node{}

	nterms, err := parseNTerms(lex)
	if err != nil {
		return nil, err
	}
	root.children = append(root.children, nterms)

	terms, err := parseTerms(lex)
	if err != nil {
		return nil, err
	}
	root.children = append(root.children, terms)

	rules, err := parseRules(lex)
	if err != nil {
		return nil, err
	}
	root.children = append(root.children, rules)

	return root, nil
}

// $RULE Nterms = "$NTERM" { "nterm" }
func parseNTerms(lex *lexer.Lexer) (*Node, error) {
	token := lex.NextToken()
	if token.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program in parseNTerms")
	}
	if token.Kind != lexer.NTermKeyword {
		return nil, fmt.Errorf("unexpeted token: %v, expected $NTERM", token)
	}
	node := &Node{
		token: &token,
	}
	for lex.HasNext() {
		child := lex.Peek()
		if child.Kind != lexer.Nterm {
			break
		}
		node.children = append(node.children, &Node{
			token: &child,
		})
		lex.NextToken()
	}

	return node, nil
}

// $RULE Terms = "$TERM" { "nterm" }
func parseTerms(lex *lexer.Lexer) (*Node, error) {
	token := lex.NextToken()
	if token.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program in parseTerms")
	}
	if token.Kind != lexer.TermKeyword {
		return nil, fmt.Errorf("unexpeted token: %v, expected $TERM", token)
	}
	node := &Node{
		token: &token,
	}
	for lex.HasNext() {
		child := lex.Peek()
		if child.Kind != lexer.Term {
			break
		}
		node.children = append(node.children, &Node{
			token: &child,
		})
		lex.NextToken()
	}

	return node, nil
}

// $RULE Rules = { "$RULE" "nterm" "=" RulesOr }
func parseRules(lex *lexer.Lexer) (*Node, error) {
	token := lex.Peek()
	if token.Kind == lexer.EOF {
		return nil, nil
	}
	node := &Node{}
	for lex.HasNext() {
		child, err := parseRule(lex)
		if err != nil {
			return nil, err
		}
		node.children = append(node.children, child)
	}
	return node, nil
}

// "$RULE" "nterm" "=" RulesOr
func parseRule(lex *lexer.Lexer) (*Node, error) {
	token := lex.NextToken()
	if token.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program")
	}
	if token.Kind != lexer.RuleKeyword {
		return nil, fmt.Errorf("unexpeted token: %v, expected $RULE", token)
	}

	token = lex.NextToken()
	if token.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program")
	}
	if token.Kind != lexer.Nterm {
		return nil, fmt.Errorf("unexpeted token: %v, expected nterm", token)
	}
	node := &Node{
		token: &token,
	}

	tok := lex.NextToken()
	if tok.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program")
	}
	if tok.Kind != lexer.Equal {
		return nil, fmt.Errorf("unexpeted token: %v, expected =", tok)
	}

	nd, err := parseRulesOr(lex)
	if err != nil {
		return nil, err
	}
	node.children = append(node.children, nd)

	return node, nil
}

// $RULE RulesOr = RulesAnd { "|" RulesAnd }
func parseRulesOr(lex *lexer.Lexer) (*Node, error) {
	token := lex.Peek()
	if token.Kind == lexer.EOF {
		return nil, fmt.Errorf("unexpected end of program")
	}
	if token.Kind == lexer.Error {
		return nil, fmt.Errorf("lex error: %v", token)
	}
	if token.Kind == lexer.RuleKeyword {
		return nil, fmt.Errorf("unexpected end of rule")
	}

	node := &Node{}

	child, err := parseRulesAnd(lex)
	if err != nil {
		return nil, err
	}
	node.children = append(node.children, child)

	for lex.HasNext() {
		tok := lex.Peek()
		if tok.Kind == lexer.Alternative {
			node.ruleType = "OR"
			lex.NextToken()
			child, err := parseRulesAnd(lex)
			if err != nil {
				return nil, err
			}
			node.children = append(node.children, child)
		} else if tok.Kind == lexer.EOF || tok.Kind == lexer.RuleKeyword ||
			tok.Kind == lexer.Close1 || tok.Kind == lexer.Close2 || tok.Kind == lexer.Close3 {
			return node, nil
		} else {
			return nil, fmt.Errorf("unexpected token in RulesOr: %v", tok)
		}
	}

	return node, nil
}

// $RULE RulesAnd = Expr RulesAnd |
func parseRulesAnd(lex *lexer.Lexer) (*Node, error) {
	token := lex.Peek()
	if token.Kind == lexer.EOF || token.Kind == lexer.RuleKeyword || token.Kind == lexer.Alternative ||
		token.Kind == lexer.Close1 || token.Kind == lexer.Close2 || token.Kind == lexer.Close3 {
		return nil, nil
	}
	node := &Node{
		ruleType: "AND",
	}

	child, err := parseExpr(lex)
	if err != nil {
		return nil, err
	}

	node.children = append(node.children, child)

	nd, err := parseRulesAnd(lex)
	if err != nil {
		return nil, err
	}
	if nd != nil {
		node.children = append(node.children, nd)
	}
	return node, nil
}

// $RULE Expr = "nterm" | "term" | "{" RulesOr "}" | "[" RulesOr "]" | "(" RulesOr ")"
// {braces}, [brackets], (parens, parenthesis)
// OpenIter, OpenOpt, OpenGroup, Close...
func parseExpr(lex *lexer.Lexer) (*Node, error) {
	token := lex.NextToken()
	switch token.Kind {
	case lexer.Nterm:
		return &Node{
			token: &token,
		}, nil
	case lexer.Term:
		return &Node{
			token: &token,
		}, nil
	case lexer.Open2:
		tok, err := parseRulesOr(lex)
		if err != nil {
			return nil, err
		}
		bracket := lex.NextToken()
		if bracket.Kind != lexer.Close2 {
			return nil, fmt.Errorf("unexpeted token: %v, expected }", bracket)
		}
		tok.iteration = "*"
		return tok, nil
	case lexer.Open3:
		tok, err := parseRulesOr(lex)
		if err != nil {
			return nil, err
		}
		bracket := lex.NextToken()
		if bracket.Kind != lexer.Close3 {
			return nil, fmt.Errorf("unexpeted token: %v, expected ]", bracket)
		}
		tok.iteration = "?"
		return tok, nil
	case lexer.Open1:
		tok, err := parseRulesOr(lex)
		if err != nil {
			return nil, err
		}
		bracket := lex.NextToken()
		if bracket.Kind != lexer.Close1 {
			return nil, fmt.Errorf("unexpeted token: %v, expected )", bracket)
		}
		return tok, nil
	default:
		return nil, fmt.Errorf("unexpected token in expr: %v", token)
	}
}

func hasEps(a map[lexer.Expr]struct{}) bool {
	for k := range a {
		if k == lexer.EpsilonToken {
			return true
		}
	}
	return false
}

func withoutEps(a map[lexer.Expr]struct{}) map[lexer.Expr]struct{} {
	res := make(map[lexer.Expr]struct{}, len(a)-1)
	for k := range a {
		if k != lexer.EpsilonToken {
			res[k] = struct{}{}
		}
	}
	return res
}

func F(node *Node, first map[lexer.Expr]map[lexer.Expr]struct{}) map[lexer.Expr]struct{} {
	if node == nil {
		return map[lexer.Expr]struct{}{
			lexer.EpsilonToken: {},
		}
	}
	if node.token != nil {
		if node.token.Kind == lexer.Term {
			return map[lexer.Expr]struct{}{
				node.token.ToExpr(): {},
			}
		} else if node.token.Kind == lexer.Nterm {
			return first[node.token.ToExpr()]
		}
	}
	if node.iteration == "*" || node.iteration == "?" {
		res := F(node.children[0], first)
		res[lexer.EpsilonToken] = struct{}{}
		return res
	}
	if node.ruleType == "AND" {
		uFirst := F(node.children[0], first)
		if !hasEps(uFirst) {
			return uFirst
		}
		uFirst = withoutEps(uFirst)
		if len(node.children) > 1 {
			vFirst := F(node.children[1], first)

			for k := range vFirst {
				uFirst[k] = struct{}{}
			}
		}
		return uFirst
	} else if node.ruleType == "OR" {
		res := make(map[lexer.Expr]struct{})
		for _, child := range node.children {
			f := F(child, first)
			for k := range f {
				res[k] = struct{}{}
			}
		}
		return res
	}

	return F(node.children[0], first)
}

func buildFirst(root *Node) map[lexer.Expr]map[lexer.Expr]struct{} {
	var nterms []lexer.Token
	for _, node := range root.children[0].children {
		nterms = append(nterms, *node.token)
	}

	res := make(map[lexer.Expr]map[lexer.Expr]struct{}, len(nterms))
	for _, nterm := range nterms {
		res[nterm.ToExpr()] = make(map[lexer.Expr]struct{})
	}

	rules := root.children[2].children
	changed := true
	for changed {
		changed = false
		for _, rule := range rules {
			first := F(rule.children[0], res)
			for k := range first {
				if _, ok := res[rule.token.ToExpr()][k]; !ok {
					res[rule.token.ToExpr()][k] = struct{}{}
					changed = true
				}
			}
		}
	}

	return res
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Wrong usage")
	}
	pathToFile := os.Args[1]

	lex, err := lexer.NewLexer(pathToFile)
	if err != nil {
		log.Fatal(err)
	}

	root, err := ParseInit(lex)
	if err != nil {
		log.Fatal(err)
	}

	first := buildFirst(root)
	for k := range first {
		fmt.Printf("FIRST(%s): ", k.Value)
		for v := range first[k] {
			fmt.Print(v.Value, " ")
		}
		fmt.Println()
	}
}
