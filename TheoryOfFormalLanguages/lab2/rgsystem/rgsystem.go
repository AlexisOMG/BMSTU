package rgsystem

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

var LeftEqutionVars map[string]struct{}

type Var struct {
	Name string
}

type Expression struct {
	Value string
}

type RegExpr struct {
	Expr []Expression
}

func (rg RegExpr) toString() string {
	res := ""
	if len(rg.Expr) > 1 {
		res = "(" + rg.Expr[0].Value
		for i := 1; i < len(rg.Expr); i++ {
			res += "|" + rg.Expr[i].Value
		}
		res += ")"
	} else {
		if len(rg.Expr) > 0 {
			res = rg.Expr[0].Value
		}
	}
	return res
}

type EqutionArg struct {
	Regexp RegExpr
	ArgVar Var
}

type Eqution struct {
	EqutionVar  Var
	EqutionArgs []EqutionArg
}

func CheckUpperCase(s string) bool {
	for _, r := range s {
		if r < 'A' || r > 'Z' {
			return false
		}
	}

	return true
}

func CheckLowerCase(s string) bool {
	for _, r := range s {
		if r < 'a' || r > 'z' {
			return false
		}
	}

	return true
}

func tail(s string) string {
	if len(s) > 0 {
		return s[1:]
	}
	return s
}

func parseVar(s string) (string, Var, error) {
	if len(s) > 0 {
		v := s[:1]
		if !CheckUpperCase(v) {
			return s, Var{}, fmt.Errorf("wrong variable name: %s", v)
		}
		s = tail(s)
		return s, Var{v}, nil
	}

	return s, Var{}, nil
}

func parseExpression(s string) (string, Expression, error) {
	if len(s) > 0 {
		expr := ""
		for len(s) > 0 && CheckLowerCase(string(s[0])) {
			expr += string(s[0])
			s = tail(s)
		}
		if len(expr) == 0 {
			return s, Expression{}, fmt.Errorf("empty expression in: %s", s)
		}
		return s, Expression{expr}, nil
	}

	return s, Expression{}, nil
}

func parseRegExp(s string) (string, RegExpr, error) {
	if len(s) > 0 {
		res := RegExpr{}
		var err error
		expr := Expression{}
		if s[0] == '(' {
			s = tail(s)

			for len(s) > 0 && s[0] != ')' {
				s, expr, err = parseExpression(s)
				if err != nil {
					return s, res, fmt.Errorf("unable to parse regexpr: %w", err)
				}
				res.Expr = append(res.Expr, expr)
				if s[0] == '|' {
					s = tail(s)
				}
			}

			if len(s) == 0 || s[0] != ')' {
				return s, res, fmt.Errorf("no close bracket for regex in: %s", s)
			}
			s = tail(s)

			if len(res.Expr) == 0 {
				return s, res, fmt.Errorf("empty regex in: %s", s)
			}
		} else {
			s, expr, err = parseExpression(s)
			if err != nil {
				return s, res, fmt.Errorf("unable to parse last expr in regexpr: %w", err)
			}
			res.Expr = append(res.Expr, expr)
			if len(res.Expr) == 0 {
				return s, res, fmt.Errorf("empty regex in: %s", s)
			}
		}
		return s, res, nil
	}
	return s, RegExpr{}, nil
}

func parseEqution(s string) (string, Eqution, error) {
	if len(s) > 0 {
		s, eqVar, err := parseVar(s)
		if err != nil {
			return s, Eqution{}, fmt.Errorf("unable to parse var in equation: %w", err)
		}
		res := Eqution{
			EqutionVar: eqVar,
		}
		LeftEqutionVars[eqVar.Name] = struct{}{}

		if len(s) == 0 || s[0] != '=' {
			return s, Eqution{}, fmt.Errorf("invalid eqution: %s", s)
		}
		s = tail(s)

		regexp := RegExpr{}
		for len(s) > 0 {
			s, regexp, err = parseRegExp(s)
			if err != nil {
				return s, res, fmt.Errorf("unable to parse regex: %w", err)
			}

			argVar := Var{}
			if len(s) > 0 && s[0] != '+' {
				s, argVar, err = parseVar(s)
				if err != nil {
					return s, res, fmt.Errorf("unable to parse var after regex: %w", err)
				}
			}

			if len(s) != 0 && s[0] != '+' {
				return s, res, fmt.Errorf("invalid eqution: %s", s)
			}
			s = tail(s)
			res.EqutionArgs = append(res.EqutionArgs, EqutionArg{
				Regexp: regexp,
				ArgVar: argVar,
			})
		}

		if len(res.EqutionArgs) == 0 {
			return s, res, fmt.Errorf("empty eqution: %s", s)
		}

		if len(s) != 0 {
			return s, res, fmt.Errorf("invalid eqution: %s", s)
		}

		return s, res, nil
	}

	return s, Eqution{}, nil
}

type subs struct {
	Eqution
	Prefix string
}

func normalize(s subs) subs {
	res := subs{
		Eqution: Eqution{
			EqutionVar: s.EqutionVar,
		},
		Prefix: s.Prefix,
	}

	dict := make(map[string][]EqutionArg)

	for _, a := range s.EqutionArgs {
		dict[a.ArgVar.Name] = append(dict[a.ArgVar.Name], a)
	}

	for k, v := range dict {
		value := v[0].Regexp.toString()

		for i := 1; i < len(v); i++ {
			value += "+" + v[i].Regexp.toString()
		}

		res.EqutionArgs = append(res.EqutionArgs, EqutionArg{
			ArgVar: Var{
				Name: k,
			},
			Regexp: RegExpr{
				Expr: []Expression{
					{
						Value: s.Prefix + "(" + value + ")",
					},
				},
			},
		})
	}

	return res
}

func insert(eqs []Eqution, start int, sub subs) []Eqution {
	for i := start; i < len(eqs); i++ {
		eq := Eqution{
			EqutionVar: eqs[i].EqutionVar,
		}

		for _, arg := range eqs[i].EqutionArgs {
			if sub.EqutionVar.Name == arg.ArgVar.Name {
				for _, subArg := range sub.EqutionArgs {
					eq.EqutionArgs = append(eq.EqutionArgs, EqutionArg{
						ArgVar: subArg.ArgVar,
						Regexp: RegExpr{
							Expr: []Expression{
								{arg.Regexp.toString() + subArg.Regexp.toString()},
							},
						},
					})
				}
			} else {
				eq.EqutionArgs = append(eq.EqutionArgs, arg)
			}
		}

		eqs[i] = eq
	}

	return eqs
}

func Solve(eqs []Eqution) ([]Eqution, error) {

	eqsArgs := make([]subs, 0)

	for t := range eqs {
		prefix := ""
		sub := subs{
			Eqution: Eqution{
				EqutionVar: eqs[t].EqutionVar,
			},
		}
		for i := range eqs[t].EqutionArgs {
			if _, ok := LeftEqutionVars[eqs[t].EqutionArgs[i].ArgVar.Name]; eqs[t].EqutionArgs[i].ArgVar.Name != "" && !ok {
				return []Eqution{}, fmt.Errorf("no such var in eqution: %s", eqs[t].EqutionArgs[i].ArgVar.Name)
			}

			if eqs[t].EqutionArgs[i].ArgVar.Name == eqs[t].EqutionVar.Name {
				if prefix == "" {
					prefix = eqs[t].EqutionArgs[i].Regexp.toString()
				} else {
					prefix += "+" + eqs[t].EqutionArgs[i].Regexp.toString()
				}
			} else {
				sub.EqutionArgs = append(sub.EqutionArgs, eqs[t].EqutionArgs[i])
			}
		}

		if prefix != "" {
			if len(prefix) > 1 {
				sub.Prefix = "(" + prefix + ")*"
			} else {
				sub.Prefix = prefix + "*"
			}
		}
		eqsArgs = append(eqsArgs, normalize(sub))
		eqs = insert(eqs, t+1, eqsArgs[len(eqsArgs)-1])
	}

	resEqs := make([]Eqution, 0, len(eqsArgs))

	for _, e := range eqsArgs {
		resEqs = append(resEqs, e.Eqution)
	}

	res, err := resolve(resEqs)
	if err != nil {
		return []Eqution{}, fmt.Errorf("cannot resolve: %w", err)
	}

	return res, nil
}

func resolve(solvings []Eqution) ([]Eqution, error) {
	reverse(&solvings)
	res := make([]Eqution, len(solvings))
	for i := 0; i < len(solvings); i++ {
		prefix := ""
		eq := subs{
			Eqution: Eqution{
				EqutionVar: solvings[i].EqutionVar,
			},
		}
		for _, arg := range solvings[i].EqutionArgs {
			if arg.ArgVar.Name == solvings[i].EqutionVar.Name {
				if prefix == "" {
					prefix = arg.Regexp.toString()
				} else {
					prefix += "+" + arg.Regexp.toString()
				}
			} else if arg.ArgVar.Name != "" {
				return []Eqution{}, fmt.Errorf("smth went wrong, couldnt solve it for var %s due to unknown var %s",
					solvings[i].EqutionVar.Name, arg.ArgVar.Name)
			} else {
				eq.EqutionArgs = append(eq.EqutionArgs, arg)
			}
		}
		if prefix != "" {
			if len(prefix) > 1 {
				eq.Prefix = "(" + prefix + ")*"
			} else {
				eq.Prefix = prefix + "*"
			}
		}
		eq = normalize(eq)
		insert(solvings, i+1, eq)
		res[len(res)-i-1] = eq.Eqution
	}
	return res, nil
}

func reverse(a *[]Eqution) {
	if a == nil {
		return
	}
	for i := 0; i < len(*a)/2; i++ {
		(*a)[i], (*a)[len(*a)-i-1] = (*a)[len(*a)-i-1], (*a)[i]
	}
}

func PrintSolving(solvings []Eqution) []string {
	var res []string
	for _, solving := range solvings {
		tmp := fmt.Sprintf("%s = ", solving.EqutionVar.Name)
		if len(solving.EqutionArgs) > 0 {
			tmp += solving.EqutionArgs[0].Regexp.toString()
			for i := 1; i < len(solving.EqutionArgs); i++ {
				tmp += "+" + solving.EqutionArgs[i].Regexp.toString()
			}
		}
		res = append(res, tmp)
	}
	return res
}

func EqutionsFromStrings(lines []string) ([]Eqution, error) {
	eqs := make([]Eqution, 0)

	for _, l := range lines {
		_, eq, err := parseEqution(l)
		if err != nil {
			return []Eqution{}, err
		}

		eqs = append(eqs, eq)
	}

	return eqs, nil
}

func Run(pathToFile string) ([]string, error) {
	LeftEqutionVars = make(map[string]struct{})

	var lines []string

	file, err := os.Open(pathToFile)
	if err != nil {
		return []string{}, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, strings.Replace(scanner.Text(), " ", "", -1))
	}
	if err := scanner.Err(); err != nil {
		return []string{}, err
	}

	eqs, err := EqutionsFromStrings(lines)
	if err != nil {
		return []string{}, err
	}

	solving, err := Solve(eqs)
	if err != nil {
		return []string{}, err
	}

	return PrintSolving(solving), nil
}
