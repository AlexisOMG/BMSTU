package analyzer

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/AlexisOMG/tfl-lab3/analyzer/rule_types"
)

func TestCheckBelonging(t *testing.T) {
	testCases := []struct {
		word    string
		rules   rule_types.Rule
		initial []rule_types.Term
		result  bool
	}{
		{
			word:    "a",
			rules:   make(rule_types.Rule),
			initial: []rule_types.Term{{"a"}},
			result:  true,
		},
		{
			word:    "a",
			rules:   make(rule_types.Rule),
			initial: []rule_types.Term{{"b"}},
			result:  false,
		},
		{
			word:    "aa",
			rules:   make(rule_types.Rule),
			initial: []rule_types.Term{{"a"}},
			result:  true,
		},
		{
			word:    "aaa",
			rules:   make(rule_types.Rule),
			initial: []rule_types.Term{{"a"}, {"a"}},
			result:  false,
		},
		{
			word: "a",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}}},
			},
			initial: []rule_types.Term{{"A1"}},
			result:  true,
		},
		{
			word: "a",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"b"}}},
			},
			initial: []rule_types.Term{{"A1"}},
			result:  false,
		},
		{
			word: "abc",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}, {"A2"}}},
				"A2": {{{"b"}, {"A3"}}},
				"A3": {{{"c"}}},
			},
			initial: []rule_types.Term{{"A1"}},
			result:  true,
		},
		{
			word: "abc",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}, {"A2"}}},
				"A2": {{{"b"}, {"A3"}}},
				"A3": {{{"d"}}},
			},
			initial: []rule_types.Term{{"A1"}},
			result:  false,
		},
		{
			word: "abab",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}, {"A2"}}},
				"A2": {{{"b"}}},
			},
			initial: []rule_types.Term{{"A1"}},
			result:  true,
		},
		{
			word: "abcd",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}, {"A3"}}},
				"A2": {{{"c"}, {"A4"}}},
				"A3": {{{"b"}}},
				"A4": {{{"d"}}},
			},
			initial: []rule_types.Term{{"A1"}, {"A2"}},
			result:  true,
		},
		{
			word: "abc",
			rules: map[string][][]rule_types.Term{
				"A1": {{{"a"}, {"A2"}}, {{"a"}, {"b"}}},
				"A2": {{{"c"}}},
			},
			initial: []rule_types.Term{{"A1"}, {"A2"}},
			result:  true,
		},
	}

	for i, test := range testCases {
		t.Run(fmt.Sprintf("TestCheckBelonging_%d", i), func(t *testing.T) {
			assert.Equal(t, test.result, checkBelonging(test.word, test.rules, test.initial, test.initial))
		})
	}
}

func TestCheckAnotherNTerm(t *testing.T) {
	testCases := []struct {
		rules      rule_types.Rule
		regular    map[string]struct{}
		mbregular  map[string]struct{}
		outputrg   map[string]struct{}
		outputmbrg map[string]struct{}
	}{
		{
			rules:      make(rule_types.Rule),
			regular:    make(map[string]struct{}),
			mbregular:  make(map[string]struct{}),
			outputrg:   make(map[string]struct{}),
			outputmbrg: make(map[string]struct{}),
		},
		{
			rules: make(rule_types.Rule),
			regular: map[string]struct{}{
				"A": {},
			},
			mbregular: map[string]struct{}{
				"B": {},
			},
			outputrg: map[string]struct{}{
				"A": {},
			},
			outputmbrg: map[string]struct{}{
				"B": {},
			},
		},
		{
			rules: map[string][][]rule_types.Term{
				"C": {{{"a"}, {"A"}}},
				"D": {{{"a"}, {"A"}}, {{"b"}, {"B"}}},
				"E": {{{"a"}, {"A"}, {"E"}}},
			},
			regular: map[string]struct{}{
				"A": {},
			},
			mbregular: map[string]struct{}{
				"B": {},
			},
			outputrg: map[string]struct{}{
				"A": {},
				"C": {},
			},
			outputmbrg: map[string]struct{}{
				"B": {},
				"D": {},
			},
		},
		{
			rules: map[string][][]rule_types.Term{
				"B": {{{"C"}, {"A"}}},
				"C": {{{"A"}}},
				"F": {{{"D"}, {"A"}}},
				"E": {{{"D"}, {"F"}}},
				"G": {{{"D"}, {"G"}}},
			},
			regular: map[string]struct{}{
				"A": {},
			},
			mbregular: map[string]struct{}{
				"D": {},
			},
			outputrg: map[string]struct{}{
				"A": {},
				"B": {},
				"C": {},
			},
			outputmbrg: map[string]struct{}{
				"D": {},
				"E": {},
				"F": {},
			},
		},
	}

	for i, test := range testCases {
		t.Run(fmt.Sprintf("TestCheckAnotherNTerm_%d", i), func(t *testing.T) {
			org, ombrg := checkAnotherNTerms(test.rules, test.regular, test.mbregular)
			assert.Equal(t, test.outputrg, org)
			assert.Equal(t, test.outputmbrg, ombrg)
		})
	}
}
