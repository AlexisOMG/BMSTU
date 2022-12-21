package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetScore(t *testing.T) {
	cases := []struct {
		name          string
		first         string
		second        string
		gap           int
		expectedScore int
	}{
		{
			name:          "lesson_example",
			first:         "AATCGA",
			second:        "AACGT",
			gap:           -5,
			expectedScore: 11,
		},
		{
			name:          "only_substitution",
			first:         "AATCGA",
			second:        "TTTTTT",
			gap:           -5,
			expectedScore: -15,
		},
		{
			name:          "single_match_several_deleting",
			first:         "TTTTTT",
			second:        "T",
			gap:           -5,
			expectedScore: -20,
		},
		{
			name:          "empty_second_string",
			first:         "TTTTTT",
			second:        "",
			gap:           -5,
			expectedScore: -30,
		},
		{
			name:          "empty_first_string",
			first:         "",
			second:        "TTTTTT",
			gap:           -5,
			expectedScore: -30,
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			actualScore := getScore(c.first, c.second, 5, -4, c.gap)
			assert.Equal(t, c.expectedScore, actualScore)
		})
	}
}

func TestOptimizeSolve(t *testing.T) {
	cases := []struct {
		name     string
		first    string
		second   string
		match    int
		mismatch int
		gap      int
		k        int

		expectedScore int
		expectedError error
	}{
		{
			name:          "bad_input",
			first:         "AA",
			second:        "BBBBBAA",
			match:         5,
			mismatch:      -100,
			gap:           -10,
			k:             0,
			expectedScore: -1,
			expectedError: fmt.Errorf("bad input"),
		},
		{
			name:          "good_input",
			first:         "AA",
			second:        "BBBBBAA",
			match:         5,
			mismatch:      -100,
			gap:           -10,
			k:             10,
			expectedScore: -40,
			expectedError: nil,
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			actualScore, actualErr := optimizeSolve(c.first, c.second, c.match, c.mismatch, c.gap, c.k)
			assert.Equal(t, c.expectedError, actualErr)
			assert.Equal(t, c.expectedScore, actualScore)
		})
	}
}
