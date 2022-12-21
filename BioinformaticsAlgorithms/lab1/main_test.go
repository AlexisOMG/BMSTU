package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test(t *testing.T) {
	cases := []struct {
		name                 string
		first                string
		second               string
		gap                  int
		expectedScore        int
		expectedFirstString  string
		expectedSecondString string
	}{
		{
			name:                 "lesson_example",
			first:                "AATCGA",
			second:               "AACGT",
			gap:                  -10,
			expectedScore:        6,
			expectedFirstString:  "AATCGA",
			expectedSecondString: "AA-CGT",
		},
		{
			name:                 "only_substitution",
			first:                "AATCGA",
			second:               "TTTTTT",
			gap:                  -10,
			expectedScore:        -15,
			expectedFirstString:  "AATCGA",
			expectedSecondString: "TTTTTT",
		},
		{
			name:                 "single_match_several_deleting",
			first:                "TTTTTT",
			second:               "T",
			gap:                  -10,
			expectedScore:        -45,
			expectedFirstString:  "TTTTTT",
			expectedSecondString: "-----T",
		},
		{
			name:                 "empty_second_string",
			first:                "TTTTTT",
			second:               "",
			gap:                  -10,
			expectedScore:        -60,
			expectedFirstString:  "TTTTTT",
			expectedSecondString: "------",
		},
		{
			name:                 "empty_first_string",
			first:                "",
			second:               "TTTTTT",
			gap:                  -10,
			expectedScore:        -60,
			expectedFirstString:  "------",
			expectedSecondString: "TTTTTT",
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			actualScore, actualFirstString, actualSecondString := solve(c.first, c.second, c.gap)
			assert.Equal(t, c.expectedScore, actualScore)
			assert.Equal(t, c.expectedFirstString, actualFirstString)
			assert.Equal(t, c.expectedSecondString, actualSecondString)
		})
	}
}
