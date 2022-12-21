package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test(t *testing.T) {
	cases := []struct {
		name                 string
		first                string
		second               string
		match                int
		mismatch             int
		gap                  int
		expectedScore        int
		expectedFirstString  string
		expectedSecondString string
	}{
		{
			name:                 "1",
			first:                "ACGT",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        20,
			expectedFirstString:  "ACGT",
			expectedSecondString: "ACGT",
		},
		{
			name:                 "2",
			first:                "ACG",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        10,
			expectedFirstString:  "ACG-",
			expectedSecondString: "ACGT",
		},
		{
			name:                 "3",
			first:                "ACGT",
			second:               "ACG",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        10,
			expectedFirstString:  "ACGT",
			expectedSecondString: "ACG-",
		},
		{
			name:                 "4",
			first:                "ACAGT",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        15,
			expectedFirstString:  "ACAGT",
			expectedSecondString: "AC-GT",
		},
		{
			name:                 "5",
			first:                "ACGT",
			second:               "ACAGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        15,
			expectedFirstString:  "AC-GT",
			expectedSecondString: "ACAGT",
		},
		{
			name:                 "6",
			first:                "CAGT",
			second:               "ACAGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        15,
			expectedFirstString:  "-CAGT",
			expectedSecondString: "ACAGT",
		},
		{
			name:                 "7",
			first:                "ACAGT",
			second:               "CAGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        15,
			expectedFirstString:  "ACAGT",
			expectedSecondString: "-CAGT",
		},
		{
			name:                 "8",
			first:                "ACGT",
			second:               "A",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        -10,
			expectedFirstString:  "ACGT",
			expectedSecondString: "A---",
		},
		{
			name:                 "9",
			first:                "ACGT",
			second:               "",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        -20,
			expectedFirstString:  "ACGT",
			expectedSecondString: "----",
		},
		{
			name:                 "10",
			first:                "A",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        -10,
			expectedFirstString:  "A---",
			expectedSecondString: "ACGT",
		},
		{
			name:                 "11",
			first:                "",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        -20,
			expectedFirstString:  "----",
			expectedSecondString: "ACGT",
		},
		{
			name:                 "12",
			first:                "",
			second:               "",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        0,
			expectedFirstString:  "",
			expectedSecondString: "",
		},
		{
			name:                 "13",
			first:                "TACGT",
			second:               "ATGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        6,
			expectedFirstString:  "TACGT",
			expectedSecondString: "-ATGT",
		},
		{
			name:                 "14",
			first:                "TACGT",
			second:               "ACTGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        10,
			expectedFirstString:  "TAC-GT",
			expectedSecondString: "-ACTGT",
		},
		{
			name:                 "15",
			first:                "ACGT",
			second:               "TAGTA",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        0,
			expectedFirstString:  "-ACGT-",
			expectedSecondString: "TA-GTA",
		},
		{
			name:                 "16",
			first:                "TAGTA",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        0,
			expectedFirstString:  "TA-GTA",
			expectedSecondString: "-ACGT-",
		},
		{
			name:                 "17",
			first:                "ACGT",
			second:               "TAGT",
			match:                5,
			mismatch:             -4,
			gap:                  0,
			expectedScore:        15,
			expectedFirstString:  "-ACGT",
			expectedSecondString: "TA-GT",
		},
		{
			name:                 "18",
			first:                "TAGT",
			second:               "ACGT",
			match:                5,
			mismatch:             -4,
			gap:                  10,
			expectedScore:        80,
			expectedFirstString:  "TA----GT",
			expectedSecondString: "--ACGT--",
		},
		{
			name:                 "19",
			first:                "GGAGCCAAGGTGAAGTTGTAGCAGTGTGTCC",
			second:               "GACTTGTGGAACCTCTGTCCTCCGAGCTCTC",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        8,
			expectedFirstString:  "GGAGCCAAGGT-GAAG-T-TGTAG-CAGTG-TGTCC",
			expectedSecondString: "-GA-C--TTGTGGAACCTCTGTCCTCCGAGCTCTC-",
		},
		{
			name:                 "20",
			first:                "AAAAAAATTTTTTT",
			second:               "TTTTTTTAAAAAAA",
			match:                5,
			mismatch:             -4,
			gap:                  -5,
			expectedScore:        -35,
			expectedFirstString:  "AAAAAAATTTTTTT-------",
			expectedSecondString: "-------TTTTTTTAAAAAAA",
		},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%s_%s_%s", c.name, c.first, c.second), func(t *testing.T) {
			actualFirstString, actualSecondString, actualScore := hirschberg(c.first, c.second, c.match, c.mismatch, c.gap)
			assert.Equal(t, c.expectedScore, actualScore)
			assert.Equal(t, c.expectedFirstString, actualFirstString)
			assert.Equal(t, c.expectedSecondString, actualSecondString)
		})
	}
}
