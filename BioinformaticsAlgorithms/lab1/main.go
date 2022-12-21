package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

const (
	match    = 5
	mismatch = -4
)

const (
	left = iota + 1
	up
	diag
)

func max(vs ...int) int {
	res := math.MinInt
	for _, v := range vs {
		if v > res {
			res = v
		}
	}
	return res
}

func parseFasta(pathToFile string) ([]string, error) {
	dataRow, err := os.ReadFile(pathToFile)
	if err != nil {
		return nil, err
	}

	data := strings.Split(string(dataRow), "\n")
	var res []string
	curStr := ""
	for _, d := range data {
		if d != "" && d[0] == '>' {
			res = append(res, curStr)
			curStr = ""
		} else if d != "" {
			curStr += d
		}
	}

	if curStr != "" {
		res = append(res, curStr)
	}

	return res, nil

}

// сделано на основе https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_full_matrix
func solve(first, second string, gap int) (int, string, string) {
	n := len(first)
	m := len(second)

	dp := make([][]int, n+1)
	for i := 0; i < n+1; i++ {
		dp[i] = make([]int, m+1)
	}

	path := make([][]int, n+1)
	for i := 0; i < n+1; i++ {
		path[i] = make([]int, m+1)
	}

	for i := 1; i < n+1; i++ {
		dp[i][0] = gap * i
		path[i][0] = up
	}

	for i := 1; i < m+1; i++ {
		dp[0][i] = gap * i
		path[0][i] = left
	}

	for j := 1; j < m+1; j++ {
		for i := 1; i < n+1; i++ {
			cost := 0
			if first[i-1] == second[j-1] {
				cost = match
			} else {
				cost = mismatch
			}

			substitution := dp[i-1][j-1] + cost
			deleting := dp[i-1][j] + gap
			inserting := dp[i][j-1] + gap

			maxCost := max(substitution, deleting, inserting)
			dp[i][j] = maxCost
			switch maxCost {
			case substitution:
				path[i][j] = diag
			case deleting:
				path[i][j] = up
			case inserting:
				path[i][j] = left
			}
		}
	}

	resFirst := ""
	resSecond := ""

	i, j := n, m

	for !(i == 0 && j == 0) {
		switch path[i][j] {
		case diag:
			resFirst = string(first[i-1]) + resFirst
			resSecond = string(second[j-1]) + resSecond
			i -= 1
			j -= 1
		case up:
			resFirst = string(first[i-1]) + resFirst
			resSecond = "-" + resSecond
			i -= 1
		case left:
			resFirst = "-" + resFirst
			resSecond = string(second[j-1]) + resSecond
			j -= 1
		}
	}

	return dp[n][m], resFirst, resSecond
}

func main() {
	args := os.Args[1:]
	if len(args) == 0 {
		log.Fatal("неверный формат ввода")
	}

	pathsToFiles := []string{}
	gap := -10

	for i, arg := range args {
		if arg == "-i" {
			for j := i + 1; j < len(args) && args[j] != "-g"; j++ {
				pathsToFiles = append(pathsToFiles, args[j])
			}
		} else if arg == "-g" || arg == "--gap" {
			if i == len(args)-1 {
				log.Fatal("неверный формат ввода")
			}
			gap1, err := strconv.Atoi(args[i+1])
			if err != nil {
				log.Fatal(fmt.Errorf("не парсится gap: %w", err).Error())
			}
			gap = gap1
		}
	}

	if len(pathsToFiles) > 2 {
		log.Fatal("необходимо передать не более 2х файлов")
	}

	inputs := []string{}

	for _, pathToFile := range pathsToFiles {
		strs, err := parseFasta(pathToFile)
		if err != nil {
			log.Fatal(err.Error())
		}
		if len(strs) == 1 {
			log.Fatal("введена пустая строка")
		}
		inputs = append(inputs, strs[1:]...)
	}

	if len(inputs) != 2 {
		log.Fatal("передано не 2 строки")
	}

	fmt.Println(solve(inputs[0], inputs[1], gap))
}
