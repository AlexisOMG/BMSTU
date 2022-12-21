package main

import (
	"fmt"
	"math"
	"strings"
)

const (
	left = iota + 1
	up
	diag
)

func reverse[T any](ts []T) []T {
	res := make([]T, 0, len(ts))
	for i := len(ts) - 1; i >= 0; i-- {
		res = append(res, ts[i])
	}
	return res
}

func max(vs ...int) int {
	res := math.MinInt
	for _, v := range vs {
		if v > res {
			res = v
		}
	}
	return res
}

func maxInd(vs ...int) int {
	res := math.MinInt
	ind := -1
	for i, v := range vs {
		if v > res {
			res = v
			ind = i
		}
	}
	return ind
}

func repeat[T any](val T, cnt int) []T {
	res := make([]T, 0, cnt)
	for i := 0; i < cnt; i++ {
		res = append(res, val)
	}
	return res
}

func getScore(first, second string, match, mismatch, gap int) []int {
	n := len(first)
	m := len(second)

	tmp := make([]int, m+1)

	for i := 1; i < m+1; i++ {
		tmp[i] = gap * i
	}

	for i := 1; i < n+1; i++ {
		prev := i * gap
		newTmp := make([]int, m+1)
		tmp[0] = gap * (i - 1)
		newTmp[0] = gap * i

		for j := 1; j < m+1; j++ {

			cost := 0
			if first[i-1] == second[j-1] {
				cost = match
			} else {
				cost = mismatch
			}

			substitution := tmp[j-1] + cost
			deleting := tmp[j] + gap
			inserting := prev + gap

			maxCost := max(substitution, deleting, inserting)
			prev = maxCost
			newTmp[j] = maxCost
		}
		copy(tmp, newTmp)
	}

	return tmp
}

func solve(first, second string, match, mismatch, gap int) (string, string, int) {
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

	return resFirst, resSecond, dp[n][m]
}

func hirschberg(first, second string, match, mismatch, gap int) (string, string, int) {
	n := len(first)
	m := len(second)

	switch {
	case n == 0:
		return strings.Join(repeat("-", m), ""), second, gap * m
	case m == 0:
		return first, strings.Join(repeat("-", n), ""), gap * n
	case n == 1 || m == 1:
		return solve(first, second, match, mismatch, gap)
	}

	mid := n / 2

	scoreL := getScore(first[:mid], second, match, mismatch, gap)
	scoreR := reverse(getScore(
		strings.Join(reverse(strings.Split(first[mid:], "")), ""),
		strings.Join(reverse(strings.Split(second, "")), ""),
		match, mismatch, gap))

	score := make([]int, 0, len(scoreL))
	for i := 0; i < len(scoreL); i++ {
		score = append(score, scoreL[i]+scoreR[i])
	}

	ind := maxInd(score...)

	fL, sL, scL := hirschberg(first[:mid], second[:ind], match, mismatch, gap)
	fR, sR, scR := hirschberg(
		strings.Join(reverse(strings.Split(first[mid:], "")), ""),
		strings.Join(reverse(strings.Split(second[ind:], "")), ""),
		match, mismatch, gap)

	return fL + strings.Join(reverse(strings.Split(fR, "")), ""), sL + strings.Join(reverse(strings.Split(sR, "")), ""), scL + scR
}

func main() {
	var first, second string
	var match, mismatch, gap int

	fmt.Println("Enter first string:")
	fmt.Scanf("%s", &first)
	fmt.Println("Enter second string:")
	fmt.Scanf("%s", &second)
	fmt.Println("Enter match value:")
	fmt.Scanf("%d", &match)
	fmt.Println("Enter mismatch value:")
	fmt.Scanf("%d", &mismatch)
	fmt.Println("Enter gap value:")
	fmt.Scanf("%d", &gap)

	fmt.Println(hirschberg(first, second, match, mismatch, gap))
}
