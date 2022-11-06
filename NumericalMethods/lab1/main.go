package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

const SIZE = 4

func readCoeff(line string, name string, size int) ([]float64, error) {
	res := make([]float64, 0, size)

	strNums := strings.Split(line, " ")
	if size != len(strNums) {
		return nil, errors.New("wrong size of " + name)
	}

	for _, sNum := range strNums {
		num, err := strconv.ParseFloat(sNum, 64)
		if err != nil {
			return nil, err
		}
		res = append(res, num)
	}

	return res, nil
}

func direct(bs, as, cs, ds []float64) (alpha, beta []float64) {
	alpha = append(alpha, -cs[0]/bs[0])
	beta = append(beta, ds[0]/bs[0])
	var div float64
	for i := 1; i < SIZE-1; i++ {
		div = as[i-1]*alpha[i-1] + bs[i]
		alpha = append(alpha, -cs[i]/div)
		beta = append(beta, (ds[i]-as[i-1]*beta[i-1])/div)
	}
	div = as[SIZE-2]*alpha[SIZE-2] + bs[SIZE-1]
	beta = append(beta, (ds[SIZE-1]-as[SIZE-2]*beta[SIZE-2])/div)
	return alpha, beta
}

func reverse(alpha, beta []float64) (xs []float64) {
	xs = make([]float64, SIZE)
	xs[SIZE-1] = beta[SIZE-1]
	for i := SIZE - 2; i >= 0; i-- {
		xs[i] = alpha[i]*xs[i+1] + beta[i]
	}
	return xs
}

func gauss(matrix [][]float64, ds []float64) (xs []float64) {
	xs = make([]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		for j := i + 1; j < SIZE; j++ {
			var k float64 = matrix[j][i] / matrix[i][i]
			for t := i; t < SIZE; t++ {
				matrix[j][t] -= k * matrix[i][t]
			}
			ds[j] -= k * ds[i]
		}
	}
	for i := SIZE - 1; i >= 0; i-- {
		var k float64 = 0
		for j := i + 1; j < SIZE; j++ {
			k += matrix[i][j] * xs[j]
		}
		xs[i] = (ds[i] - k) / matrix[i][i]
	}
	return xs
}

func multiply(matrix [][]float64, xs []float64) (ds []float64) {
	ds = make([]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		var sum float64 = 0
		for j := 0; j < SIZE; j++ {
			sum += matrix[i][j] * xs[j]
		}
		ds[i] = sum
	}
	return ds
}

func buildMatrix(bs, as, cs []float64) [][]float64 {
	matrix := make([][]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		matrix[i] = make([]float64, SIZE)
	}
	for i := 0; i < SIZE; i++ {
		for j := 0; j < SIZE; j++ {
			if i == j {
				matrix[i][j] = bs[i]
				if i != SIZE-1 {
					matrix[i][i+1] = as[i]
					matrix[i+1][i] = cs[i]
				}
			}
		}
	}
	return matrix
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if scanner.Err() != nil {
		log.Fatal(scanner.Err().Error())
	}

	bs, err := readCoeff(lines[0], "bs", SIZE)
	if err != nil {
		log.Fatal(err.Error())
	}
	as, err := readCoeff(lines[1], "as", SIZE-1)
	if err != nil {
		log.Fatal(err.Error())
	}
	cs, err := readCoeff(lines[2], "cs", SIZE-1)
	if err != nil {
		log.Fatal(err.Error())
	}
	ds, err := readCoeff(lines[3], "ds", SIZE)
	if err != nil {
		log.Fatal(err.Error())
	}

	firstRes := reverse(direct(bs, as, cs, ds))
	fmt.Print("First: ")
	for _, res := range firstRes {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}

	matrix := buildMatrix(bs, as, cs)
	secondRes := gauss(matrix, ds)
	fmt.Print("\nSecond: ")
	for _, res := range secondRes {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}
	fmt.Print("\nDiff: ")
	for i := 0; i < SIZE; i++ {
		fmt.Print(fmt.Sprintf("%.16f", math.Abs(firstRes[i]-secondRes[i])), " ")
	}
	fmt.Println()
	matrix = buildMatrix(bs, as, cs)
	newDs := multiply(matrix, firstRes)
	fmt.Print("NewDs: ")
	for _, res := range newDs {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}
	fmt.Println()
}
