package main

import (
	"fmt"
	"math"
)

type Point struct {
	x, y float64
}

var (
	intersec = Point{0, 0}
)

type function func(float64) float64

func gauss(matrix [][]float64, ds []float64, SIZE int) (xs []float64) {
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

var (
	f1 = func(p Point) float64 {
		return math.Cos(p.x+0.5) - p.y - 2
	}
	f2 = func(p Point) float64 {
		return math.Sin(p.y) - 2*p.x - 1
	}
	m2 = [][]function{
		{
			func(x float64) float64 { return -math.Sin(x + 0.5) },
			func(y float64) float64 { return -1 },
		},
		{
			func(x float64) float64 { return -2 },
			func(y float64) float64 { return math.Cos(y) },
		},
	}
)

func main() {
	n := 2

	realIntersec := Point{-0.9450111644002, -1.0973941404642}
	cnt := 0
	eps := math.Pow10(-2)

	for {
		cnt += 1
		matrix := [][]float64{
			{m2[0][0](intersec.x), m2[0][1](intersec.y)},
			{m2[1][0](intersec.x), m2[1][1](intersec.y)},
		}
		ds := []float64{-f1(intersec), -f2(intersec)}
		ys := gauss(matrix, ds, n)
		intersec.x += ys[0]
		intersec.y += ys[1]

		if math.Abs(ys[0]) < eps && math.Abs(ys[1]) < eps {
			fmt.Printf("STEP: %d, x=%.16f, y=%.16f, diff_x=%.16f, diff_y=%.16f\n",
				cnt,
				intersec.x,
				intersec.y,
				math.Abs(intersec.x-realIntersec.x),
				math.Abs(intersec.y-realIntersec.y),
			)
			break
		}

	}
}
