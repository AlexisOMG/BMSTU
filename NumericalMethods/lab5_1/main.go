package main

import (
	"fmt"
	"math"
)

func f1(x, y, y1 float64) float64 {
	return math.Exp(x)*(x*x+x-3) + 2*y1 - 0*y
}

func f(x, y, y1 float64) []float64 {
	return []float64{y1, f1(x, y, y1)}
}

func analyticalSolution(x float64) float64 {
	return math.Exp(x) * (-math.Pow(x, 2) - x + math.Exp(x) + 1)
}

func analyticalSolutiont(x float64) float64 {
	return math.Exp(x) * (-x*(x+3) + 2*math.Exp(x))
}

func step(h, x, y, y1 float64) (float64, float64, float64) {
	k1 := f(x, y, y1)
	k2 := f(x+h/2, y+h*k1[0]/2, y1+h*k1[1]/2)
	k3 := f(x+h/2, y+h*k2[0]/2, y1+h*k2[1]/2)
	k4 := f(x+h/2, y+h*k3[0], y1+h*k3[1])

	x += h
	y = y + h/6*(k1[0]+2*(k2[0]+k3[0])+k4[0])
	y1 = y1 + h/6*(k1[1]+2*(k2[1]+k3[1])+k4[1])
	return x, y, y1
}

func main() {
	a := 0.0
	b := 1.0
	h := 0.01

	x := a
	y := 2.0
	y1 := 2.0

	for x < b {
		x, y, y1 = step(h, x, y, y1)
	}

	fmt.Printf("y*(1)=%.16f,y'*(1)=%.16f, diff_y=%.16f, diff_y'=%.16f\n",
		y,
		y1,
		math.Abs(y-analyticalSolution(x)),
		math.Abs(y1-analyticalSolutiont(x)),
	)
}
