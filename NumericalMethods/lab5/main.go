package main

import (
	"fmt"
	"math"
)

func direct(bs, as, cs, ds []float64, size int) (alpha, beta []float64) {
	alpha = append(alpha, -cs[0]/bs[0])
	beta = append(beta, ds[0]/bs[0])
	var div float64
	for i := 1; i < size-1; i++ {
		div = as[i-1]*alpha[i-1] + bs[i]
		alpha = append(alpha, -cs[i]/div)
		beta = append(beta, (ds[i]-as[i-1]*beta[i-1])/div)
	}
	div = as[size-2]*alpha[size-2] + bs[size-1]
	beta = append(beta, (ds[size-1]-as[size-2]*beta[size-2])/div)
	return alpha, beta
}

func reverse(alpha, beta []float64, size int) (xs []float64) {
	xs = make([]float64, size)
	xs[size-1] = beta[size-1]
	for i := size - 2; i >= 0; i-- {
		xs[i] = alpha[i]*xs[i+1] + beta[i]
	}
	return xs
}

func f(x float64) float64 {
	return math.Exp(x)
}

// func f(x float64) float64 {
// 	return math.Exp(x) * (math.Pow(x, 2) + x - 3)
// }

func analyticalSolution(x float64) float64 {
	return math.Exp(x)
}

// func analyticalSolution(x float64) float64 {
// 	return math.Exp(x) * (-math.Pow(x, 2) - x + math.Exp(x) + 1)
// }

var (
	p = 1.0
	q = -1.0
	a = 1.0
	b = math.E
	// p = -2.0
	// q = 0.0
	// a = analyticalSolution(0)
	// b = analyticalSolution(1)
)

func main() {
	n := 9
	h := 1.0 / float64(n+1)
	xs := make([]float64, 0, n+1)
	for i := 0; i < n+1; i++ {
		xs = append(xs, float64(i)*h)
	}
	as := make([]float64, 0, n-1)
	bs := make([]float64, 0, n)
	cs := make([]float64, 0, n-1)
	ds := make([]float64, 0, n)

	for i := 0; i < n+1; i++ {
		bs = append(bs, h*h*q-2)
		ds = append(ds, h*h*f(float64(i)*h))
	}
	ds[0] = h*h*f(0) - a*(1-h/2*p)
	ds[len(ds)-1] = h*h*f(float64(len(ds)-1)*h) - b*(1+h/2*p)
	for i := 0; i < n; i++ {
		as = append(as, 1-h/2*p)
		cs = append(cs, 1+h/2*p)
	}

	fmt.Println("LEN: ", len(ds))

	alpha, beta := direct(bs, as, cs, ds, n+1)
	ys := reverse(alpha, beta, n+1)

	maxDiff := 0.0

	for i, y := range ys {
		fmt.Printf("%d: x=%.16f, y=%.16f, y*=%.16f  diff=%.16f\n",
			i,
			xs[i],
			analyticalSolution(xs[i]),
			y,
			math.Abs(y-analyticalSolution(xs[i])),
		)
		if math.Abs(y-analyticalSolution(xs[i])) > maxDiff {
			maxDiff = math.Abs(y - analyticalSolution(xs[i]))
		}
	}

	fmt.Printf("max_diff=%.16f\n", maxDiff)
}
