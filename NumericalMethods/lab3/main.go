package main

import (
	"fmt"
	"math"
)

const a, b float64 = 0.22352220596602404, 0.08902416312224844
const SIZE = 9

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
	return 1 / (a*math.Log(x) + b)
}

func main() {
	l, r := 1.0, 5.0
	h := (r - l) / float64(SIZE-1)
	xs := []float64{1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0}
	ys := []float64{9.44, 5.16, 4.43, 3.31, 3.48, 3.2, 2.34, 2.13, 2.18}
	// ys := []float64{}
	// for i := 0; i < SIZE; i++ {
	// 	ys = append(ys, f(xs[i]))
	// }

	ds := []float64{}
	for i := 1; i < SIZE-1; i++ {
		ds = append(ds, 3*(ys[i+1]-2*ys[i]+ys[i-1])/(h*h))
	}

	bs := []float64{}
	for i := 1; i < SIZE-1; i++ {
		bs = append(bs, 4)
	}

	as := []float64{}
	for i := 1; i < SIZE-2; i++ {
		as = append(as, 1)
	}

	cs := []float64{}
	for i := 1; i < SIZE-2; i++ {
		cs = append(cs, 1)
	}

	alpha, beta := direct(bs, as, cs, ds, SIZE-2)
	res := reverse(alpha, beta, SIZE-2)
	res = append([]float64{0}, res...)

	coefA := ys

	coefB := make([]float64, 0, SIZE-1)
	for i := 0; i < SIZE-2; i++ {
		coefB = append(coefB, (ys[i+1]-ys[i])/h-h/3*(res[i+1]+2*res[i]))
	}
	coefB = append(coefB, (ys[SIZE-1]-ys[SIZE-2])/h-2.0/3*h*res[SIZE-2])

	coefD := make([]float64, 0, SIZE-1)
	for i := 0; i < SIZE-2; i++ {
		coefD = append(coefD, (res[i+1]-res[i])/(3*h))
	}
	coefD = append(coefD, -res[len(res)-1]/(3*h))

	// geo := "z(x) = If("
	for i := 0; i < SIZE-1; i++ {
		newX := l + (float64(i+1)-0.5)*h
		newY := f(newX)
		val := coefA[i] + coefB[i]*(newX-xs[i]) + res[i]*math.Pow(newX-xs[i], 2) + coefD[i]*math.Pow(newX-xs[i], 3)
		// geo += fmt.Sprintf(
		// 	" %.16f <= x <=  %.16f, %.16f + %.16f*(x-%.16f)+%.16f*(x-%.16f)^2+%.16f*(x-%.16f)^3, ",
		// 	xs[i],
		// 	xs[i+1],
		// 	coefA[i],
		// 	coefB[i],
		// 	xs[i],
		// 	res[i],
		// 	xs[i],
		// 	coefD[i],
		// 	xs[i],
		// )
		fmt.Printf(
			"xi: %f, yi: %f, yi*: %f, |yi-yi*|: %f\n",
			newX,
			newY,
			val,
			math.Abs(newY-val),
		)
	}
	// fmt.Println(geo[:len(geo)-2] + ")")
}
