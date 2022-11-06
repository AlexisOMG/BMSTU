package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

func avRect(a, b float64, n int, f func(float64) float64) float64 {
	h := (b - a) / float64(n)
	sum := 0.0

	for i := 1; i <= n; i++ {
		sum += f(a + float64(i)*h - h/2)
	}

	return h * sum
}

func trapezoid(a, b float64, n int, f func(float64) float64) float64 {
	h := (b - a) / float64(n)
	sum := (f(a) + f(b)) / 2

	for i := 1; i < n; i++ {
		sum += f(a + h*float64(i))
	}

	return h * sum
}

func simps(a, b float64, n int, f func(float64) float64) float64 {
	h := (b - a) / float64(n)
	sum := f(a) + f(b)

	for i := 1; i < n; i++ {
		if i%2 == 0 {
			sum += 2 * f(a+float64(i)*h)
		} else {
			sum += 4 * f(a+float64(i)*h)
		}
	}

	return h / 3 * sum
}

func montekarl(n int, a, b, maxX float64, f func(float64) float64) float64 {
	type point struct {
		x, y float64
	}
	points := make([]point, 0, n)
	for i := 0; i < n; i++ {
		points = append(points, point{a + rand.Float64()*(b-a), rand.Float64() * f(maxX)})
	}
	// fmt.Println(points)
	cnt := 0
	for _, p := range points {
		if p.y <= f(p.x) {
			cnt += 1
		}
	}
	return ((b - a) * f(maxX)) * (float64(cnt) / float64(n))
}

func main() {
	rand.Seed(time.Now().UnixNano())
	rectN := 16
	trapN := 20
	simpN := 14
	e := 4.0
	tf := func(x float64) float64 {
		return 2 * math.Sin(math.Sqrt(x))
	}
	a, b := 0.0, math.Pi*math.Pi/4
	fmt.Printf("n: %d Rect: %.16f diff=%.16f correction=%.16f corrected=%.16f cor_dif=%.16f\nn: %d Trap: %.16f diff=%.16f correction=%.16f corrected=%.16f cor_dif=%.16f\nn: %d Simp: %.16f diff=%.16f correction=%.16f corrected=%.16f cor_dif=%.16f\n",
		rectN,
		avRect(a, b, rectN, tf),
		math.Abs(avRect(a, b, rectN, tf)-e),
		math.Abs(avRect(a, b, rectN, tf)-avRect(a, b, 2*rectN, tf))/(math.Exp2(2)-1),
		avRect(a, b, 2*rectN, tf)+(avRect(a, b, rectN, tf)-avRect(a, b, 2*rectN, tf))/(math.Exp2(2)-1),
		math.Abs(avRect(a, b, 2*rectN, tf)+(avRect(a, b, rectN, tf)-avRect(a, b, 2*rectN, tf))/(math.Exp2(2)-1)-e),
		trapN,
		trapezoid(a, b, trapN, tf),
		math.Abs(trapezoid(a, b, trapN, tf)-e),
		math.Abs(trapezoid(a, b, trapN, tf)-trapezoid(a, b, 2*trapN, tf))/(math.Exp2(2)-1),
		trapezoid(a, b, 2*trapN, tf)+(trapezoid(a, b, trapN, tf)-trapezoid(a, b, 2*trapN, tf))/(math.Exp2(2)-1),
		math.Abs(trapezoid(a, b, 2*trapN, tf)+(trapezoid(a, b, trapN, tf)-trapezoid(a, b, 2*trapN, tf))/(math.Exp2(2)-1)-e),
		simpN,
		simps(a, b, simpN, tf),
		math.Abs(simps(a, b, simpN, tf)-e),
		math.Abs(simps(a, b, simpN, tf)-simps(a, b, 2*simpN, tf))/(math.Exp2(4)-1),
		simps(a, b, 2*simpN, tf)+(simps(a, b, simpN, tf)-simps(a, b, 2*simpN, tf))/(math.Exp2(4)-1),
		math.Abs(simps(a, b, 2*simpN, tf)+(simps(a, b, simpN, tf)-simps(a, b, 2*simpN, tf))/(math.Exp2(4)-1)-e),
	)

	eps := 0.01
	cntP := 2
	m := montekarl(cntP, a, b, b, tf)
	for math.Abs(m-e) >= eps {
		cntP *= 2
		m = montekarl(cntP, a, b, b, tf)
	}
	fmt.Printf("n=%d Monte: %.16F diff=%.16f\n", cntP, m, math.Abs(m-e))
}
