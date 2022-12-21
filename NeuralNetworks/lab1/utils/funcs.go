package utils

import "math"

func Sigmoid(x float64) float64 {
	return 1.0 / (1 + math.Exp(-x))
}

func SigmoidDerivative(x float64) float64 {
	return Sigmoid(x) * (1 - Sigmoid(x))
}
