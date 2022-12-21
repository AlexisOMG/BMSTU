package utils

import (
	"math"
	"math/rand"
)

type Vector struct {
	Values []float64 `json:"values"`
}

func (v *Vector) Elements() int {
	return len(v.Values)
}

func (v Vector) EuclideanLength() float64 {
	res := 0.0
	for _, v := range v.Values {
		res += v * v
	}
	return math.Sqrt(res)
}

func (v Vector) AddVector(another Vector) Vector {
	res := NewEmptyVector(v.Elements())
	for i := range v.Values {
		res.Values[i] = v.Values[i] + another.Values[i]
	}
	return res
}

func (v Vector) HadamarProduct(another Vector) Vector {
	res := NewEmptyVector(v.Elements())
	for i := range v.Values {
		res.Values[i] = v.Values[i] * another.Values[i]
	}
	return res
}

func (v Vector) GetMaxIndex() int {
	res := 0
	for i := range v.Values {
		if v.Values[i] > v.Values[res] {
			res = i
		}
	}
	return res
}

func (v Vector) ToMatrix() Matrix {
	values := make([][]float64, v.Elements())
	for i, v := range v.Values {
		values[i] = []float64{v}
	}
	return NewMatrix(values)
}

func (v Vector) MultiplyScalar(a float64) Vector {
	res := NewEmptyVector(v.Elements())
	for i := range v.Values {
		res.Values[i] = v.Values[i] * a
	}
	return res
}

func (v Vector) Map(f func(float64) float64) Vector {
	res := make([]float64, v.Elements())
	for i := range v.Values {
		res[i] = f(v.Values[i])
	}
	return NewVector(res)
}

func NewVector(vals []float64) Vector {
	values := make([]float64, len(vals))
	copy(values, vals)

	return Vector{
		Values: values,
	}
}

func NewEmptyVector(n int) Vector {
	return Vector{
		Values: make([]float64, n),
	}
}

func NewRandomVector(length int, min, max float64) Vector {
	res := NewEmptyVector(length)
	for i := 0; i < length; i++ {
		res.Values[i] = min + rand.Float64()*(max-min)
	}
	return res
}
