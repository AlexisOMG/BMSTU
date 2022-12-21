package utils

import "math/rand"

type Matrix struct {
	Values [][]float64 `json:"values"`
}

func (m *Matrix) Rows() int {
	return len(m.Values)
}

func (m *Matrix) Columns() int {
	if m.Rows() == 0 {
		return 0
	}
	return len(m.Values[0])
}

func (m Matrix) AddMatrix(another Matrix) Matrix {
	res := NewEmptyMatrix(m.Rows(), m.Columns())

	for i := 0; i < m.Rows(); i++ {
		for j := 0; j < m.Columns(); j++ {
			res.Values[i][j] = m.Values[i][j] + another.Values[i][j]
		}
	}

	return res
}

func (m Matrix) MultiplyMatrix(another Matrix) Matrix {
	res := NewEmptyMatrix(m.Rows(), another.Columns())

	for i := 0; i < m.Rows(); i++ {
		for j := 0; j < another.Columns(); j++ {
			for k := 0; k < m.Columns(); k++ {
				res.Values[i][j] += m.Values[i][k] * another.Values[k][j]
			}
		}
	}

	return res
}

func (m Matrix) MultiplyVector(another Vector) Vector {
	res := NewEmptyVector(m.Rows())

	for i := 0; i < m.Rows(); i++ {
		for j := 0; j < m.Columns(); j++ {
			res.Values[i] += m.Values[i][j] * another.Values[j]
		}
	}

	return res
}

func (m Matrix) MultiplyScalar(val float64) Matrix {
	res := NewEmptyMatrix(m.Rows(), m.Columns())

	for i := 0; i < m.Rows(); i++ {
		for j := 0; j < m.Columns(); j++ {
			res.Values[i][j] = m.Values[i][j] * val
		}
	}

	return res
}

func (m Matrix) Transpose() Matrix {
	res := NewEmptyMatrix(m.Columns(), m.Rows())

	for i := 0; i < m.Rows(); i++ {
		for j := 0; j < m.Columns(); j++ {
			res.Values[j][i] = m.Values[i][j]
		}
	}

	return res
}

func NewMatrix(values [][]float64) Matrix {
	vs := make([][]float64, len(values))
	for i := range values {
		vs[i] = make([]float64, len(values[i]))
		copy(vs[i], values[i])
	}
	return Matrix{
		Values: vs,
	}
}

func NewEmptyMatrix(n, m int) Matrix {
	values := make([][]float64, 0, n)
	for i := 0; i < n; i++ {
		values = append(values, make([]float64, m))
	}
	return Matrix{
		Values: values,
	}
}

func NewRandomMatrix(n, m int, min, max float64) Matrix {
	res := NewEmptyMatrix(n, m)
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			res.Values[i][j] = min + rand.Float64()*(max-min)
		}
	}
	return res
}
