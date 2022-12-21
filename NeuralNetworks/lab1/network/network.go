package network

import (
	"encoding/json"
	"fmt"
	"math"
	"os"

	"github.com/AlexisOMG/ml-labs/lab1/utils"
)

type Network struct {
	LayerSizes []int          `json:"layerSizes"`
	Weights    []utils.Matrix `json:"weights"`
	Biases     []utils.Vector `json:"biases"`
}

const (
	STEP = 1
)

func NewNetwork(layerSizes []int) Network {
	ntw := Network{
		LayerSizes: layerSizes,
		Weights:    []utils.Matrix{utils.NewEmptyMatrix(0, 0)},
		Biases:     []utils.Vector{utils.NewEmptyVector(0)},
	}

	for i := 1; i < len(ntw.LayerSizes); i++ {
		ntw.Weights = append(ntw.Weights, utils.NewRandomMatrix(ntw.LayerSizes[i], ntw.LayerSizes[i-1], 0, 1))
		ntw.Biases = append(ntw.Biases, utils.NewRandomVector(ntw.LayerSizes[i], 0, 1))
	}

	return ntw
}

func NewNetworkFromFile(pathToFile string) (Network, error) {
	data, err := os.ReadFile(pathToFile)
	if err != nil {
		return Network{}, err
	}

	ntw := Network{}

	err = json.Unmarshal(data, &ntw)
	if err != nil {
		return Network{}, err
	}

	return ntw, nil
}

func (n *Network) Forward(sample utils.Sample) Result {
	activations := make([]utils.Vector, 0, len(n.LayerSizes))
	for _, layerSize := range n.LayerSizes {
		activations = append(activations, utils.NewEmptyVector(layerSize))
	}

	activations[0] = sample.Input

	weighetInputs := make([]utils.Vector, 0, len(n.LayerSizes))
	for _, layerSize := range n.LayerSizes {
		weighetInputs = append(weighetInputs, utils.NewEmptyVector(layerSize))
	}

	for i := 1; i < len(n.LayerSizes); i++ {
		weighetInputs[i] = n.Weights[i].MultiplyVector(activations[i-1]).AddVector(n.Biases[i])
		activations[i] = weighetInputs[i].Map(utils.Sigmoid)
	}

	return Result{
		Activations:    activations,
		WeightedInputs: weighetInputs,
	}
}

func (n *Network) Backpropagation(sample utils.Sample, result Result) []utils.Vector {
	errors := make([]utils.Vector, 0, len(n.LayerSizes))
	for _, layerSize := range n.LayerSizes {
		errors = append(errors, utils.NewEmptyVector(layerSize))
	}

	costFuncGradient := n.CalcCostFuncGradient(sample, result)

	errors[len(errors)-1] = costFuncGradient.HadamarProduct(
		result.WeightedInputs[len(result.WeightedInputs)-1].Map(utils.SigmoidDerivative),
	)

	for i := len(n.LayerSizes) - 2; i >= 1; i-- {
		errors[i] = n.Weights[i+1].Transpose().MultiplyVector(errors[i+1]).HadamarProduct(result.WeightedInputs[i].Map(utils.SigmoidDerivative))
	}

	return errors
}

func (n *Network) CalcCostFuncGradient(sample utils.Sample, result Result) utils.Vector {
	res := utils.NewEmptyVector(n.LayerSizes[len(n.LayerSizes)-1])

	for i := 0; i < n.LayerSizes[len(n.LayerSizes)-1]; i++ {
		res.Values[i] = result.Activations[len(result.Activations)-1].Values[i] - sample.Expected.Values[i]
	}

	return res
}

func (n *Network) RebalanceWeights(samples []utils.Sample, activations, errors [][]utils.Vector) {
	for i := 1; i < len(n.LayerSizes); i++ {
		deltaWeights := utils.NewEmptyMatrix(n.LayerSizes[i], n.LayerSizes[i-1])
		deltaBiases := utils.NewEmptyVector(n.LayerSizes[i])

		for j := 0; j < len(samples); j++ {
			deltaWeights = deltaWeights.AddMatrix(
				errors[j][i].ToMatrix().MultiplyMatrix(activations[j][i-1].ToMatrix().Transpose()),
			)
			deltaBiases = deltaBiases.AddVector(errors[j][i])
		}

		koeff := -float64(STEP) / float64(len(n.LayerSizes))

		n.Weights[i] = n.Weights[i].AddMatrix(deltaWeights.MultiplyScalar(koeff))
		n.Biases[i] = n.Biases[i].AddVector(deltaBiases.MultiplyScalar(koeff))
	}
}

func (n *Network) Statistic(samples []utils.Sample) Statistic {
	loss := 0.0
	accuracy := 0.0
	var predictions []Prediction

	for _, sample := range samples {
		res := n.Forward(sample)
		output := res.Activations[len(res.Activations)-1]

		prediction := Prediction{
			Sample:     sample,
			Prediction: output,
		}
		predictions = append(predictions, prediction)

		loss += math.Pow(sample.Expected.AddVector(output.MultiplyScalar(-1)).EuclideanLength(), 2)

		if output.GetMaxIndex() == sample.Expected.GetMaxIndex() {
			accuracy += 1
		}
	}

	return Statistic{
		Accuracy:    accuracy / float64(len(samples)),
		Loss:        loss / float64(2*len(samples)),
		Predictions: predictions,
	}
}

func (n *Network) Predict(sample utils.Sample) int {
	res := n.Forward(sample)
	return res.Activations[len(res.Activations)-1].GetMaxIndex()
}

func (n *Network) Train(samples []utils.Sample, epochs int) (stats []Statistic) {
	for i := 0; i < epochs; i++ {
		statistic := n.Statistic(samples)
		statistic.Epoch = i
		stats = append(stats, statistic)
		fmt.Println(statistic.Accuracy, statistic.Loss)

		var errors [][]utils.Vector
		var activations [][]utils.Vector

		for _, sample := range samples {
			res := n.Forward(sample)
			activations = append(activations, res.Activations)
			curErrors := n.Backpropagation(sample, res)
			errors = append(errors, curErrors)
		}

		n.RebalanceWeights(samples, activations, errors)
	}
	return
}

func (n *Network) Save(pathToFile string) error {
	data, err := json.Marshal(n)
	if err != nil {
		return err
	}

	return os.WriteFile(pathToFile, data, 0755)
}

type Statistic struct {
	Epoch       int
	Accuracy    float64
	Loss        float64
	Predictions []Prediction
}

type Prediction struct {
	Sample     utils.Sample
	Prediction utils.Vector
}

type Result struct {
	Activations    []utils.Vector
	WeightedInputs []utils.Vector
}
