package main

import (
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"

	"github.com/AlexisOMG/ml-labs/lab1/network"
	"github.com/AlexisOMG/ml-labs/lab1/utils"
)

func main() {
	rand.Seed(time.Now().Unix())

	train := false
	generateTestDataset := false

	flag.BoolVar(&train, "t", false, "train network")
	flag.BoolVar(&generateTestDataset, "g", false, "generate test dataset")
	flag.Parse()

	if train {
		ntw := network.NewNetwork([]int{24, 10})

		samples, err := utils.GetSamples("train_dataset")
		if err != nil {
			log.Fatal(err.Error())
		}

		stats := ntw.Train(samples, 250)

		epochs, loss, accuracy := "[", "[", "["
		for _, st := range stats {
			epochs += strconv.Itoa(st.Epoch) + ","
			loss += fmt.Sprintf("%f,", st.Loss)
			accuracy += fmt.Sprintf("%f,", st.Accuracy)
		}

		epochs += "]\n"
		loss += "]\n"
		accuracy += "]\n"

		file, err := os.Create("func.txt")
		if err != nil {
			log.Fatal(err.Error())
		}
		defer file.Close()

		_, err = file.WriteString(epochs)
		if err != nil {
			log.Fatal(err.Error())
		}
		_, err = file.WriteString(loss)
		if err != nil {
			log.Fatal(err.Error())
		}
		_, err = file.WriteString(accuracy)
		if err != nil {
			log.Fatal(err.Error())
		}

		statistic := ntw.Statistic(samples)

		// for _, prediction := range statistic.Predictions {
		// 	fmt.Println(prediction.Prediction.Values, prediction.Sample.Expected.Values)
		// }

		fmt.Println("ACCURACY:", statistic.Accuracy, "LOSS:", statistic.Loss)

		err = ntw.Save("weights.json")
		if err != nil {
			log.Fatal(err.Error())
		}
	}

	if generateTestDataset {
		err := utils.CreatePictures("train_dataset", "ndigits")
		if err != nil {
			log.Fatal(err.Error())
		}
	}

	ntw, err := network.NewNetworkFromFile("weights.json")
	if err != nil {
		log.Fatal(err.Error())
	}

	samples, err := utils.GetSamples("ndigits")
	if err != nil {
		log.Fatal(err.Error())
	}

	statistic := ntw.Statistic(samples)

	for _, prediction := range statistic.Predictions {
		fmt.Printf("%s\n\toutput: %v\n\t expected: %v\n\t prediction: %d\n",
			prediction.Sample.Path,
			prediction.Prediction.Values,
			prediction.Sample.Expected.Values,
			ntw.Predict(prediction.Sample),
		)
	}

	fmt.Println(statistic.Accuracy, statistic.Loss)
}
