package main

import (
	"fmt"
	"log"
	"os"

	"github.com/AlexisOMG/tfl-lab3/analyzer"
)

func main() {
	pathToFile := os.Args[1]

	res, err := analyzer.Analyze(pathToFile)
	if err != nil {
		log.Fatal(err.Error())
	}

	fmt.Println(res)
}
