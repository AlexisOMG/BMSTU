package main

import (
	"flag"
	"fmt"
	"log"

	"github.com/AlexisOMG/tfl-lab2/regex"
	"github.com/AlexisOMG/tfl-lab2/rgconverter"
	"github.com/AlexisOMG/tfl-lab2/rgsystem"
)

func main() {
	mode := flag.String("mode", "", "rgsystem | rgconverter | regex")
	pathToTestFile := flag.String("test", "", "path to test file or path to dir for creating test files for mode=regex")
	pathToOutputFile := flag.String("output", ".", "path to output file for mode=regex")
	flag.Parse()

	if *mode == "" {
		log.Fatal("No mode")
	}

	if *pathToTestFile == "" {
		log.Fatal("No path to tests")
	}

	switch *mode {
	case "rgsystem":
		answ, err := rgsystem.Run(*pathToTestFile)
		if err != nil {
			log.Fatal(err.Error())
		}
		for _, a := range answ {
			fmt.Println(a)
		}
	case "rgconverter":
		s, err := rgconverter.Run(*pathToTestFile)
		if err != nil {
			log.Fatal(err.Error())
		}
		fmt.Println(s)
	case "regex":
		if *pathToOutputFile == "" {
			log.Fatal("No path to output file")
		}
		err := regex.Run(*pathToTestFile, *pathToOutputFile)
		if err != nil {
			log.Fatal(err)
		}
	}
}
