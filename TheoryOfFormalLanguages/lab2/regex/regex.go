package regex

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"regexp"
	"time"
)

const (
	regexStr     = `^ac((ab)|(bc))*aaa(b|c|d)*$`
	nregexStr    = `^[^bcd][^abd](([^bcd][^acd])|([^acd][^abd]))*[^bcd][^bcd][^bcd]([^acd]|[^abd]|[^abc])*$`
	lazyRegexStr = `^ac(ab|bc)*?aaa(b|c|d)*?$`
)

var lengths []int = []int{100, 100, 200, 200, 1000, 1000, 10000, 10000, 100000, 100000}

func genTestFiles(pathToTests string) error {
	firstChoice := []string{"ab", "bc"}
	secondChoice := []string{"b", "c", "d"}

	for i, length := range lengths {
		size := rand.Intn(length - 6)
		line := "ac"
		for j := 0; j < size/2; j++ {
			line += firstChoice[rand.Intn(2)]
		}
		line += "aaa"
		for len(line) < length {
			line += secondChoice[rand.Intn(3)]
		}

		file, err := os.Create(pathToTests + fmt.Sprintf("test%d.txt", i))
		if err != nil {
			return fmt.Errorf("cannot generate tests files: %w", err)
		}
		defer file.Close()

		_, err = file.WriteString(line)
		if err != nil {
			return fmt.Errorf("cannot write str to test file: %w", err)
		}
	}

	return nil
}

func Run(pathToTests, output string) error {
	if pathToTests[len(pathToTests)-1] != '/' {
		pathToTests += "/"
	}

	err := genTestFiles(pathToTests)
	if err != nil {
		return err
	}

	regex := regexp.MustCompile(regexStr)
	nregex := regexp.MustCompile(nregexStr)
	lazyRegex := regexp.MustCompile(lazyRegexStr)

	out, err := os.Create(output)
	if err != nil {
		return err
	}
	defer out.Close()
	out.WriteString("Time in Nanoseconds\n")
	fmt.Println("Time in Nanoseconds")

	for i := 0; i < 10; i++ {
		file, err := os.Open(pathToTests + fmt.Sprintf("test%d.txt", i))
		if err != nil {
			return err
		}
		defer file.Close()

		line := ""
		scanner := bufio.NewScanner(file)
		buf := make([]byte, lengths[i]+1)
		scanner.Buffer(buf, lengths[len(lengths)-1]+1)

		for scanner.Scan() {
			line = scanner.Text()
		}
		if scanner.Err() != nil {
			return scanner.Err()
		}

		start := time.Now()
		m := regex.Match([]byte(line))
		end := time.Now()
		if !m {
			return fmt.Errorf("test%d.txt is not matched by regex", i)
		}
		regexDiff := end.Nanosecond() - start.Nanosecond()

		start = time.Now()
		m = nregex.Match([]byte(line))
		end = time.Now()
		if !m {
			return fmt.Errorf("test%d.txt is not matched by nregex", i)
		}
		nregexDiff := end.Nanosecond() - start.Nanosecond()

		start = time.Now()
		m = lazyRegex.Match([]byte(line))
		end = time.Now()
		if !m {
			return fmt.Errorf("test%d.txt is not matched by lazyRegex", i)
		}
		lazyRegexDiff := end.Nanosecond() - start.Nanosecond()

		res := fmt.Sprintf("\nTest %d:\nFirst regexp: %dns\nSecond regexp: %dns\nThird regexp: %dns\n",
			i,
			regexDiff,
			nregexDiff,
			lazyRegexDiff,
		)

		out.WriteString(res)
		fmt.Println(res)
	}

	return nil
}
