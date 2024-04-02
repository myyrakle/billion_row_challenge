package main

import (
	"brc/pkg"
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func solution(inputPath string) string {
	buffer := ""

	type stats struct {
		min, max, sum float64
		count         int64
	}

	f, err := os.Open(inputPath)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	stationStats := make(map[string]stats)

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		station, tempStr, hasSemi := strings.Cut(line, ";")
		if !hasSemi {
			continue
		}

		temp, err := strconv.ParseFloat(tempStr, 64)
		if err != nil {
			panic(err)
		}

		s, ok := stationStats[station]
		if !ok {
			s.min = temp
			s.max = temp
			s.sum = temp
			s.count = 1
		} else {
			s.min = min(s.min, temp)
			s.max = max(s.max, temp)
			s.sum += temp
			s.count++
		}
		stationStats[station] = s
	}

	stations := make([]string, 0, len(stationStats))
	for station := range stationStats {
		stations = append(stations, station)
	}
	sort.Strings(stations)

	for _, station := range stations {
		s := stationStats[station]
		mean := s.sum / float64(s.count)
		line := fmt.Sprintf("%s=%.1f;%.1f;%.1f(%.1f/%d)\n", station, s.min, s.max, mean, s.sum, s.count)

		fmt.Println(line)
		buffer += line
	}

	return buffer
}

func main() {
	expect_output, err := os.ReadFile(pkg.OUTPUT_PATH)
	if err != nil {
		panic(err)
	}

	expect_output_str := string(expect_output)

	timer := pkg.NewTimer()
	got := solution(pkg.MEASUREMENTS_PATH)
	fmt.Printf("Elapsed: %fms\n", timer.ElapsedAsMilliseconds())

	if got != expect_output_str {
		panic("output not as expected")
	}
}
