package main

import (
	"brc/internal/measurement"
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func min(a, b int64) int64 {
	if a < b {
		return a
	}
	return b
}

func max(a, b int64) int64 {
	if a > b {
		return a
	}
	return b
}

func solution(inputPath string) string {
	buffer := ""

	type stats struct {
		min, max, sum int64
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

		temp, err := strconv.ParseInt(tempStr, 10, 64)
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
		mean := s.sum / s.count
		line := fmt.Sprintf("%s=%d;%d;%d(%d/%d)\n", station, s.min, s.max, mean, s.sum, s.count)

		buffer += line
	}

	return buffer
}

func main() {
	expect_output, err := os.ReadFile(measurement.OUTPUT_PATH)
	if err != nil {
		panic(err)
	}

	expect_output_str := string(expect_output)

	timer := measurement.NewTimer()
	got := solution(measurement.MEASUREMENTS_PATH)
	fmt.Printf("Elapsed: %fms\n", timer.ElapsedAsMilliseconds())

	if expect_output_str != got {
		fmt.Println("Expect:")
		fmt.Println(expect_output_str)
		fmt.Println("Got:")
		fmt.Println(got)
		panic("Not matched")
	} else {
		fmt.Println("Matched")
	}
}
