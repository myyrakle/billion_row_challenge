package main

import (
	"flag"
	"fmt"
	"math"
	"math/bits"
	"os"
	"path/filepath"
	"strconv"
	"time"
)

const measurementLimit = uint64(50_000_000)

type generator uint64

func (state *generator) next() uint64 {
	value := uint64(*state)
	value ^= value >> 12
	value ^= value << 25
	value ^= value >> 27
	*state = generator(value)
	return value * 0x2545f4914f6cdd1d
}

func (state *generator) bounded(bound uint64) uint64 {
	value := state.next()
	high, low := bits.Mul64(value, bound)
	if low < bound {
		threshold := -bound % bound
		for low < threshold {
			value = state.next()
			high, low = bits.Mul64(value, bound)
		}
	}
	return high
}

type aggregate struct {
	sum   int64
	count int64
	min   int32
	max   int32
}

func writeFileAtomically(path string, write func(*os.File) error) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	temporary := path + ".tmp"
	file, err := os.Create(temporary)
	if err != nil {
		return err
	}
	if err := write(file); err != nil {
		file.Close()
		os.Remove(temporary)
		return err
	}
	if err := file.Close(); err != nil {
		os.Remove(temporary)
		return err
	}
	return os.Rename(temporary, path)
}

func generate(rows uint64, seed uint64, measurementsPath, outputPath string) error {
	prefixes := make([][]byte, len(cityNames))
	statistics := make([]aggregate, len(cityNames))
	for index, name := range cityNames {
		prefixes[index] = append([]byte(name), ';')
		statistics[index].min = math.MaxInt32
	}

	state := generator(seed)
	const flushSize = 8 << 20
	started := time.Now()
	err := writeFileAtomically(measurementsPath, func(file *os.File) error {
		buffer := make([]byte, 0, flushSize+64)
		for range rows {
			stationIndex := int(state.bounded(uint64(len(cityNames))))
			measurement := int32(state.bounded(measurementLimit))
			buffer = append(buffer, prefixes[stationIndex]...)
			buffer = strconv.AppendInt(buffer, int64(measurement), 10)
			buffer = append(buffer, '\n')

			current := &statistics[stationIndex]
			current.sum += int64(measurement)
			current.count++
			if measurement < current.min {
				current.min = measurement
			}
			if measurement > current.max {
				current.max = measurement
			}
			if len(buffer) >= flushSize {
				if _, err := file.Write(buffer); err != nil {
					return err
				}
				buffer = buffer[:0]
			}
		}
		if len(buffer) > 0 {
			_, err := file.Write(buffer)
			return err
		}
		return nil
	})
	if err != nil {
		return err
	}

	err = writeFileAtomically(outputPath, func(file *os.File) error {
		output := make([]byte, 0, 16*1024)
		for index, name := range cityNames {
			current := statistics[index]
			if current.count == 0 {
				continue
			}
			output = append(output, name...)
			output = append(output, '=')
			output = strconv.AppendInt(output, int64(current.min), 10)
			output = append(output, ';')
			output = strconv.AppendInt(output, int64(current.max), 10)
			output = append(output, ';')
			output = strconv.AppendInt(output, current.sum/current.count, 10)
			output = append(output, '(')
			output = strconv.AppendInt(output, current.sum, 10)
			output = append(output, '/')
			output = strconv.AppendInt(output, current.count, 10)
			output = append(output, ')', '\n')
		}
		_, err := file.Write(output)
		return err
	})
	if err != nil {
		return err
	}

	info, err := os.Stat(measurementsPath)
	if err != nil {
		return err
	}
	elapsed := time.Since(started)
	fmt.Printf("Generated %d rows, %.3f GiB in %s (%.1f MiB/s)\n", rows, float64(info.Size())/(1<<30), elapsed.Round(time.Millisecond), float64(info.Size())/(1<<20)/elapsed.Seconds())
	return nil
}

func main() {
	rows := flag.Uint64("rows", 100_000_000, "number of measurement rows")
	seed := flag.Uint64("seed", 0x4c656d6f6e4d696e, "deterministic random seed")
	measurements := flag.String("measurements", "measurements.txt", "measurement output path")
	output := flag.String("output", "outputs.txt", "expected aggregate output path")
	flag.Parse()
	if *seed == 0 {
		panic("seed must not be zero")
	}
	if err := generate(*rows, *seed, *measurements, *output); err != nil {
		panic(err)
	}
}
