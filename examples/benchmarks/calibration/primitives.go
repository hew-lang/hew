package main

import (
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

func positive(text string) int {
	value, err := strconv.Atoi(text)
	if err != nil || value <= 0 {
		panic("iterations and size must be positive integers")
	}
	return value
}

func main() {
	if len(os.Args) != 4 {
		panic("usage: primitives WORKLOAD ITERATIONS SIZE")
	}
	workload := os.Args[1]
	iterations, size := positive(os.Args[2]), positive(os.Args[3])
	var values []int64
	var original []byte
	if workload == "vector_scan" {
		values = make([]int64, size)
		for i := range values {
			values[i] = int64(i % 251)
		}
	}
	if workload == "bytes_shared" || workload == "bytes_cow" {
		original = make([]byte, size)
		for i := range original {
			original[i] = byte(i % 251)
		}
	}
	start := time.Now()
	var checksum int64
	switch workload {
	case "scalar":
		checksum = 1
		for r := 0; r < iterations; r++ {
			checksum = (checksum * 48271) % 2147483647
		}
	case "vector_scan":
		for r := 0; r < iterations; r++ {
			for _, value := range values {
				checksum += value
			}
		}
	case "vector_build":
		for r := 0; r < iterations; r++ {
			values := make([]int64, 0)
			for i := 0; i < size; i++ {
				values = append(values, int64(i%251))
			}
			for _, value := range values {
				checksum += value
			}
			runtime.KeepAlive(values)
		}
	case "bytes_shared":
		for r := 0; r < iterations; r++ {
			snapshot := original
			checksum += int64(snapshot[r%size])
			runtime.KeepAlive(snapshot)
		}
	case "bytes_cow":
		for r := 0; r < iterations; r++ {
			snapshot := make([]byte, size)
			copy(snapshot, original)
			index := r % size
			snapshot[index] = (original[index] + 1) % 251
			checksum += int64(snapshot[index]) + int64(original[index])
			runtime.KeepAlive(snapshot)
		}
	default:
		panic("unknown workload")
	}
	elapsed := time.Since(start).Nanoseconds()
	fmt.Printf("{\"workload\":\"%s\",\"iterations\":%d,\"size\":%d,\"elapsed_ns\":%d,\"checksum\":%d}\n", workload, iterations, size, elapsed, checksum)
}
