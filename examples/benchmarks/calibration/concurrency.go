// Scheduler calibration. Actor/channel setup and one warm-up task precede timing.
package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

type measurement struct {
	Workload   string
	Iterations int64
	Size       int64
	Elapsed    int64
	Checksum   int64
	Latencies  []int64
}

type request struct{ reply chan int64 }

func counter() (chan request, chan struct{}) {
	requests := make(chan request)
	done := make(chan struct{})
	go func() {
		defer close(done)
		var count int64
		for message := range requests {
			count++
			message.reply <- count
		}
	}()
	return requests, done
}

func main() {
	if len(os.Args) != 4 {
		panic("usage: concurrency WORKLOAD ITERATIONS SIZE")
	}
	n, e := strconv.ParseInt(os.Args[2], 10, 64)
	if e != nil || n <= 0 {
		panic("iterations must be positive")
	}
	size, e := strconv.ParseInt(os.Args[3], 10, 64)
	if e != nil || size <= 0 {
		panic("size must be positive")
	}
	m := measurement{Workload: os.Args[1], Iterations: n, Size: size, Latencies: []int64{}}
	warm := make(chan struct{})
	go func() { close(warm) }()
	<-warm
	var channels []chan request
	var stops []chan struct{}
	if m.Workload == "actor_roundtrip" || m.Workload == "actor_fanout" {
		count := size
		if m.Workload == "actor_roundtrip" {
			count = 1
		}
		for i := int64(0); i < count; i++ {
			messages, done := counter()
			channels = append(channels, messages)
			stops = append(stops, done)
		}
	}
	start := time.Now()
	switch m.Workload {
	case "task_ready":
		for offset := int64(0); offset < n; offset += size {
			end := offset + size
			if end > n {
				end = n
			}
			results := make([]chan int64, 0, end-offset)
			for index := offset; index < end; index++ {
				result := make(chan int64, 1)
				results = append(results, result)
				go func(i int64) { result <- i % 251 }(index)
			}
			for _, result := range results {
				m.Checksum += <-result
			}
		}
	case "actor_roundtrip":
		reply := make(chan int64)
		for i := int64(0); i < n; i++ {
			before := time.Now()
			channels[0] <- request{reply}
			m.Checksum += <-reply
			elapsed := time.Since(before).Nanoseconds()
			if i < 10000 {
				m.Latencies = append(m.Latencies, elapsed)
			}
		}
	case "actor_fanout":
		results := make([]chan int64, 0, size)
		for _, messages := range channels {
			result := make(chan int64, 1)
			results = append(results, result)
			go func(in chan request) {
				reply := make(chan int64)
				var sum int64
				for i := int64(0); i < n; i++ {
					in <- request{reply}
					sum += <-reply
				}
				result <- sum
			}(messages)
		}
		for _, result := range results {
			m.Checksum += <-result
		}
	case "timer":
		for i := int64(0); i < n; i++ {
			before := time.Now()
			time.Sleep(time.Millisecond)
			overshoot := time.Since(before).Nanoseconds() - 1000000
			if overshoot < 0 {
				overshoot = 0
			}
			if i < 10000 {
				m.Latencies = append(m.Latencies, overshoot)
			}
		}
		m.Checksum = n
	default:
		panic("unknown concurrency workload")
	}
	m.Elapsed = time.Since(start).Nanoseconds()
	for index, messages := range channels {
		close(messages)
		<-stops[index]
	}
	fmt.Printf("{\"workload\":%q,\"iterations\":%d,\"size\":%d,\"elapsed_ns\":%d,\"checksum\":%d,\"latencies_ns\":[", m.Workload, n, size, m.Elapsed, m.Checksum)
	for index, latency := range m.Latencies {
		if index > 0 {
			fmt.Print(",")
		}
		fmt.Print(latency)
	}
	fmt.Println("]}")
}
