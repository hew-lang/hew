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

type streamResult struct {
	checksum  int64
	elapsedNs int64
	samples   []int64
}

func streamWorker(input <-chan int64, output chan<- int64, ready chan<- struct{}, gate <-chan struct{}, n int64) {
	ready <- struct{}{}
	<-gate
	for index := int64(0); index < n; index++ {
		value := <-input
		output <- value + 1
	}
}

func streamClient(requests chan<- int64, replies <-chan int64, ready chan<- struct{}, gate <-chan struct{}, n int64, sampled bool, done chan<- streamResult) {
	ready <- struct{}{}
	<-gate
	start := time.Now()
	var checksum int64
	samples := make([]int64, 0)
	for index := int64(0); index < n; index++ {
		if sampled && index < 640000 && index%64 == 0 {
			before := time.Now()
			requests <- index
			reply := <-replies
			samples = append(samples, time.Since(before).Nanoseconds())
			if reply != index+1 {
				panic("stream sequence mismatch")
			}
		} else {
			requests <- index
			if reply := <-replies; reply != index+1 {
				panic("stream sequence mismatch")
			}
		}
		checksum += index
	}
	done <- streamResult{checksum: checksum, elapsedNs: time.Since(start).Nanoseconds(), samples: samples}
}

func pipelineProducer(output chan<- int64, ready chan<- struct{}, gate <-chan struct{}, n, id, size int64, done chan<- struct{}) {
	ready <- struct{}{}
	<-gate
	for index := int64(0); index < n; index++ {
		output <- index*size + id
	}
	close(output)
	done <- struct{}{}
}

func pipelineDrain(input <-chan int64, ready chan<- struct{}, gate <-chan struct{}, n, id, size int64, sampled bool, done chan<- streamResult) {
	ready <- struct{}{}
	<-gate
	var checksum int64
	samples := make([]int64, 0)
	for index := int64(0); index < n; index++ {
		expected := index*size + id
		var value int64
		if sampled && expected < 640000 && expected%64 == 0 {
			before := time.Now()
			value = <-input
			samples = append(samples, time.Since(before).Nanoseconds())
		} else {
			value = <-input
		}
		if value != expected {
			panic("pipeline sequence mismatch")
		}
		checksum += value % 251
	}
	done <- streamResult{checksum: checksum, samples: samples}
}

func runStreamRoundtrip(n int64, sampled bool) streamResult {
	requests, replies := make(chan int64, 1), make(chan int64, 1)
	ready, gate := make(chan struct{}, 2), make(chan struct{})
	done := make(chan streamResult, 1)
	go streamWorker(requests, replies, ready, gate, n)
	go streamClient(requests, replies, ready, gate, n, sampled, done)
	<-ready
	<-ready
	close(gate)
	result := <-done
	return result
}

func runStreamPipeline(n, size int64, sampled bool) streamResult {
	ready, gate := make(chan struct{}, 2*size), make(chan struct{})
	producers := make(chan struct{}, size)
	drains := make(chan streamResult, size)
	for id := int64(0); id < size; id++ {
		messages := make(chan int64, 1)
		go pipelineProducer(messages, ready, gate, n, id, size, producers)
		go pipelineDrain(messages, ready, gate, n, id, size, sampled, drains)
	}
	for endpoint := int64(0); endpoint < 2*size; endpoint++ {
		<-ready
	}
	start := time.Now()
	close(gate)
	var result streamResult
	completed := make([]streamResult, 0, size)
	for pair := int64(0); pair < size; pair++ {
		completed = append(completed, <-drains)
	}
	for pair := int64(0); pair < size; pair++ {
		<-producers
	}
	result.elapsedNs = time.Since(start).Nanoseconds()
	for _, pair := range completed {
		result.checksum += pair.checksum
		result.samples = append(result.samples, pair.samples...)
	}
	return result
}

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
	case "stream_roundtrip":
		result := runStreamRoundtrip(n, true)
		m.Checksum, m.Elapsed, m.Latencies = result.checksum, result.elapsedNs, result.samples
	case "stream_roundtrip_alloc":
		result := runStreamRoundtrip(n, false)
		m.Checksum, m.Elapsed, m.Latencies = result.checksum, result.elapsedNs, result.samples
	case "stream_pipeline":
		result := runStreamPipeline(n, size, true)
		m.Checksum, m.Elapsed, m.Latencies = result.checksum, result.elapsedNs, result.samples
	case "stream_pipeline_alloc":
		result := runStreamPipeline(n, size, false)
		m.Checksum, m.Elapsed, m.Latencies = result.checksum, result.elapsedNs, result.samples
	default:
		panic("unknown concurrency workload")
	}
	if m.Elapsed == 0 {
		m.Elapsed = time.Since(start).Nanoseconds()
	}
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
