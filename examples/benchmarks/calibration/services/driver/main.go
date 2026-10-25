// One external client and result oracle for every service implementation.
package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"sort"
	"sync"
	"sync/atomic"
	"syscall"
	"time"
)

type sample struct {
	Sequence int    `json:"sequence"`
	Latency  int64  `json:"latency_ns"`
	Service  int64  `json:"service_ns"`
	Error    string `json:"error,omitempty"`
}

func payload(sequence int) []byte {
	return []byte(fmt.Sprintf("%c%08d%s", 'A'+sequence%26, sequence, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
}

func expected(path string, body []byte) []byte {
	if path == "/echo" || path == "/wait" || path == "/io" {
		return body
	}
	// Independent periodic-sum oracle; servers execute the full scalar loop.
	n, seed := 200000, int(body[0])
	total := n / 251 * 31375
	for i := 0; i < n%251; i++ {
		total += (seed + i) % 251
	}
	return []byte(fmt.Sprintf("%s:%d", body, total))
}

func main() {
	address := flag.String("url", "http://127.0.0.1:18080", "service URL")
	path := flag.String("path", "/echo", "checked route")
	count := flag.Int("requests", 1000, "offered requests")
	concurrency := flag.Int("concurrency", 16, "maximum in-flight requests, 1 to 32")
	rate := flag.Float64("rate", 0, "offered requests/second; zero is closed-loop")
	backend := flag.String("backend", "", "bind address for the checked five-millisecond HTTP dependency")
	flag.Parse()
	if *count < 1 || *concurrency < 1 || *concurrency > 32 || *rate < 0 {
		panic("invalid load settings")
	}
	if *path != "/echo" && *path != "/compute" && *path != "/wait" && *path != "/fanout" && *path != "/io" {
		panic("unknown route")
	}
	var backendRequests atomic.Int64
	if *path == "/io" {
		if *backend == "" {
			panic("/io requires --backend")
		}
		listener, err := net.Listen("tcp", *backend)
		if err != nil {
			panic(err)
		}
		server := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			body, err := io.ReadAll(io.LimitReader(r.Body, 65))
			if err != nil || len(body) != 64 || r.Method != "POST" || r.URL.Path != "/echo" {
				http.Error(w, "invalid dependency request", 400)
				return
			}
			time.Sleep(5 * time.Millisecond)
			w.Header().Set("Connection", "close")
			backendRequests.Add(1)
			_, _ = w.Write(body)
		})}
		server.SetKeepAlivesEnabled(false)
		go server.Serve(listener)
		defer server.Close()
	}
	transport := &http.Transport{DisableKeepAlives: true, MaxConnsPerHost: *concurrency, ResponseHeaderTimeout: 10 * time.Second}
	client := &http.Client{Transport: transport, Timeout: 15 * time.Second}
	request := func(sequence int, scheduled time.Time) sample {
		started := time.Now()
		result := sample{Sequence: sequence}
		body := payload(sequence)
		req, err := http.NewRequest("POST", *address+*path, bytes.NewReader(body))
		if err == nil {
			req.Header.Set("Content-Type", "application/octet-stream")
			req.Close = true
		}
		var response *http.Response
		if err == nil {
			response, err = client.Do(req)
		}
		if err == nil {
			var got []byte
			got, err = io.ReadAll(io.LimitReader(response.Body, 1025))
			response.Body.Close()
			if err == nil && (response.StatusCode != 200 || !bytes.Equal(got, expected(*path, body))) {
				err = fmt.Errorf("response oracle: status %d body %q", response.StatusCode, got)
			}
		}
		result.Service = time.Since(started).Nanoseconds()
		result.Latency = time.Since(scheduled).Nanoseconds()
		if err != nil {
			result.Error = err.Error()
		}
		return result
	}
	// Warm JITs, connections and runtime pools before the measured interval.
	for i := 0; i < 64; i++ {
		if s := request(i, time.Now()); s.Error != "" {
			panic(s.Error)
		}
	}
	results := make([]sample, *count)
	var group sync.WaitGroup
	var cpuBefore, cpuAfter syscall.Rusage
	if err := syscall.Getrusage(syscall.RUSAGE_SELF, &cpuBefore); err != nil {
		panic(err)
	}
	started := time.Now()
	if *rate == 0 {
		jobs := make(chan int)
		for worker := 0; worker < *concurrency; worker++ {
			group.Add(1)
			go func() {
				defer group.Done()
				for sequence := range jobs {
					results[sequence] = request(sequence, time.Now())
				}
			}()
		}
		for i := 0; i < *count; i++ {
			jobs <- i
		}
		close(jobs)
		group.Wait()
	} else {
		// Never create a hidden client backlog. Missed admissions remain results.
		permits := make(chan struct{}, *concurrency)
		for i := 0; i < *count; i++ {
			due := started.Add(time.Duration(float64(i) / *rate * float64(time.Second)))
			if delay := time.Until(due); delay > 0 {
				time.Sleep(delay)
			}
			if time.Since(due) > time.Millisecond {
				results[i] = sample{Sequence: i, Error: "load generator missed schedule"}
				continue
			}
			select {
			case permits <- struct{}{}:
				group.Add(1)
				go func(sequence int, due time.Time) {
					defer group.Done()
					defer func() { <-permits }()
					results[sequence] = request(sequence, due)
				}(i, due)
			default:
				results[i] = sample{Sequence: i, Error: "in-flight admission limit"}
			}
		}
		group.Wait()
	}
	elapsed := time.Since(started).Nanoseconds()
	if err := syscall.Getrusage(syscall.RUSAGE_SELF, &cpuAfter); err != nil {
		panic(err)
	}
	latencies := make([]int64, 0, *count)
	failures := map[string]int{}
	for _, s := range results {
		if s.Error == "" {
			latencies = append(latencies, s.Latency)
		} else {
			failures[s.Error]++
		}
	}
	sort.Slice(latencies, func(i, j int) bool { return latencies[i] < latencies[j] })
	percentile := func(p float64) int64 {
		if len(latencies) == 0 {
			return 0
		}
		return latencies[int(float64(len(latencies)-1)*p)]
	}
	json.NewEncoder(os.Stdout).Encode(map[string]any{"path": *path, "offered": *count, "completed": len(latencies), "failures": failures, "concurrency": *concurrency, "rate": *rate, "elapsed_ns": elapsed, "p50_ns": percentile(.50), "p95_ns": percentile(.95), "p99_ns": percentile(.99), "samples": results, "backend_requests": backendRequests.Load(), "connection_policy": "HTTP/1.1 close; one request per connection", "client_user_ns": cpuAfter.Utime.Nano() - cpuBefore.Utime.Nano(), "client_system_ns": cpuAfter.Stime.Nano() - cpuBefore.Stime.Nano(), "client_cpu_scope": "timed batch; includes dependency server for /io"})
	if *path == "/io" && backendRequests.Load() != int64(64+len(latencies)) {
		os.Exit(3)
	}
	if len(latencies) != *count {
		os.Exit(2)
	}
}
