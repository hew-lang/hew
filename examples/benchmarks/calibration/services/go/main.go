package main

import (
	"bytes"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"runtime"
	"strconv"
	"sync"
	"time"
)

const bodySize = 64

var backendClient = &http.Client{
	Timeout:       10 * time.Second,
	Transport:     &http.Transport{DisableKeepAlives: true, DisableCompression: true},
	CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
}

func backendEcho(body []byte) error {
	backend := os.Getenv("BENCH_BACKEND")
	if _, _, err := net.SplitHostPort(backend); err != nil {
		return fmt.Errorf("BENCH_BACKEND must be host:port: %w", err)
	}
	req, err := http.NewRequest(http.MethodPost, "http://"+backend+"/echo", bytes.NewReader(body))
	if err != nil {
		return err
	}
	req.Header.Set("Accept-Encoding", "identity")
	req.Close = true
	resp, err := backendClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	echoed, err := io.ReadAll(io.LimitReader(resp.Body, bodySize+1))
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK || !bytes.Equal(echoed, body) {
		return fmt.Errorf("backend returned status %d and %d body bytes", resp.StatusCode, len(echoed))
	}
	return nil
}

func envInt(name string, fallback int) int {
	raw, ok := os.LookupEnv(name)
	if !ok {
		return fallback
	}
	value, err := strconv.Atoi(raw)
	if err != nil || value < 1 {
		panic(fmt.Sprintf("%s must be a positive integer", name))
	}
	return value
}

func checkedBody(r *http.Request) ([]byte, bool) {
	if r.Method != http.MethodPost {
		return nil, false
	}
	body, err := io.ReadAll(io.LimitReader(r.Body, bodySize+1))
	if err != nil || len(body) != bodySize {
		return body, false
	}
	for _, b := range body {
		if b > 127 {
			return body, false
		}
	}
	return body, true
}

func work(seed byte, start, count int) uint64 {
	var total uint64
	for i := start; i < start+count; i++ {
		total += uint64((int(seed) + i) % 251)
	}
	return total
}

func main() {
	workers := envInt("BENCH_WORKERS", 2)
	runtime.GOMAXPROCS(workers)
	bind := os.Getenv("BENCH_BIND")
	if bind == "" {
		bind = "127.0.0.1:18080"
	}
	limit := make(chan struct{}, 32)
	mux := http.NewServeMux()
	mux.HandleFunc("POST /echo", func(w http.ResponseWriter, r *http.Request) {
		body, ok := checkedBody(r)
		if !ok {
			http.Error(w, "expected exactly 64 ASCII bytes", http.StatusBadRequest)
			return
		}
		w.Header().Set("Connection", "close")
		_, _ = w.Write(body)
	})
	mux.HandleFunc("POST /wait", func(w http.ResponseWriter, r *http.Request) {
		body, ok := checkedBody(r)
		if !ok {
			http.Error(w, "expected exactly 64 ASCII bytes", http.StatusBadRequest)
			return
		}
		time.Sleep(5 * time.Millisecond)
		w.Header().Set("Connection", "close")
		_, _ = w.Write(body)
	})
	mux.HandleFunc("POST /io", func(w http.ResponseWriter, r *http.Request) {
		body, ok := checkedBody(r)
		if !ok {
			http.Error(w, "expected exactly 64 ASCII bytes", http.StatusBadRequest)
			return
		}
		if err := backendEcho(body); err != nil {
			http.Error(w, "backend I/O failed: "+err.Error(), http.StatusBadGateway)
			return
		}
		_, _ = w.Write(body)
	})
	mux.HandleFunc("POST /compute", func(w http.ResponseWriter, r *http.Request) {
		body, ok := checkedBody(r)
		if !ok {
			http.Error(w, "expected exactly 64 ASCII bytes", http.StatusBadRequest)
			return
		}
		total := work(body[0], 0, 200000)
		w.Header().Set("Connection", "close")
		_, _ = fmt.Fprintf(w, "%s:%d", body, total)
	})
	mux.HandleFunc("POST /fanout", func(w http.ResponseWriter, r *http.Request) {
		body, ok := checkedBody(r)
		if !ok {
			http.Error(w, "expected exactly 64 ASCII bytes", http.StatusBadRequest)
			return
		}
		var total uint64
		var wg sync.WaitGroup
		var mu sync.Mutex
		for part := 0; part < 8; part++ {
			wg.Add(1)
			go func(part int) {
				defer wg.Done()
				partial := work(body[0], part*25000, 25000)
				mu.Lock()
				total += partial
				mu.Unlock()
			}(part)
		}
		wg.Wait()
		w.Header().Set("Connection", "close")
		_, _ = fmt.Fprintf(w, "%s:%d", body, total)
	})
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Connection", "close")
		limit <- struct{}{}
		defer func() { <-limit }()
		mux.ServeHTTP(w, r)
	})
	server := &http.Server{Addr: bind, Handler: handler, DisableGeneralOptionsHandler: true}
	listener, err := net.Listen("tcp", bind)
	if err != nil {
		panic(err)
	}
	fmt.Printf("READY %s\n", listener.Addr().String())
	if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
		panic(err)
	}
}
