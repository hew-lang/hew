// Go: Counter Service
// Same problem as counter_service.hew but in Go.
// Notice: manual channel/mutex management, no supervision,
// no wire types, race conditions possible.
package main

import (
	"fmt"
	"sync"
)

// Go has no actor model. You choose between channels or mutexes.
// Both require discipline. The compiler won't catch data races.

type Counter struct {
	mu    sync.Mutex // You must remember this
	count int32
}

func NewCounter() *Counter {
	return &Counter{}
}

func (c *Counter) Increment(amount int32) int32 {
	c.mu.Lock()         // You must remember this
	defer c.mu.Unlock() // You must remember this
	c.count += amount
	return c.count
}

func (c *Counter) GetCount() int32 {
	c.mu.Lock()         // You must remember this
	defer c.mu.Unlock() // You must remember this
	return c.count
}

// Go has no supervision trees. You build your own restart logic.
type CounterPool struct {
	counters []*Counter
	maxRestarts int
	restartCount int
}

func NewCounterPool(size int) *CounterPool {
	pool := &CounterPool{
		counters:    make([]*Counter, size),
		maxRestarts: 5,
	}
	for i := range pool.counters {
		pool.counters[i] = NewCounter()
	}
	return pool
}

// No language support for restart strategies. You write this yourself.
func (p *CounterPool) RestartChild(idx int) {
	p.restartCount++
	if p.restartCount > p.maxRestarts {
		fmt.Println("Exceeded restart limit")
		return
	}
	p.counters[idx] = NewCounter() // Manual restart
}

// Go has no wire types. You need protobuf + protoc + code generation.
// This is a plain struct. No schema evolution. No compile-time checks.
type CounterUpdate struct {
	CounterID uint32 `json:"counter_id"` // Manual tags
	NewCount  int32  `json:"new_count"`  // Manual tags
	Timestamp uint64 `json:"timestamp"`  // Manual tags
}

func fibonacci(n int32) int32 {
	if n <= 1 {
		return n
	}
	return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
	fmt.Println("Go counter service: manual locks, no supervision, no wire types")
	fmt.Println("Lines of domain logic: 80+")
	fmt.Println("External dependencies: sync, encoding/json (or protobuf)")
	fmt.Println("Runtime overhead: GC pauses, goroutine scheduler")
	result := fibonacci(10)
	fmt.Printf("fibonacci(10) = %d\n", result)
}
