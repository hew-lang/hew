// Benchmark: Fibonacci Search
// Uses Fibonacci numbers to divide the search space.

package main

import "fmt"

func fibonacciSearch(v []int, target int) int {
	n := len(v)
	if n == 0 {
		return -1
	}
	fib2 := 0
	fib1 := 1
	fib := fib2 + fib1
	for fib < n {
		fib2 = fib1
		fib1 = fib
		fib = fib2 + fib1
	}
	offset := -1
	for fib > 1 {
		i := offset + fib2
		if i >= n-1 {
			i = n - 1
		}
		val := v[i]
		if val < target {
			fib = fib1
			fib1 = fib2
			fib2 = fib - fib1
			offset = i
		} else {
			if val > target {
				fib = fib2
				fib1 = fib1 - fib2
				fib2 = fib - fib1
			} else {
				return i
			}
		}
	}
	if fib1 == 1 {
		if offset+1 < n {
			if v[offset+1] == target {
				return offset + 1
			}
		}
	}
	return -1
}

func main() {
	v := make([]int, 10000)
	for i := 0; i < 10000; i++ {
		v[i] = i
	}
	iters := 1000000
	sum := 0
	for iter := 0; iter < iters; iter++ {
		sum += fibonacciSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
