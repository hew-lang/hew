// Benchmark: Sentinel Search
// Places target at end of array to eliminate bounds check in loop.

package main

import "fmt"

func sentinelSearch(v []int, target int) int {
	n := len(v)
	if n == 0 {
		return -1
	}
	last := v[n-1]
	v[n-1] = target
	i := 0
	for v[i] != target {
		i = i + 1
	}
	v[n-1] = last
	if i < n-1 {
		return i
	}
	if last == target {
		return n - 1
	}
	return -1
}

func main() {
	v := make([]int, 10000)
	for i := 0; i < 10000; i++ {
		v[i] = i
	}
	iters := 100000
	sum := 0
	for iter := 0; iter < iters; iter++ {
		sum += sentinelSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
