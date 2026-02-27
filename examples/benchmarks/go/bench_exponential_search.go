// Benchmark: Exponential Search
// Finds range by doubling index, then binary searches within that range.

package main

import "fmt"

func binarySearchRange(v []int, target int, lo int, hi int) int {
	for lo <= hi {
		mid := lo + (hi-lo)/2
		val := v[mid]
		if val == target {
			return mid
		}
		if val < target {
			lo = mid + 1
		} else {
			hi = mid - 1
		}
	}
	return -1
}

func exponentialSearch(v []int, target int) int {
	n := len(v)
	if n == 0 {
		return -1
	}
	if v[0] == target {
		return 0
	}
	bound := 1
	for bound < n {
		if v[bound] >= target {
			// Binary search in [bound/2, bound]
			return binarySearchRange(v, target, bound/2, bound)
		}
		bound = bound * 2
	}
	// Binary search in [bound/2, n-1]
	return binarySearchRange(v, target, bound/2, n-1)
}

func main() {
	v := make([]int, 10000)
	for i := 0; i < 10000; i++ {
		v[i] = i
	}
	iters := 1000000
	sum := 0
	for iter := 0; iter < iters; iter++ {
		sum += exponentialSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
