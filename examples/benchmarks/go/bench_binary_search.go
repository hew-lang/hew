// Benchmark: Binary Search
// Repeatedly halves the search space by comparing the middle element.

package main

import "fmt"

func binarySearch(v []int, target int) int {
	lo := 0
	hi := len(v) - 1
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

func main() {
	v := make([]int, 10000)
	for i := 0; i < 10000; i++ {
		v[i] = i
	}
	iters := 1000000
	sum := 0
	for iter := 0; iter < iters; iter++ {
		sum += binarySearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
