// Benchmark: Linear Search
// Searches sequentially through the array from start to end.

package main

import "fmt"

func linearSearch(v []int, target int) int {
	i := 0
	for i < len(v) {
		if v[i] == target {
			return i
		}
		i = i + 1
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
		sum += linearSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
