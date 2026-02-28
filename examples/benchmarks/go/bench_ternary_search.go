// Benchmark: Ternary Search
// Divides search space into three parts instead of two.

package main

import "fmt"

func ternarySearch(v []int, target int) int {
	lo := 0
	hi := len(v) - 1
	for lo <= hi {
		third := (hi - lo) / 3
		mid1 := lo + third
		mid2 := hi - third
		val1 := v[mid1]
		val2 := v[mid2]
		if val1 == target {
			return mid1
		}
		if val2 == target {
			return mid2
		}
		if target < val1 {
			hi = mid1 - 1
		} else {
			if target > val2 {
				lo = mid2 + 1
			} else {
				lo = mid1 + 1
				hi = mid2 - 1
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
		sum += ternarySearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
