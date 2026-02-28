// Benchmark: Interpolation Search
// Estimates position based on value distribution in uniformly distributed data.

package main

import "fmt"

func interpolationSearch(v []int, target int) int {
	lo := 0
	hi := len(v) - 1
	for lo <= hi {
		loVal := v[lo]
		hiVal := v[hi]
		if loVal == hiVal {
			if loVal == target {
				return lo
			}
			return -1
		}
		if target < loVal {
			return -1
		}
		if target > hiVal {
			return -1
		}
		pos := lo + (target-loVal)*(hi-lo)/(hiVal-loVal)
		val := v[pos]
		if val == target {
			return pos
		}
		if val < target {
			lo = pos + 1
		} else {
			hi = pos - 1
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
		sum += interpolationSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
