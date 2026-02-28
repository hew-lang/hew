// Benchmark: Jump Search
// Jumps ahead by sqrt(n) steps, then does linear search in the block.

package main

import "fmt"

func isqrt(n int) int {
	if n <= 0 {
		return 0
	}
	x := n
	y := (x + 1) / 2
	for y < x {
		x = y
		y = (x + n/x) / 2
	}
	return x
}

func jumpSearch(v []int, target int) int {
	n := len(v)
	if n == 0 {
		return -1
	}
	step := isqrt(n)
	prev := 0
	curr := step
	found := -1
	done := 0
	// Jump ahead
	for curr < n {
		if done == 0 {
			if v[curr] >= target {
				j := prev
				for j <= curr {
					if v[j] == target {
						if found == -1 {
							found = j
						}
					}
					j = j + 1
				}
				done = 1
			}
		}
		if done == 0 {
			prev = curr + 1
		}
		curr = curr + step
	}
	// Search remaining elements
	if done == 0 {
		j := prev
		for j < n {
			if v[j] == target {
				if found == -1 {
					found = j
				}
			}
			j = j + 1
		}
	}
	return found
}

func main() {
	v := make([]int, 10000)
	for i := 0; i < 10000; i++ {
		v[i] = i
	}
	iters := 1000000
	sum := 0
	for iter := 0; iter < iters; iter++ {
		sum += jumpSearch(v, 7777)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
