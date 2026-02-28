package main

import "fmt"

func cocktailSort(v []int) {
	n := len(v)
	start := 0
	end := n - 1
	swapped := true
	for swapped {
		swapped = false
		for i := start; i < end; i++ {
			if v[i] > v[i+1] {
				v[i], v[i+1] = v[i+1], v[i]
				swapped = true
			}
		}
		if !swapped {
			break
		}
		end--
		swapped = false
		for i := end; i > start; i-- {
			if v[i] < v[i-1] {
				v[i], v[i-1] = v[i-1], v[i]
				swapped = true
			}
		}
		start++
	}
}

func makeTestArray() []int {
	v := make([]int, 100)
	for i := 0; i < 100; i++ {
		v[i] = 100 - i
	}
	return v
}

func main() {
	iters := 50000
	for i := 0; i < iters; i++ {
		v := makeTestArray()
		cocktailSort(v)
	}
	fmt.Println(iters)
}
