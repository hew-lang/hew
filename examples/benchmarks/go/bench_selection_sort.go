package main

import "fmt"

func selectionSort(v []int) {
	n := len(v)
	for i := 0; i < n; i++ {
		minIdx := i
		for j := i + 1; j < n; j++ {
			if v[j] < v[minIdx] {
				minIdx = j
			}
		}
		v[i], v[minIdx] = v[minIdx], v[i]
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
		selectionSort(v)
	}
	fmt.Println(iters)
}
