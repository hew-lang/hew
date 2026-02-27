package main

import "fmt"

func insertionSort(v []int) {
	n := len(v)
	for i := 1; i < n; i++ {
		key := v[i]
		j := i - 1
		for j >= 0 && v[j] > key {
			v[j+1] = v[j]
			j = j - 1
		}
		v[j+1] = key
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
	iters := 100000
	for i := 0; i < iters; i++ {
		v := makeTestArray()
		insertionSort(v)
	}
	fmt.Println(iters)
}
