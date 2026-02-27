package main

import "fmt"

func bubbleSort(v []int) {
	n := len(v)
	for i := 0; i < n; i++ {
		for j := 0; j < n-1-i; j++ {
			if v[j] > v[j+1] {
				v[j], v[j+1] = v[j+1], v[j]
			}
		}
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
		bubbleSort(v)
	}
	fmt.Println(iters)
}
