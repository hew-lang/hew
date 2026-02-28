package main

import "fmt"

func partition(v []int, low int, high int) int {
	pivot := v[high]
	i := low - 1
	for j := low; j < high; j++ {
		if v[j] <= pivot {
			i++
			v[i], v[j] = v[j], v[i]
		}
	}
	v[i+1], v[high] = v[high], v[i+1]
	return i + 1
}

func quickSortRange(v []int, low int, high int) {
	if low < high {
		pi := partition(v, low, high)
		quickSortRange(v, low, pi-1)
		quickSortRange(v, pi+1, high)
	}
}

func quickSort(v []int) {
	n := len(v)
	if n > 1 {
		quickSortRange(v, 0, n-1)
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
		quickSort(v)
	}
	fmt.Println(iters)
}
