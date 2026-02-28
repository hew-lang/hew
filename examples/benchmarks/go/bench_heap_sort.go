package main

import "fmt"

func heapify(v []int, n int, i int) {
	largest := i
	left := 2*i + 1
	right := 2*i + 2
	if left < n && v[left] > v[largest] {
		largest = left
	}
	if right < n && v[right] > v[largest] {
		largest = right
	}
	if largest != i {
		v[i], v[largest] = v[largest], v[i]
		heapify(v, n, largest)
	}
}

func heapSort(v []int) {
	n := len(v)
	for i := n/2 - 1; i >= 0; i-- {
		heapify(v, n, i)
	}
	for i := n - 1; i > 0; i-- {
		v[0], v[i] = v[i], v[0]
		heapify(v, i, 0)
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
		heapSort(v)
	}
	fmt.Println(iters)
}
