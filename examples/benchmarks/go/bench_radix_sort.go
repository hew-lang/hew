package main

import "fmt"

func getMax(v []int) int {
	maxVal := v[0]
	for i := 1; i < len(v); i++ {
		if v[i] > maxVal {
			maxVal = v[i]
		}
	}
	return maxVal
}

func countSortByExp(v []int, exp int) {
	n := len(v)
	output := make([]int, n)
	count := make([]int, 10)
	for i := 0; i < n; i++ {
		idx := (v[i] / exp) % 10
		count[idx]++
	}
	for i := 1; i < 10; i++ {
		count[i] += count[i-1]
	}
	for i := n - 1; i >= 0; i-- {
		idx := (v[i] / exp) % 10
		pos := count[idx] - 1
		output[pos] = v[i]
		count[idx]--
	}
	for i := 0; i < n; i++ {
		v[i] = output[i]
	}
}

func radixSort(v []int) {
	n := len(v)
	if n == 0 {
		return
	}
	maxVal := getMax(v)
	for exp := 1; maxVal/exp > 0; exp *= 10 {
		countSortByExp(v, exp)
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
		radixSort(v)
	}
	fmt.Println(iters)
}
