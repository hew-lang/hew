package main

import "fmt"

func countingSort(v []int) {
	n := len(v)
	if n == 0 {
		return
	}
	maxVal := v[0]
	for i := 1; i < n; i++ {
		if v[i] > maxVal {
			maxVal = v[i]
		}
	}
	count := make([]int, maxVal+1)
	for i := 0; i < n; i++ {
		count[v[i]]++
	}
	pos := 0
	for i := 0; i <= maxVal; i++ {
		c := count[i]
		for c > 0 {
			v[pos] = i
			pos++
			c--
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
	iters := 200000
	for i := 0; i < iters; i++ {
		v := makeTestArray()
		countingSort(v)
	}
	fmt.Println(iters)
}
