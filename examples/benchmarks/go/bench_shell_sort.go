package main

import "fmt"

func shellSort(v []int) {
	n := len(v)
	for gap := n / 2; gap > 0; gap = gap / 2 {
		for i := gap; i < n; i++ {
			temp := v[i]
			j := i
			for j >= gap && v[j-gap] > temp {
				v[j] = v[j-gap]
				j = j - gap
			}
			v[j] = temp
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
	iters := 100000
	for i := 0; i < iters; i++ {
		v := makeTestArray()
		shellSort(v)
	}
	fmt.Println(iters)
}
