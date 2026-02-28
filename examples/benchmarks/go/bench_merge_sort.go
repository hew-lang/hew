package main

import "fmt"

func merge(v []int, left int, mid int, right int) {
	leftArr := make([]int, mid-left+1)
	rightArr := make([]int, right-mid)
	for i := 0; i < len(leftArr); i++ {
		leftArr[i] = v[left+i]
	}
	for i := 0; i < len(rightArr); i++ {
		rightArr[i] = v[mid+1+i]
	}
	li := 0
	ri := 0
	k := left
	for li < len(leftArr) && ri < len(rightArr) {
		if leftArr[li] <= rightArr[ri] {
			v[k] = leftArr[li]
			li++
		} else {
			v[k] = rightArr[ri]
			ri++
		}
		k++
	}
	for li < len(leftArr) {
		v[k] = leftArr[li]
		li++
		k++
	}
	for ri < len(rightArr) {
		v[k] = rightArr[ri]
		ri++
		k++
	}
}

func mergeSortRange(v []int, left int, right int) {
	if left < right {
		mid := left + (right-left)/2
		mergeSortRange(v, left, mid)
		mergeSortRange(v, mid+1, right)
		merge(v, left, mid, right)
	}
}

func mergeSort(v []int) {
	n := len(v)
	if n > 1 {
		mergeSortRange(v, 0, n-1)
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
		mergeSort(v)
	}
	fmt.Println(iters)
}
