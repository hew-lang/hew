package main

import "fmt"

func reverseRange(arr []int, start, end int) {
	lo := start
	hi := end
	for lo < hi {
		arr[lo], arr[hi] = arr[hi], arr[lo]
		lo++
		hi--
	}
}

func rotateArray(arr []int, k int) int {
	n := len(arr)
	rot := k % n
	reverseRange(arr, 0, n-1)
	reverseRange(arr, 0, rot-1)
	reverseRange(arr, rot, n-1)
	return arr[0] + arr[n/2] + arr[n-1]
}

func main() {
	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		arr := make([]int, 1000)
		for k := 0; k < 1000; k++ {
			arr[k] = k
		}
		checksum += rotateArray(arr, 337)
	}
	fmt.Println("bench_rotate_array")
	fmt.Println(checksum)
	fmt.Println(iters)
}
