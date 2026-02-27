package main

import "fmt"

func twoSum(arr []int, target int) int {
	n := len(arr)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if arr[i]+arr[j] == target {
				return i + j
			}
		}
	}
	return -1
}

func main() {
	arr := make([]int, 1000)
	for k := 0; k < 1000; k++ {
		arr[k] = k
	}
	target := 998 + 999

	iters := 10000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += twoSum(arr, target)
	}
	fmt.Println("bench_two_sum")
	fmt.Println(checksum)
	fmt.Println(iters)
}
