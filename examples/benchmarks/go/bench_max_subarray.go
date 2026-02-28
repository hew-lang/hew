package main

import "fmt"

func maxSubarray(arr []int) int {
	maxEnding := arr[0]
	maxSoFar := arr[0]
	for i := 1; i < len(arr); i++ {
		val := arr[i]
		if val > maxEnding+val {
			maxEnding = val
		} else {
			maxEnding = maxEnding + val
		}
		if maxEnding > maxSoFar {
			maxSoFar = maxEnding
		}
	}
	return maxSoFar
}

func main() {
	arr := make([]int, 1000)
	for k := 0; k < 1000; k++ {
		rem := k % 3
		if rem == 0 {
			arr[k] = k%7 + 1
		} else if rem == 1 {
			arr[k] = -(k%5 + 1)
		} else {
			arr[k] = k % 10
		}
	}

	iters := 500000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += maxSubarray(arr)
	}
	fmt.Println("bench_max_subarray")
	fmt.Println(checksum)
	fmt.Println(iters)
}
