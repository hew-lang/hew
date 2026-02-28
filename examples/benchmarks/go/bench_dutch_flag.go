package main

import "fmt"

func dutchFlag(arr []int) int {
	n := len(arr)
	low := 0
	mid := 0
	high := n - 1

	for mid <= high {
		val := arr[mid]
		if val == 0 {
			arr[low], arr[mid] = arr[mid], arr[low]
			low++
			mid++
		} else if val == 1 {
			mid++
		} else {
			arr[high], arr[mid] = arr[mid], arr[high]
			high--
		}
	}
	return arr[0] + arr[n/2] + arr[n-1]
}

func main() {
	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		arr := make([]int, 1000)
		for k := 0; k < 1000; k++ {
			arr[k] = k % 3
		}
		checksum += dutchFlag(arr)
	}
	fmt.Println("bench_dutch_flag")
	fmt.Println(checksum)
	fmt.Println(iters)
}
