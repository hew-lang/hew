package main

import "fmt"

func removeDupes(arr []int) int {
	n := len(arr)
	if n == 0 {
		return 0
	}
	write := 1
	for read := 1; read < n; read++ {
		if arr[read] != arr[write-1] {
			arr[write] = arr[read]
			write++
		}
	}
	return write
}

func main() {
	iters := 200000
	checksum := 0
	for i := 0; i < iters; i++ {
		arr := make([]int, 1000)
		for k := 0; k < 1000; k++ {
			arr[k] = k / 3
		}
		checksum += removeDupes(arr)
	}
	fmt.Println("bench_remove_dupes")
	fmt.Println(checksum)
	fmt.Println(iters)
}
