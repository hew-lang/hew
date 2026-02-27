package main

import "fmt"

func lis(arr []int, n int) int {
	dp := make([]int, n)
	for idx := 0; idx < n; idx++ {
		dp[idx] = 1
	}

	for i := 1; i < n; i++ {
		for j := 0; j < i; j++ {
			if arr[j] < arr[i] {
				candidate := dp[j] + 1
				if candidate > dp[i] {
					dp[i] = candidate
				}
			}
		}
	}

	best := 0
	for i := 0; i < n; i++ {
		if dp[i] > best {
			best = dp[i]
		}
	}
	return best
}

func main() {
	n := 500
	arr := make([]int, n)
	for k := 0; k < n; k++ {
		arr[k] = (k*37 + 13) % 100
	}

	iters := 5000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += lis(arr, n)
	}
	fmt.Println("bench_lis")
	fmt.Println(checksum)
	fmt.Println(iters)
}
