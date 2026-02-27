package main

import "fmt"

func rodCutting(prices []int, n int) int {
	dp := make([]int, n+1)

	for i := 1; i <= n; i++ {
		for j := 1; j <= i; j++ {
			candidate := prices[j-1] + dp[i-j]
			if candidate > dp[i] {
				dp[i] = candidate
			}
		}
	}
	return dp[n]
}

func main() {
	n := 50
	prices := make([]int, n)
	for k := 0; k < n; k++ {
		prices[k] = k%10 + 1 + k/5
	}

	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += rodCutting(prices, n)
	}
	fmt.Println("bench_rod_cutting")
	fmt.Println(checksum)
	fmt.Println(iters)
}
