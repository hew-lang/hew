package main

import "fmt"

func climbingStairs(n int) int {
	dp := make([]int, n+1)
	dp[0] = 1
	dp[1] = 1

	for i := 2; i <= n; i++ {
		dp[i] = dp[i-1] + dp[i-2]
	}
	return dp[n]
}

func main() {
	n := 50
	iters := 5000000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += climbingStairs(n)
	}
	fmt.Println("bench_climbing_stairs")
	fmt.Println(checksum)
	fmt.Println(iters)
}
