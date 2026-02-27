package main

import "fmt"

func catalan(n int) int {
	dp := []int{1, 1}
	for i := 2; i <= n; i++ {
		dp = append(dp, 0)
		for j := 0; j < i; j++ {
			dp[i] = dp[i] + dp[j]*dp[i-1-j]
		}
	}
	return dp[n]
}

func main() {
	iters := 500000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += catalan(15)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
