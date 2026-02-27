package main

import "fmt"

func coinChange(coins []int, numCoins, amount int) int {
	const INF = 999999
	dp := make([]int, amount+1)
	for idx := 1; idx <= amount; idx++ {
		dp[idx] = INF
	}

	for i := 0; i < numCoins; i++ {
		c := coins[i]
		for a := c; a <= amount; a++ {
			prev := dp[a-c] + 1
			if prev < dp[a] {
				dp[a] = prev
			}
		}
	}
	return dp[amount]
}

func main() {
	coins := []int{1, 5, 10, 25, 50, 100}
	numCoins := 6
	amount := 500

	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += coinChange(coins, numCoins, amount)
	}
	fmt.Println("bench_coin_change")
	fmt.Println(checksum)
	fmt.Println(iters)
}
