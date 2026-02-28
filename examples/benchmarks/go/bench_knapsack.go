package main

import "fmt"

func knapsack(weights, values []int, n, cap int) int {
	cols := cap + 1
	dp := make([]int, (n+1)*cols)

	for i := 1; i <= n; i++ {
		for w := 0; w <= cap; w++ {
			skip := dp[(i-1)*cols+w]
			wi := weights[i-1]
			if wi <= w {
				take := dp[(i-1)*cols+(w-wi)] + values[i-1]
				if take > skip {
					dp[i*cols+w] = take
				} else {
					dp[i*cols+w] = skip
				}
			} else {
				dp[i*cols+w] = skip
			}
		}
	}
	return dp[n*cols+cap]
}

func main() {
	n := 20
	cap := 100

	weights := make([]int, n)
	values := make([]int, n)
	for k := 0; k < n; k++ {
		weights[k] = k%10 + 1
		values[k] = k%15 + 3
	}

	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += knapsack(weights, values, n, cap)
	}
	fmt.Println("bench_knapsack")
	fmt.Println(checksum)
	fmt.Println(iters)
}
