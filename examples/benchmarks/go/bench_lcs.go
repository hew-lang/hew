package main

import "fmt"

func lcs(a, b []int, m, n int) int {
	cols := n + 1
	dp := make([]int, (m+1)*cols)

	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			if a[i-1] == b[j-1] {
				dp[i*cols+j] = dp[(i-1)*cols+(j-1)] + 1
			} else {
				top := dp[(i-1)*cols+j]
				left := dp[i*cols+(j-1)]
				if top > left {
					dp[i*cols+j] = top
				} else {
					dp[i*cols+j] = left
				}
			}
		}
	}
	return dp[m*cols+n]
}

func main() {
	m := 50
	n := 50

	a := make([]int, m)
	b := make([]int, n)
	for k := 0; k < m; k++ {
		a[k] = k % 7
	}
	for k := 0; k < n; k++ {
		b[k] = k % 9
	}

	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += lcs(a, b, m, n)
	}
	fmt.Println("bench_lcs")
	fmt.Println(checksum)
	fmt.Println(iters)
}
