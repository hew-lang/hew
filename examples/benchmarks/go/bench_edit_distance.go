package main

import "fmt"

func editDistance(a, b []int, m, n int) int {
	cols := n + 1
	dp := make([]int, (m+1)*cols)

	for i := 0; i <= m; i++ {
		dp[i*cols+0] = i
	}
	for j := 0; j <= n; j++ {
		dp[0*cols+j] = j
	}

	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			if a[i-1] == b[j-1] {
				dp[i*cols+j] = dp[(i-1)*cols+(j-1)]
			} else {
				rep := dp[(i-1)*cols+(j-1)] + 1
				del := dp[(i-1)*cols+j] + 1
				ins := dp[i*cols+(j-1)] + 1
				best := rep
				if del < best {
					best = del
				}
				if ins < best {
					best = ins
				}
				dp[i*cols+j] = best
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
		a[k] = k % 11
	}
	for k := 0; k < n; k++ {
		b[k] = k % 13
	}

	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += editDistance(a, b, m, n)
	}
	fmt.Println("bench_edit_distance")
	fmt.Println(checksum)
	fmt.Println(iters)
}
