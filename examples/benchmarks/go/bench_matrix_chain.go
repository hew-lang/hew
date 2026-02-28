package main

import "fmt"

func matrixChain(dims []int, n int) int {
	const INF = 999999
	dp := make([]int, n*n)

	for length := 2; length <= n; length++ {
		for i := 0; i <= n-length; i++ {
			j := i + length - 1
			dp[i*n+j] = INF
			for k := i; k < j; k++ {
				cost := dp[i*n+k] + dp[(k+1)*n+j] + dims[i]*dims[k+1]*dims[j+1]
				if cost < dp[i*n+j] {
					dp[i*n+j] = cost
				}
			}
		}
	}
	return dp[0*n+(n-1)]
}

func main() {
	numMatrices := 10
	n := numMatrices
	numDims := numMatrices + 1

	dims := make([]int, numDims)
	for k := 0; k < numDims; k++ {
		dims[k] = k%7*5 + 10
	}

	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += matrixChain(dims, n)
	}
	fmt.Println("bench_matrix_chain")
	fmt.Println(checksum)
	fmt.Println(iters)
}
