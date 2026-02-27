package main

import "fmt"

func matrixMultiply(a, b []int, n int) int {
	total := n * n
	c := make([]int, total)

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			sum := 0
			for k := 0; k < n; k++ {
				sum += a[i*n+k] * b[k*n+j]
			}
			c[i*n+j] = sum
		}
	}
	return c[0] + c[total/2] + c[total-1]
}

func main() {
	n := 20
	total := n * n

	a := make([]int, total)
	b := make([]int, total)
	for k := 0; k < total; k++ {
		a[k] = k%7 + 1
		b[k] = k%5 + 1
	}

	iters := 10000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += matrixMultiply(a, b, n)
	}
	fmt.Println("bench_matrix_multiply")
	fmt.Println(checksum)
	fmt.Println(iters)
}
