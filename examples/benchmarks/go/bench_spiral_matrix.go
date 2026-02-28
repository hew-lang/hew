package main

import "fmt"

func spiralMatrix(n int) int {
	total := n * n
	mat := make([]int, total)

	top := 0
	bottom := n - 1
	left := 0
	right := n - 1
	num := 1

	for top <= bottom && left <= right {
		for col := left; col <= right; col++ {
			mat[top*n+col] = num
			num++
		}
		top++

		for row := top; row <= bottom; row++ {
			mat[row*n+right] = num
			num++
		}
		right--

		if top <= bottom {
			for col := right; col >= left; col-- {
				mat[bottom*n+col] = num
				num++
			}
			bottom--
		}

		if left <= right {
			for row := bottom; row >= top; row-- {
				mat[row*n+left] = num
				num++
			}
			left++
		}
	}

	return mat[0] + mat[total/2] + mat[total-1]
}

func main() {
	n := 20
	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += spiralMatrix(n)
	}
	fmt.Println("bench_spiral_matrix")
	fmt.Println(checksum)
	fmt.Println(iters)
}
