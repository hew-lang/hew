package main

import "fmt"

func pascal(n int) int {
	row := []int{1}
	for i := 0; i < n; i++ {
		row = append(row, 0)
		for j := len(row) - 1; j > 0; j-- {
			row[j] = row[j] + row[j-1]
		}
	}
	return row[n/2]
}

func main() {
	iters := 100000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += pascal(20)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
