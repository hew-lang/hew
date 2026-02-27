package main

import "fmt"

func factorial(n int) int {
	result := 1
	for i := 1; i <= n; i++ {
		result = result * i
	}
	return result
}

func main() {
	iters := 1000000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += factorial(20)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
