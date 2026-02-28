package main

import "fmt"

func isPrime(n int) int {
	if n < 2 {
		return 0
	}
	for i := 2; i*i <= n; i++ {
		if n%i == 0 {
			return 0
		}
	}
	return 1
}

func main() {
	iters := 100000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += isPrime(104729)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
