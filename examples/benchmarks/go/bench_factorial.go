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
	// factorial(20) is ~2.4e18; summing it iters times overflows int almost
	// immediately, so fold each result into the checksum modulo a large prime.
	// The Hew twin traps on that overflow instead of wrapping, so both sides
	// use the same well-defined checksum.
	sum := 0
	for i := 0; i < iters; i++ {
		sum = (sum + factorial(20)) % 1000000007
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
