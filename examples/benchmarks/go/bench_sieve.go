package main

import "fmt"

func sieve(n int) int {
	flags := make([]int, n+1)
	for i := 0; i <= n; i++ {
		flags[i] = 1
	}
	flags[0] = 0
	flags[1] = 0
	for p := 2; p*p <= n; p++ {
		if flags[p] == 1 {
			for j := p * p; j <= n; j += p {
				flags[j] = 0
			}
		}
	}
	count := 0
	for k := 0; k <= n; k++ {
		count += flags[k]
	}
	return count
}

func main() {
	iters := 10000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += sieve(10000)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
