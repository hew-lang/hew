package main

import "fmt"

func newtonSqrt(n int) int {
	if n == 0 {
		return 0
	}
	x := n
	y := (x + 1) / 2
	for y < x {
		x = y
		y = (x + n/x) / 2
	}
	return x
}

func main() {
	iters := 5000000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += newtonSqrt(1000000)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
