package main

import "fmt"

func gcd(a, b int) int {
	x := a
	y := b
	for y != 0 {
		temp := y
		y = x % temp
		x = temp
	}
	return x
}

func lcm(a, b int) int {
	return a / gcd(a, b) * b
}

func main() {
	iters := 5000000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += lcm(1071, 462)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
