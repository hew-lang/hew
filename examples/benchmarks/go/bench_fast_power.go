package main

import "fmt"

func fastPower(base, exp int) int {
	result := 1
	b := base
	e := exp
	for e > 0 {
		if e%2 == 1 {
			result = result * b
		}
		b = b * b
		e = e / 2
	}
	return result
}

func main() {
	iters := 5000000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += fastPower(2, 30)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
