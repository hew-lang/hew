package main

import "fmt"

func fibonacci(n int) int {
	a := 0
	b := 1
	for i := 0; i < n; i++ {
		temp := b
		b = a + b
		a = temp
	}
	return a
}

func main() {
	iters := 1000000
	sum := 0
	for i := 0; i < iters; i++ {
		sum += fibonacci(30)
	}
	fmt.Println(iters)
	fmt.Println(sum)
}
