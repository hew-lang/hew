package main

import "fmt"

func hammingDistance(a, b string) int {
	n := len(a)
	dist := 0
	for i := 0; i < n; i++ {
		if a[i] != b[i] {
			dist++
		}
	}
	return dist
}

func main() {
	a := "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv"
	b := "aXcdeXghijXlmnopXrstuvXxyzaXcdeXghijXlmnopXrstuvXxyzaXcdeXghijXlmnopXrstuvXxyzaXcdeXghijXlmnopXrstuX"
	iters := 500000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += hammingDistance(a, b)
	}
	fmt.Println("bench_hamming")
	fmt.Println(checksum)
	fmt.Println(iters)
}
