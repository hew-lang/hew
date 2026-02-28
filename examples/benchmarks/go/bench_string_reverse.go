package main

import "fmt"

func reverseString(s string) int {
	n := len(s)
	result := make([]int, n)
	for i := 0; i < n; i++ {
		result[i] = n - 1 - i
	}
	checksum := 0
	for k := 0; k < n; k++ {
		j := result[k]
		if s[j] == s[k] {
			checksum++
		}
		checksum++
	}
	return checksum
}

func main() {
	s := "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv"
	iters := 200000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += reverseString(s)
	}
	fmt.Println("bench_string_reverse")
	fmt.Println(checksum)
	fmt.Println(iters)
}
