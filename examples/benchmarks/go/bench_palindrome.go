package main

import "fmt"

func isPalindrome(s string) bool {
	n := len(s)
	half := n / 2
	for i := 0; i < half; i++ {
		j := n - 1 - i
		if s[i] != s[j] {
			return false
		}
	}
	return true
}

func main() {
	s := "abcdefghijklmnopqrstuvwxyzzyxwvutsrqponmlkjihgfedcba"
	iters := 500000
	count := 0
	for i := 0; i < iters; i++ {
		if isPalindrome(s) {
			count++
		}
	}
	fmt.Println("bench_palindrome")
	fmt.Println(count)
	fmt.Println(iters)
}
