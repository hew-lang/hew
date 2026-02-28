package main

import "fmt"

func caesarChecksum(s string, lower string, table []int) int {
	n := len(s)
	checksum := 0
	for i := 0; i < n; i++ {
		found := -1
		for p := 0; p < 26; p++ {
			if s[i] == lower[p] {
				found = p
				break
			}
		}
		if found >= 0 {
			checksum += table[found]
		}
	}
	return checksum
}

func main() {
	s := "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv"
	lower := "abcdefghijklmnopqrstuvwxyz"

	table := make([]int, 26)
	for k := 0; k < 26; k++ {
		shifted := (k + 13) % 26
		table[k] = shifted + 97
	}

	iters := 200000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += caesarChecksum(s, lower, table)
	}
	fmt.Println("bench_caesar_cipher")
	fmt.Println(checksum)
	fmt.Println(iters)
}
