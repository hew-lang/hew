package main

import "fmt"

func rleCount(s string) int {
	n := len(s)
	if n == 0 {
		return 0
	}
	runs := 0
	i := 0
	for i < n {
		ch := s[i]
		runLen := 1
		j := i + 1
		for j < n {
			if s[j] != ch {
				j = n
			} else {
				runLen++
				j++
			}
		}
		runs += runLen
		i += runLen
	}
	return runs
}

func main() {
	s := "aaabbccccdddddeee"
	iters := 500000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += rleCount(s)
	}
	fmt.Println("bench_rle")
	fmt.Println(checksum)
	fmt.Println(iters)
}
