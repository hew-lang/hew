package main

import "fmt"

func mergeSorted(a, b []int) int {
	na := len(a)
	nb := len(b)
	result := make([]int, 0, na+nb)
	i := 0
	j := 0
	for i < na {
		if j >= nb {
			result = append(result, a[i])
			i++
		} else if a[i] <= b[j] {
			result = append(result, a[i])
			i++
		} else {
			result = append(result, b[j])
			j++
		}
	}
	for j < nb {
		result = append(result, b[j])
		j++
	}
	return result[0] + result[len(result)/2] + result[len(result)-1]
}

func main() {
	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		a := make([]int, 500)
		b := make([]int, 500)
		for k := 0; k < 500; k++ {
			a[k] = k * 2
			b[k] = k*2 + 1
		}
		checksum += mergeSorted(a, b)
	}
	fmt.Println("bench_merge_sorted")
	fmt.Println(checksum)
	fmt.Println(iters)
}
