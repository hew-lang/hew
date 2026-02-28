package main

import "fmt"

func buildDAG(n int) ([][]int, []int) {
	adj := make([][]int, n)
	inDegree := make([]int, n)

	for src := 0; src < n; src++ {
		adj[src] = make([]int, 0, 4)
		for dst := 1; dst <= 3; dst++ {
			neighbor := src + dst
			if neighbor < n {
				adj[src] = append(adj[src], neighbor)
			}
		}
		extra := (src*3 + 7) % n
		if extra > src {
			adj[src] = append(adj[src], extra)
		}
	}

	for src := 0; src < n; src++ {
		for _, neighbor := range adj[src] {
			inDegree[neighbor]++
		}
	}
	return adj, inDegree
}

func topologicalSort(adj [][]int, inDegOrig []int, n int) int {
	inDeg := make([]int, n)
	copy(inDeg, inDegOrig)

	queue := make([]int, 0, n)
	for i := 0; i < n; i++ {
		if inDeg[i] == 0 {
			queue = append(queue, i)
		}
	}

	head := 0
	count := 0
	orderSum := 0

	for head < len(queue) {
		node := queue[head]
		head++
		orderSum += node
		count++

		for _, neighbor := range adj[node] {
			inDeg[neighbor]--
			if inDeg[neighbor] == 0 {
				queue = append(queue, neighbor)
			}
		}
	}
	return count*1000 + orderSum%1000
}

func main() {
	n := 50
	adj, inDegree := buildDAG(n)

	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += topologicalSort(adj, inDegree, n)
	}
	fmt.Println("bench_topological_sort")
	fmt.Println(checksum)
	fmt.Println(iters)
}
