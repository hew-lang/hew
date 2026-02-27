package main

import "fmt"

func buildGraph(n int) [][]int {
	adj := make([][]int, n)
	for src := 0; src < n; src++ {
		adj[src] = make([]int, 0, 6)
		for dst := 1; dst <= 5; dst++ {
			neighbor := (src + dst) % n
			adj[src] = append(adj[src], neighbor)
		}
		extra1 := (src*7 + 3) % n
		adj[src] = append(adj[src], extra1)
	}
	return adj
}

func dfs(adj [][]int, n, startNode int) int {
	visited := make([]bool, n)
	stack := make([]int, 0, n)

	stack = append(stack, startNode)
	count := 0

	for len(stack) > 0 {
		node := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if visited[node] {
			continue
		}
		visited[node] = true
		count++

		for _, neighbor := range adj[node] {
			if !visited[neighbor] {
				stack = append(stack, neighbor)
			}
		}
	}
	return count
}

func main() {
	n := 50
	adj := buildGraph(n)

	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += dfs(adj, n, 0)
	}
	fmt.Println("bench_dfs")
	fmt.Println(checksum)
	fmt.Println(iters)
}
