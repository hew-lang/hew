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

func bfs(adj [][]int, n, startNode int) int {
	visited := make([]bool, n)
	queue := make([]int, 0, n)

	visited[startNode] = true
	queue = append(queue, startNode)
	head := 0
	count := 0

	for head < len(queue) {
		node := queue[head]
		head++
		count++

		for _, neighbor := range adj[node] {
			if !visited[neighbor] {
				visited[neighbor] = true
				queue = append(queue, neighbor)
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
		checksum += bfs(adj, n, 0)
	}
	fmt.Println("bench_bfs")
	fmt.Println(checksum)
	fmt.Println(iters)
}
