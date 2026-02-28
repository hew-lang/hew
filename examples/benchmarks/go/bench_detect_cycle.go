package main

import "fmt"

func buildDirectedGraph(n int) [][]int {
	adj := make([][]int, n)
	for src := 0; src < n; src++ {
		adj[src] = make([]int, 0, 4)
		for dst := 1; dst <= 3; dst++ {
			neighbor := (src + dst) % n
			adj[src] = append(adj[src], neighbor)
		}
		extra := (src*7 + 13) % n
		adj[src] = append(adj[src], extra)
	}
	return adj
}

func detectCycle(adj [][]int, n int) int {
	color := make([]int, n)
	type frame struct {
		node  int
		phase int
	}
	stack := make([]frame, 0, n)

	hasCycle := 0
	for start := 0; start < n; start++ {
		if color[start] == 0 {
			stack = append(stack, frame{start, 0})

			for len(stack) > 0 {
				top := len(stack) - 1
				node := stack[top].node
				phase := stack[top].phase

				if phase == 0 {
					color[node] = 1
					stack[top].phase = 1
					for _, neighbor := range adj[node] {
						if color[neighbor] == 1 {
							hasCycle = 1
						}
						if color[neighbor] == 0 {
							stack = append(stack, frame{neighbor, 0})
						}
					}
				} else {
					color[node] = 2
					stack = stack[:top]
				}
			}
		}
	}
	return hasCycle
}

func main() {
	n := 50
	adj := buildDirectedGraph(n)

	iters := 200000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += detectCycle(adj, n)
	}
	fmt.Println("bench_detect_cycle")
	fmt.Println(checksum)
	fmt.Println(iters)
}
