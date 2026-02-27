package main

import "fmt"

func buildWeightedGraph(n int) ([][]int, [][]int) {
	adj := make([][]int, n)
	weights := make([][]int, n)

	for src := 0; src < n; src++ {
		adj[src] = make([]int, 0, 6)
		weights[src] = make([]int, 0, 6)
		for dst := 1; dst <= 5; dst++ {
			neighbor := (src + dst) % n
			weight := (src+dst)%10 + 1
			adj[src] = append(adj[src], neighbor)
			weights[src] = append(weights[src], weight)
		}
		extra := (src*7 + 3) % n
		ew := (src*3+1)%15 + 1
		adj[src] = append(adj[src], extra)
		weights[src] = append(weights[src], ew)
	}
	return adj, weights
}

func dijkstra(adj, wts [][]int, n, startNode int) int {
	const INF = 999999
	dist := make([]int, n)
	used := make([]bool, n)
	for i := 0; i < n; i++ {
		dist[i] = INF
	}
	dist[startNode] = 0

	for iter := 0; iter < n; iter++ {
		u := -1
		bestDist := INF
		for v := 0; v < n; v++ {
			if !used[v] {
				if dist[v] < bestDist {
					bestDist = dist[v]
					u = v
				}
			}
		}

		if u < 0 {
			break
		}
		used[u] = true

		for e := 0; e < len(adj[u]); e++ {
			neighbor := adj[u][e]
			w := wts[u][e]
			newDist := dist[u] + w
			if newDist < dist[neighbor] {
				dist[neighbor] = newDist
			}
		}
	}

	total := 0
	for i := 0; i < n; i++ {
		total += dist[i]
	}
	return total
}

func main() {
	n := 50
	adj, wts := buildWeightedGraph(n)

	iters := 50000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += dijkstra(adj, wts, n, 0)
	}
	fmt.Println("bench_dijkstra")
	fmt.Println(checksum)
	fmt.Println(iters)
}
