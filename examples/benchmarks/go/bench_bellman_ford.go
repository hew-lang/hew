package main

import "fmt"

func bellmanFord(edgeSrc, edgeDst, edgeWt []int, numEdges, n, startNode int) int {
	const INF = 999999
	dist := make([]int, n)
	for i := 0; i < n; i++ {
		dist[i] = INF
	}
	dist[startNode] = 0

	for iter := 0; iter < n-1; iter++ {
		for e := 0; e < numEdges; e++ {
			u := edgeSrc[e]
			v := edgeDst[e]
			w := edgeWt[e]
			nd := dist[u] + w
			if dist[u] < INF {
				if nd < dist[v] {
					dist[v] = nd
				}
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
	edgeSrc := make([]int, 0, 150)
	edgeDst := make([]int, 0, 150)
	edgeWt := make([]int, 0, 150)

	for src := 0; src < n; src++ {
		for dst := 1; dst <= 3; dst++ {
			neighbor := (src + dst) % n
			weight := (src+dst)%10 + 1
			edgeSrc = append(edgeSrc, src)
			edgeDst = append(edgeDst, neighbor)
			edgeWt = append(edgeWt, weight)
		}
	}
	numEdges := len(edgeSrc)

	iters := 20000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += bellmanFord(edgeSrc, edgeDst, edgeWt, numEdges, n, 0)
	}
	fmt.Println("bench_bellman_ford")
	fmt.Println(checksum)
	fmt.Println(iters)
}
