package main

import "fmt"

func floydWarshall(n int, edgeSrc, edgeDst, edgeWt []int, numEdges int) int {
	const INF = 999999
	total := n * n
	dist := make([]int, total)
	for idx := 0; idx < total; idx++ {
		dist[idx] = INF
	}

	for i := 0; i < n; i++ {
		dist[i*n+i] = 0
	}

	for e := 0; e < numEdges; e++ {
		u := edgeSrc[e]
		v := edgeDst[e]
		w := edgeWt[e]
		if w < dist[u*n+v] {
			dist[u*n+v] = w
		}
	}

	for k := 0; k < n; k++ {
		for i := 0; i < n; i++ {
			for j := 0; j < n; j++ {
				throughK := dist[i*n+k] + dist[k*n+j]
				if throughK < dist[i*n+j] {
					dist[i*n+j] = throughK
				}
			}
		}
	}

	sum := 0
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			d := dist[i*n+j]
			if d < INF {
				sum += d
			}
		}
	}
	return sum
}

func main() {
	n := 30
	edgeSrc := make([]int, 0, 120)
	edgeDst := make([]int, 0, 120)
	edgeWt := make([]int, 0, 120)

	for src := 0; src < n; src++ {
		for dst := 1; dst <= 3; dst++ {
			neighbor := (src + dst) % n
			weight := (src+dst)%10 + 1
			edgeSrc = append(edgeSrc, src)
			edgeDst = append(edgeDst, neighbor)
			edgeWt = append(edgeWt, weight)
		}
		extra := (src*7 + 3) % n
		ew := (src*3+1)%15 + 1
		edgeSrc = append(edgeSrc, src)
		edgeDst = append(edgeDst, extra)
		edgeWt = append(edgeWt, ew)
	}
	numEdges := len(edgeSrc)

	iters := 5000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += floydWarshall(n, edgeSrc, edgeDst, edgeWt, numEdges)
	}
	fmt.Println("bench_floyd_warshall")
	fmt.Println(checksum)
	fmt.Println(iters)
}
