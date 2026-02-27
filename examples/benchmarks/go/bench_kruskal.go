package main

import (
	"fmt"
	"sort"
)

func find(parent []int, x int) int {
	r := x
	for parent[r] != r {
		r = parent[r]
	}
	c := x
	for c != r {
		next := parent[c]
		parent[c] = r
		c = next
	}
	return r
}

func union(parent, rank []int, a, b int) int {
	ra := find(parent, a)
	rb := find(parent, b)
	if ra == rb {
		return 0
	}
	if rank[ra] < rank[rb] {
		parent[ra] = rb
	} else if rank[ra] > rank[rb] {
		parent[rb] = ra
	} else {
		parent[rb] = ra
		rank[ra]++
	}
	return 1
}

func kruskal(edgeSrc, edgeDst, edgeWt, sortedIdx []int, numEdges, n int) int {
	parent := make([]int, n)
	rankArr := make([]int, n)
	for i := 0; i < n; i++ {
		parent[i] = i
	}

	mstWeight := 0
	edgesAdded := 0
	for e := 0; e < numEdges; e++ {
		if edgesAdded >= n-1 {
			break
		}
		idx := sortedIdx[e]
		u := edgeSrc[idx]
		v := edgeDst[idx]
		w := edgeWt[idx]
		merged := union(parent, rankArr, u, v)
		if merged == 1 {
			mstWeight += w
			edgesAdded++
		}
	}
	return mstWeight
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

	sortedIdx := make([]int, numEdges)
	for i := 0; i < numEdges; i++ {
		sortedIdx[i] = i
	}
	sort.Slice(sortedIdx, func(i, j int) bool {
		return edgeWt[sortedIdx[i]] < edgeWt[sortedIdx[j]]
	})

	iters := 100000
	checksum := 0
	for i := 0; i < iters; i++ {
		checksum += kruskal(edgeSrc, edgeDst, edgeWt, sortedIdx, numEdges, n)
	}
	fmt.Println("bench_kruskal")
	fmt.Println(checksum)
	fmt.Println(iters)
}
