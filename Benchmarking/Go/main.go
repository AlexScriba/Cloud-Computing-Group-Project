package main

import (
	"fmt"
	"os"
	"strconv"
)

func fibRecursive(n uint64) uint64 {
	if n <= 1 {
		return n
	}
	return fibRecursive(n-1) + fibRecursive(n-2)
}

func main() {
	if len(os.Args) < 1 {
		panic("One parameter expected")
	}
	n, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic(err)
	}
	res := fibRecursive(uint64(n))
	fmt.Println(res)
}
