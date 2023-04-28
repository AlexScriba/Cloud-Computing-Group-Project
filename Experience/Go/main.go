package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"sync"
)

func min2(x, y int) int {
  if x < y {
    return x
  }
  return y
}

func min3(x, y, z int) int {
	if x < y {
		if x < z {
			return x
		}
		return z
	}
	if y < z {
		return y
	}
	return z
}

func levenshteinDistance(s, t string) int {
	m, n := len(s), len(t)

	if m == 0 {
		return n
	}

	if n == 0 {
		return m
	}

	d := make([][]int, m+1)
	for i := range d {
		d[i] = make([]int, n+1)
	}

	for i := 0; i <= m; i++ {
		d[i][0] = i
	}

	for j := 0; j <= n; j++ {
		d[0][j] = j
	}

	for j := 1; j <= n; j++ {
		for i := 1; i <= m; i++ {
			if s[i-1] == t[j-1] {
				d[i][j] = d[i-1][j-1]
			} else {
				d[i][j] = 1 + min3(d[i-1][j], d[i][j-1], d[i-1][j-1])
			}
		}
	}

	return d[m][n]
}

func compareDistances(words []string, start, end int, results chan<- [3]int) {
  fmt.Printf("Thread dispatched for %d-%d\n", start, end)

  for i := start; i < end; i++ {
    for j := i + 1; j < len(words); j++ {
      distance := levenshteinDistance(words[i], words[j])
      results <- [3]int{i, j, distance}
    }
  }

  fmt.Printf("Thread finished for %d-%d\n", start, end)
}

func main() {
  args := os.Args

  if len(args) < 3 {
    fmt.Println("Usage: <file_path> <num_threads>")
    os.Exit(1)
  }

  filePath := args[1]
  numThreads, err := strconv.Atoi(args[2])
  if err != nil || numThreads < 1 {
    fmt.Println("Invalid number of threads")
    os.Exit(1)
  }

  file, err := os.Open(filePath)
  if err != nil {
    fmt.Printf("Unable to open file: %sn", err)
    os.Exit(1)
  }
  defer file.Close()

  reader := bufio.NewReader(file)
  var words []string
  for {
    word, err := reader.ReadString('\n')
    if err == io.EOF {
      break
    }
    words = append(words, strings.TrimSpace(word))
  }

  results := make(chan [3]int)
  var wg sync.WaitGroup

  chunkSize := (len(words) + numThreads - 1) / numThreads
  for i := 0; i < numThreads; i++ {
    start := i * chunkSize
    end := min2((i+1)*chunkSize, len(words))
    wg.Add(1)
    go func(start, end int) {
      defer wg.Done()
      compareDistances(words, start, end, results)
    }(start, end)
  }

  go func() {
    wg.Wait()
    close(results)
  }()

  minDistance := int(^uint(0) >> 1)
  var minIndeces [2]int

  for result := range results {
    i, j, distance := result[0], result[1], result[2]
    if distance < minDistance {
      minDistance = distance
      minIndeces = [2]int{i, j}
    }
  }

  fmt.Printf(
    "Most similar strings are: \n\t%s\n\t%s\nWith distance of %d\n",
    words[minIndeces[0]],
    words[minIndeces[1]],
    minDistance,
    )
}

