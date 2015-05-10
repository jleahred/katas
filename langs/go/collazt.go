package main

import (
    "fmt"
    "time"
)

type TStats struct {
    biggestNum       int64
    biggestNumLength int
    failedCache      int64
    inCache          int64
}

var (
    catched map[int64]int = make(map[int64]int)

    stats = TStats{0, 0, 0, 0}
)

func main() {
    start := time.Now()
    catched[1] = 1

    calculateRange(1, 300)

    fmt.Printf("Biggest Collazt size of %d is %d \n", stats.biggestNum, stats.biggestNumLength)
    fmt.Printf("calulated time... %s\n", time.Now().Sub(start))
    fmt.Printf("failled cache %d\n", stats.failedCache)
    fmt.Printf("in cache %d\n", stats.inCache)
}

func calculateRange(s int64, e int64) {
    var i int64
    for i = s; i < e; i++ {
        result := calculateCollazt(i, 0)
        if result > stats.biggestNumLength {
            stats.biggestNum = i
            stats.biggestNumLength = result
        }
    }

}

func calculateCollazt(n int64, acc int) int {
    l, present := catched[n]
    if present {
        stats.inCache++
        return l + acc
    } else if n%2 == 0 {
        stats.failedCache++
        l := calculateCollazt(n/2, acc+1)
        catched[n] = l
        return l
    } else {
        stats.failedCache++
        l := calculateCollazt(3*n+1, acc+1)
        catched[n] = l
        return l
    }
}
