package main

import (
    "fmt"
    "runtime"
    "time"
)

const iterations = 3000000

type tCacheData struct {
    n      int64
    length int
}

type tFindData struct {
    n      int64
    result chan tCacheData
}

type tResult struct {
    biggestNum       int64
    biggestNumLength int
}

type tStats struct {
    cacheSize   int
    cacheFind   int64
    cacheFailed int64
    result      tResult
}

type tCacheStats struct {
    response chan tStats
}

type tCacheChannels struct {
    chFind            chan tFindData
    chAdd             chan tCacheData
    chStats           chan tCacheStats
    chTryUpdateResult chan tResult
}

var (
    cacheChannels = tCacheChannels{
        chFind:            make(chan tFindData),
        chAdd:             make(chan tCacheData),
        chStats:           make(chan tCacheStats),
        chTryUpdateResult: make(chan tResult),
    }
)

func main() {
    start := time.Now()

    runtime.GOMAXPROCS(4)

    cachServiceStart()

    calculateRange(1, iterations)

    statsResponse := make(chan tStats)
    cacheChannels.chStats <- tCacheStats{statsResponse}
    findCache := <-statsResponse

    fmt.Printf("calulated time... %s\n", time.Now().Sub(start))
    fmt.Printf("Biggest Collazt size of %d is %d \n",
        findCache.result.biggestNum,
        findCache.result.biggestNumLength)
    fmt.Printf("located in cache. %d\n", findCache.cacheFind)
    fmt.Printf("failed in cache. %d\n", findCache.cacheFailed)
}

func cachServiceStart() {
    cached := make(map[int64]int)
    stats := tStats{0, 0, 0, tResult{0, 0}}
    cached[1] = 1

    go func() {
        for {
            select {
            case find := <-cacheChannels.chFind:
                val, ok := cached[find.n] //  val == 0 if doesn't exists
                if ok {
                    stats.cacheFind++
                } else {
                    stats.cacheFailed++
                }
                find.result <- tCacheData{find.n, val}
                break

            case add := <-cacheChannels.chAdd:
                cached[add.n] = add.length
                break
            case response := <-cacheChannels.chStats:
                stats.cacheSize = len(cached)
                response.response <- stats
                break
            case result := <-cacheChannels.chTryUpdateResult:
                if result.biggestNumLength > stats.result.biggestNumLength {
                    stats.result.biggestNum = result.biggestNum
                    stats.result.biggestNumLength = result.biggestNumLength
                }
                break
            }
        }

    }()
    time.Sleep(time.Millisecond * 100)
}

func calculateRange(s int64, e int64) {
    chFindResponse := make(chan tCacheData)

    var i int64
    for i = s; i < e; i++ {
        result := calculateCollazt(i, 0, chFindResponse)
        cacheChannels.chTryUpdateResult <- tResult{i, result}
    }
}

func calculateCollazt(n int64, acc int, chFindResp chan tCacheData) int {
    cacheChannels.chFind <- tFindData{n, chFindResp}
    resp := <-chFindResp

    if resp.length != 0 {
        return resp.length + acc
    } else if n%2 == 0 {
        l := calculateCollazt(n/2, acc+1, chFindResp)
        cacheChannels.chAdd <- tCacheData{n, l}
        return l
    } else {
        l := calculateCollazt(3*n+1, acc+1, chFindResp)
        cacheChannels.chAdd <- tCacheData{n, l}
        return l
    }
}



















------------------------

package main

import (
    "fmt"
    "runtime"
    "time"
)

const (
    iterations = 3000000
    goRutines  = 1000
    nTreads    = 4
)

type tCacheData struct {
    n      int64
    length int
}

type tFindData struct {
    n      int64
    result chan tCacheData
}

type tResult struct {
    biggestNum       int64
    biggestNumLength int
}

type tStats struct {
    cacheSize   int
    cacheFind   int64
    cacheFailed int64
    result      tResult
}

type tCacheStats struct {
    response chan tStats
}

type tCacheChannels struct {
    chFind            chan tFindData
    chAdd             chan tCacheData
    chStats           chan tCacheStats
    chTryUpdateResult chan tResult
    chFinished        chan bool
}

var (
    cacheChannels = tCacheChannels{
        chFind:            make(chan tFindData),
        chAdd:             make(chan tCacheData),
        chStats:           make(chan tCacheStats),
        chTryUpdateResult: make(chan tResult),
        chFinished:        make(chan bool),
    }
)

func main() {
    start := time.Now()

    runtime.GOMAXPROCS(nTreads)

    cachServiceStart()

    var each int64 = iterations / goRutines
    var i int64
    for i = 0; i < goRutines; i++ {
        start := each*i + 1
        end := each*i + each
        go calculateRange(start, end)
    }
    for i = 0; i < goRutines; i++ {
        <-cacheChannels.chFinished
    }

    statsResponse := make(chan tStats)
    cacheChannels.chStats <- tCacheStats{statsResponse}
    findCache := <-statsResponse

    fmt.Printf("calulated time... %s\n", time.Now().Sub(start))
    fmt.Printf("Biggest Collazt size of %d is %d \n",
        findCache.result.biggestNum,
        findCache.result.biggestNumLength)
    fmt.Printf("located in cache. %d\n", findCache.cacheFind)
    fmt.Printf("failed in cache. %d\n", findCache.cacheFailed)
}

func cachServiceStart() {
    cached := make(map[int64]int)
    stats := tStats{0, 0, 0, tResult{0, 0}}
    cached[1] = 1

    go func() {
        for {
            select {
            case find := <-cacheChannels.chFind:
                val, ok := cached[find.n] //  val == 0 if doesn't exists
                if ok {
                    stats.cacheFind++
                } else {
                    stats.cacheFailed++
                }
                find.result <- tCacheData{find.n, val}
                break

            case add := <-cacheChannels.chAdd:
                cached[add.n] = add.length
                break
            case response := <-cacheChannels.chStats:
                stats.cacheSize = len(cached)
                response.response <- stats
                break
            case result := <-cacheChannels.chTryUpdateResult:
                if result.biggestNumLength > stats.result.biggestNumLength {
                    stats.result.biggestNum = result.biggestNum
                    stats.result.biggestNumLength = result.biggestNumLength
                }
                break
            }
        }

    }()
    time.Sleep(time.Millisecond * 100)
}

func calculateRange(s int64, e int64) {
    chFindResponse := make(chan tCacheData)

    var i int64
    for i = s; i < e; i++ {
        result := calculateCollazt(i, 0, chFindResponse)
        cacheChannels.chTryUpdateResult <- tResult{i, result}
    }
    cacheChannels.chFinished <- true
}

func calculateCollazt(n int64, acc int, chFindResp chan tCacheData) int {
    cacheChannels.chFind <- tFindData{n, chFindResp}
    resp := <-chFindResp

    if resp.length != 0 {
        return resp.length + acc
    } else if n%2 == 0 {
        l := calculateCollazt(n/2, acc+1, chFindResp)
        cacheChannels.chAdd <- tCacheData{n, l}
        return l
    } else {
        l := calculateCollazt(3*n+1, acc+1, chFindResp)
        cacheChannels.chAdd <- tCacheData{n, l}
        return l
    }
}







 
