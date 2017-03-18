package main

import (
	"fmt"
	"runtime"
	"time"
)

func main() {
	runtime.GOMAXPROCS(2)
	c := make(chan int)
	go p("", c)
	go p2("a", c)
	go p2(".", c)
	a := <-c
	b := <-c
	<-c

	fmt.Println(a)
	fmt.Println(b)
}

func p(a string, c chan int) {
	r := 0
	for i := 0; i < 500; i++ {
		// fmt.Print(a)
		// time.Sleep(10 * time.Millisecond)
		for j := 0; j < 10000; j++ {
			for j := 0; j < 5000; j++ {
				// runtime.Gosched()
			}
		}
		r += i
	}
	c <- r
}

func p2(a string, c chan int) {
	for i := 0; i < 500; i++ {
		fmt.Print(a)
		time.Sleep(10 * time.Millisecond)
	}
	c <- 0
}
