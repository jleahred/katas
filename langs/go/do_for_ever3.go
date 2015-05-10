package main

import (
	"errors"
	"fmt"
	"strconv"
)

func main() {
	waitToEnd := make(chan bool)
	go protectedLoop(divisionTask, waitToEnd)
	fmt.Println("Waitting for ever")
	<-waitToEnd
}

func protectedLoop(f func() bool, waitToEnd chan bool) {
	for {
		err := protectedLoop2(f, waitToEnd)
		if err != nil {
			fmt.Println(err.Error())
		} else {
			return
		}
	}
}

func protectedLoop2(f func() bool, waitToEnd chan bool) (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New(
				fmt.Sprint("Fail in loop:> ", r))
		}
	}()

	for {
		result := f()
		if result == false {
			waitToEnd <- true
			return nil
		}
	}
}

func divisionTask() bool /*stop*/ {
	fmt.Println("Insert a number...")

	var s string
	fmt.Scan(&s)

	if s == "FIN" {
		return false
	}

	i, _ := strconv.Atoi(s)

	dangerous := 1000000 / i

	fmt.Println("after division", dangerous)

	return true
}
 
