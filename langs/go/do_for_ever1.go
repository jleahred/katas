package main

import (
	"errors"
	"fmt"
	"strconv"
)

func main() {
	protectedLoop(divisionTask)
}

func protectedLoop(f func()) {
	for {
		err := protectedLoop2(f)
		if err != nil {
			fmt.Println(err.Error())
		}
	}
}

func protectedLoop2(f func()) (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New(
				fmt.Sprint("Fail in loop:> ", r))
		}
	}()

	for {
		f()
	}
}

func divisionTask() {
	fmt.Println("Insert a number...")

	var s string
	fmt.Scan(&s)

	i, _ := strconv.Atoi(s)

	dangerous := 1000000 / i

	fmt.Println("after division", dangerous)
}
 
