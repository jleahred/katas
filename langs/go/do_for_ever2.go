package main

import (
	"errors"
	"fmt"
	"strconv"
)

func main() {
	protectedLoop(loop)
}

func protectedLoop(_loop func() error) {
	for {
		err := _loop()
		if err != nil {
			fmt.Println(err.Error())
		}
	}
}

func loop() (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New(
				fmt.Sprint("Fail in loop:> ", r))
		}
	}()

	for {
		fmt.Println("Insert a number...")

		var s string
		fmt.Scan(&s)

		i, _ := strconv.Atoi(s)

		dangerous := div1(i)

		fmt.Println("after division", dangerous)
	}
}

func div1(i int) int {
	return div2(i)
}

func div2(i int) int {
	return 1000000 / i
}
 
