package main

import "fmt"

func foo(x int) *int { return &x }

func main() {
	x := foo(40)
	y := foo(41)
	fmt.Print(*x, "\n")
	fmt.Print(*y, "\n")
	*x = 42
	*y = 43
	fmt.Print(*x, "\n")
	fmt.Print(*y, "\n")
}

