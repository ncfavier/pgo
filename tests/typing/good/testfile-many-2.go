package main

import "fmt"
func foo(x int) (int, int) { return x, x+1 }
func main() {
	x, y := foo(41)
	fmt.Print(x+y)
}
