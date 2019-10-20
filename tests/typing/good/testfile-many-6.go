package main

import "fmt"
func f() (int, int) { return 41, 42 }
func main() { x,y := f(); fmt.Print(x+y) }
