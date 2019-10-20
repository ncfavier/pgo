package main

import "fmt"
func f() (int, int) { return 41, 42 }
func main() { var x,y int = f(); fmt.Print(x+y) }
