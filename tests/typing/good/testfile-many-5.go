package main

import "fmt"
func f() (int,int) { return 41, 42 }
func g() (int,int) { return f() }
func main() { fmt.Print(g()) }
