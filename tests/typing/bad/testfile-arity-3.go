package main

func f() int { return 42 }
func g() {}
func main() { f(g()) }
