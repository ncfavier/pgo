package main

import "fmt"

func main() {
	s := new(string) // chaîne vide
	fmt.Print(4, *s, 2, "\n")
	x := "Dave"
	s = &x
	fmt.Print("I'm sorry, ", *s, ". I'm afraid I can't do that.\n")
}

