package main
import "fmt"

func foo(x int) *int {
	return &x
}

func main() {
	p := foo(41)
	fmt.Print(*p)
	fmt.Print("\n")
	*p = *p + 1
	fmt.Print(*p)
	fmt.Print("\n")
}

