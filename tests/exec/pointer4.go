package main
import "fmt"

func foo() *int { x := 41; return &x }

func main() {
	x := foo()
	y := foo()
	fmt.Print(*x, "\n")
	fmt.Print(*y, "\n")
	*x = 42
	*y = 43
	fmt.Print(*x, "\n")
	fmt.Print(*y, "\n")
}
