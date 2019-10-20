package main
import "fmt"

func foo(a int) (int, int) {
	return a, a+2
}
func bar(x, y int) (int, int) {
	return foo(x+y)
}

func main() {
	a, b := bar(10, 10)
	fmt.Print(a+b, "\n")
}
