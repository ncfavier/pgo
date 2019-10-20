package main
import "fmt"

func foo(a int) (int, int) {
	return a, a+2
}
func bar(a,b int) { fmt.Print(a+b, "\n") }

func main() {
	bar(foo(20))
}
