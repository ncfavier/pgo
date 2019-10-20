package main
import "fmt"

func step(a, b int) (int, int) {
	return b, a+b
}

func fib(n int) int {
	a, b := 0, 1
	for n > 0 {
		a, b = step(a, b)
		n--
	}
	return a
}

func main() {
	for n := 0; n <= 10; n++ {
		fmt.Print(fib(n), "\n");
	}
}
