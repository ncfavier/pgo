package main
import "fmt"

func step(a, b int) (int, int) {
	return b, a+b
}

func main() {
	_, x := step(step(step(0, 1)));
	fmt.Print(x, "\n");
}
