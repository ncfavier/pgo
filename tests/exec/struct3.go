package main
import "fmt"

type T struct { a,b int }

func main() {
	var x = 41
	var t T
	t.a = 0
	t.b = 1
	var y = 42
	fmt.Print(t.a, "\n");
	fmt.Print(t.b, "\n");
	x = x + 1
	y = y + 1
	fmt.Print(t.a, "\n");
	fmt.Print(t.b, "\n");
}
