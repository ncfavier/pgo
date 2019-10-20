package main
import "fmt"
type List struct { x int; next *List }
func main() {
	var l *List = nil;
	fmt.Print(l.x)
}
