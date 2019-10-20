package main
import "fmt"

type T struct {
}
 /* */
func foo(x,y int, z T) {
}
func bar(x int) *int {
	return &x
}
func gee(x int) (int, int) {
	return x, x+1
}
func main() {
	var x,x1 int
	var y,y1 int = 0, 1
	var z,z1 = gee(1)
	a, b := gee(42)
	s := x + x1 + y + y1 + z + z1 + a + b
	fmt.Print(s)
	t := new(T)
	t, a = nil, 2
	fmt.Print(t)
	for ; 2<1;  {}
}
/*
a */
