package main

import "fmt"

/* arithmétique de virgule fixe
   precision q = 65536 i.e. 16 bits pour la partie décimale */

func add(x int, y int) int {
	return x + y
}
func sub(x int, y int) int {
	return x - y
}
func mul(x int, y int) int {
	t := x * y
	return (t + 65536/2) / 65536
}
func div(x int, y int) int {
	t := x * 65536
	return (t + y/2) / y
}
func of_int(x int) int {
	return x * 65536
}

func iter(n int, a int, b int, xn int, yn int) int {
	if n == 100 {
		return 1
	}
	xn2 := mul(xn, xn)
	yn2 := mul(yn, yn)
	if add(xn2, yn2) > of_int(4) {
		return 0
	}
	return iter(n+1, a, b, add(sub(xn2, yn2), a),
		add(mul(of_int(2), mul(xn, yn)), b))
}

func inside(x int, y int) bool {
	return iter(0, x, y, of_int(0), of_int(0)) > 0
}

func run(steps int) int {
	xmin := of_int(-2)
	xmax := of_int(1)
	deltax := div(sub(xmax, xmin), of_int(2*steps))
	ymin := of_int(-1)
	ymax := of_int(1)
	deltay := div(sub(ymax, ymin), of_int(steps))
	for i := 0; i < steps; i++ {
		y := add(ymin, mul(of_int(i), deltay))
		for j := 0; j < 2*steps; j++ {
			x := add(xmin, mul(of_int(j), deltax))
			if inside(x, y) {
				fmt.Print("0")
			} else {
				fmt.Print("1")
			}
		}
		fmt.Print("\n")
	}
	return 0
}

func main() {
	run(30)
}
