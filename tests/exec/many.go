package main
import "fmt"

func many(a,b,c,d,e,f,g,h,i,j int) {
	fmt.Print(a)
	fmt.Print(b)
	fmt.Print(c)
	fmt.Print(d)
	fmt.Print(e)
	fmt.Print(f)
	fmt.Print(g)
	fmt.Print(h)
	fmt.Print(i)
	fmt.Print(j)
	fmt.Print("\n")
	if a < 9 {
		many(b, c, d, e, f, g, h, i, j, a)
	}
}

func main() {
	many(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
}
