package main
import "fmt"

/* valeurs par défaut :
	 int    0
	 bool   false
	 string ""
	 *tau   nil
*/

type T struct { x int; y bool; z *int }

func main() {
	var b bool
	var i int
	var s string
	var t T
	var p *T
    var q = new(T)
	fmt.Print(b, "\n")
	fmt.Print(i, "\n")
	fmt.Print(s, "\n")
	fmt.Print(t.x, "\n")
	fmt.Print(t.y, "\n")
	fmt.Print(t.z, "\n")
	fmt.Print(p, "\n")
	fmt.Print(q.x, "\n")
	fmt.Print(q.y, "\n")
	fmt.Print(q.z, "\n")
}
