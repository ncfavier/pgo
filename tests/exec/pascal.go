package main
import "fmt"

/* triangle de Pascal modulo 7 */

type List struct { head int; next *List; };

func get(l *List, i int) int {
	if i == 0 { return l.head; }
	return get(l.next, i-1);
}

func set(l *List, i, v int) {
	if i == 0 { l.head = v; return }
	set(l.next, i-1, v);
}

func create(n int) *List {
	if n == 0 { return nil }
	r := new(List)
	r.head = 0;
	r.next = create(n-1);
	return r;
}

func print_row(r *List, i int) {
	for j := 0; j <= i; j++ {
		if get(r, j) != 0 {
			fmt.Print("*");
		} else {
			fmt.Print(".");
		}
	}
	fmt.Print("\n");
}

func compute_row(r *List, i int) {
	for j := i; j > 0; j-- {
		set(r, j, (get(r,j) + get(r,j-1)) % 7);
	}
	set(r, 0, 1);
}

func pascal(n int) {
	r := create(n + 1);
	for i:= 0; i < n; i++ {
		set(r, i, 0);
		compute_row(r, i);
		print_row(r, i);
	}
}

func main() {
	pascal(42);
}
