package main
import "fmt"

// renvoie l'entier 00...0011..11 (n bits faibles à n)
// c'est-à-dire l'ensemble {0,1,...,n-1}
// version inefficace de ^(^0 << n)
func full(n int) int {
	if n == 0 {
		return 0;
	}
	return full(n-1) * 2 + 1;
}

// le plus petit élément de x
// version inefficace de x & -x
func lowestbit(x int) int {
	if x == 1 {
		return 1;
	}
	return 2 * lowestbit(x / 2);
}

// différence ensembliste
// version inefficace de x &^ y
func andnot(x, y int) int {
	if x == 0 {
		return 0;
	}
	z := 2 * andnot(x / 2, y / 2);
	if x % 2 == 1 && y % 2 == 0 {
		return z + 1;
	} else {
		return z;
	}
}

func t(a int, b int, c int) int {
	if a == 0 {
		return 1;
	}
	f := 0;
	e := andnot(andnot(a, b), c);
	for e > 0{
		d := lowestbit(e)
		f = f + t(a - d, (b+d)*2, (c+d)/2);
		e = e - d;
	}
	return f;
}

func queens(n int) int {
	return t(full(n), 0, 0);
}

func main() {
	for n := 0; n <= 10; n++ {
		fmt.Print(n);
		fmt.Print(" ");
		fmt.Print(queens(n));
		fmt.Print("\n")
	}
}
