package main

import "fmt"

/* Calcul de la hauteur d'un arbre en espace constant en utilisant
     Joseph M. Morris
     Traversing binary trees simply and cheaply
     Information Processing Letters 9(5), 1979
*/

type Tree struct {
	left, right *Tree
}

func make(left *Tree, right *Tree) *Tree {
	t := new(Tree)
	t.left = left;
	t.right = right;
	return t;
}

func max(x int, y int) int { if x > y { return x } else { return y } }

func height(t *Tree) int {
	if (t == nil) { return 0; }
	return 1 + max(height(t.left), height(t.right));
}

func morris(t *Tree) int {
	d, h := 0, 0;
	for t != nil {
		if t.left == nil {
			t = t.right;
			d++;
			h = max(h, d);
		} else {
			p := t.left;
			delta := 1;
			for p.right != nil && p.right != t {
				p = p.right;
				delta++;
			}
			if p.right == nil {
				p.right = t;
				t = t.left;
				d++;
				h = max(h, d);
			} else {
				p.right = nil;
				t = t.right;
				d = d - delta;
			}
		}
	}
	return h;
}

func linearl(t *Tree, n int) *Tree {
  if n == 0 { return t; }
  return linearl(make(t, nil), n - 1);
}

func path(t *Tree, n int) *Tree {
  if n == 0 { return t; }
  if n % 2 == 0 {
	  return path(make(nil, t), n - 1);
  } else {
	  return path(make(t, nil), n - 1);
  }
}

func mirror(t *Tree) *Tree {
  if t == nil { return t; }
  return make(mirror(t.right), mirror(t.left));
}

func test(t *Tree) {
	fmt.Print(height(t));
	fmt.Print(" ");
	fmt.Print(morris(t));
	fmt.Print(" ");
	fmt.Print(morris(mirror(t)));
	fmt.Print("\n");
}

func main() {
	test(nil);
	t1 := make(nil, nil);
	test(t1);
	t2 := make(t1, nil);
	test(t2);
	t3 := make(make(nil, nil), t2);
	test(t3);
	test(make(make(make(nil, make(nil, nil)),
		            make(make(nil, nil), t3)),
   		       make(nil, make(nil,make(nil,nil)))));
	test(linearl(nil, 42));
	test(path(nil, 42));
	a, b := 0, 1;
	for a < 100 {
		test(path(nil, a));
		a, b = b, a + b
	}
}
