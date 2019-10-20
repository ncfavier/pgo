package main

import "fmt"

// le problème de Josephus

// listes circulaires doublement chaînées

type L struct {
	valeur             int
	suivant, precedent *L
}

/* liste réduite à un élément */
func make(v int) *L {
	r := new(L)
	r.valeur = v
	r.suivant = r
	r.precedent = r
	return r
}

/* insertion après un élément donnée */
func inserer_apres(l *L, v int) {
	e := make(v)
	e.suivant = l.suivant
	l.suivant = e
	e.suivant.precedent = e
	e.precedent = l
}

/* suppression d'un élément donné */
func supprimer(l *L) {
	l.precedent.suivant = l.suivant
	l.suivant.precedent = l.precedent
}

/* affichage */
func afficher(l *L) {
	p := l
	fmt.Print(p.valeur)
	p = p.suivant
	for p != l {
		fmt.Print(p.valeur)
		p = p.suivant
	}
	fmt.Print("\n")
}

/*** Partie 3 : problème de Josephus ***/

/* construction de la liste circulaire 1,2,...,n;
   l'élément renvoyé est celui contenant 1 */
func cercle(n int) *L {
	l := make(1)
	for i := n; i >= 2; i-- {
		inserer_apres(l, i)
	}
	return l
}

/* jeu de Josephus */
func josephus(n int, p int) int {
	/* c est le joueur courant, 1 au départ */
	c := cercle(n)

	/* tant qu'il reste plus d'un joueur */
	for c != c.suivant {
		/* on élimine un joueur */
		for i := 1; i < p; i++ {
			c = c.suivant
		}
		supprimer(c)
		c = c.suivant
	}
	return c.valeur
}

func main() {
	fmt.Print(josephus(7, 5)) // 6
	fmt.Print("\n")
	fmt.Print(josephus(5, 5)) // 2
	fmt.Print("\n")
	fmt.Print(josephus(5, 17)) // 4
	fmt.Print("\n")
	fmt.Print(josephus(13, 2)) // 11
	fmt.Print("\n")
}
