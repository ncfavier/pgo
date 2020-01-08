# pgo

Ce projet est réalisé en Haskell, et dépend (uniquement) des bibliothèques et extensions fournies par le compilateur [GHC](https://www.haskell.org/ghc/).

### Analyse lexico-syntaxique

J'utilise la bibliothèque [parsec](https://hackage.haskell.org/package/parsec), basée sur les combinateurs de parseurs (*parser combinators*), pour l'analyse lexicale et syntaxique.

La principale difficulté de cette partie a été l'insertion automatique de point-virgules. Je maintiens une valeur booléenne pendant le parsage, indiquant la présence d'un point-virgule automatique : quand la valeur est à `True`, le seul moyen de parser quoi que ce soit est d'appeler `semicolon` directement.

Il y a sans doute des façons plus élégantes de faire ça, notamment en séparant l'analyse lexicale et l'analyse syntaxique.

### Compilation

J'ai choisi de faire le typage et la production de code en une seule étape, dans le module `Compile`.

Le module `Pack` fournit un type `Pack` permettant de représenter une zone contiguë en mémoire dans laquelle sont stockés des objets, éventuellement nommés.

La compilation globale a lieu dans la monade `Compiler`, composée des niveaux suivants :

- `Except TypeError` pour lever des erreurs de typage,
- `WriterT String` pour la production de code,
- `StateT GlobalState` pour l'état global.

La compilation des fonctions a lieu dans la monade `FunctionCompiler`, qui rajoute un niveau `StateT FunctionState` par-dessus la monade `Compiler`.

Aucune optimisation n'est recherchée.
