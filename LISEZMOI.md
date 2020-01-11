# pgo

Ce projet est réalisé en Haskell, et dépend des bibliothèques et extensions fournies par le compilateur [GHC](https://www.haskell.org/ghc/).

## Analyse lexico-syntaxique

J'utilise la bibliothèque [parsec](https://hackage.haskell.org/package/parsec), basée sur les combinateurs de parseurs (*parser combinators*), pour l'analyse lexicale et syntaxique.

La principale difficulté de cette partie a été l'insertion automatique de point-virgules. Je maintiens une valeur booléenne pendant le parsage, indiquant la présence d'un point-virgule automatique : quand la valeur est à `True`, le seul moyen de parser quoi que ce soit est d'appeler `semicolon` directement.

Il y a sans doute des façons plus élégantes de faire ça, notamment en séparant l'analyse lexicale et l'analyse syntaxique.

## Compilation

J'ai choisi de faire le typage et la production de code en une seule passe, dans le module `Compile`.

Le module `Pack` fournit un type `Pack` permettant de représenter une zone contiguë en mémoire dans laquelle sont stockés des objets, éventuellement nommés.

La compilation globale a lieu dans la monade `Compiler`, composée des niveaux suivants :

- `Except TypeError` pour les erreurs de typage ;
- `WriterT String` pour la production de code ;
- `StateT GlobalState` pour l'état global.

La compilation des fonctions a lieu dans la monade `FunctionCompiler`, qui rajoute un niveau `StateT FunctionState` par-dessus la monade `Compiler`.

La structure de donnée utilisée pour les associations (structures, champs, fonctions, variables, etc.) est `Data.Map`, qui est implémentée avec des arbres binaires de recherche équilibrés.

La compilation débute avec `compileFile` et se déroule ainsi, dans les grandes lignes :

- on ajoute les structures dans l'environnement, en deux temps :
    - d'abord, on ajoute les champs de chaque structure, sans information de taille ; on en profite pour vérifier l'unicité des noms de structures
    - puis on construit un `Pack` pour les champs chaque structure, récursivement, en gardant une trace explicite de la pile d'appels récursifs afin de détecter les cycles
- on ajoute les fonctions dans l'environnement :
    - on vérifie l'unicité des noms
    - on vérifie que la fonction `main` a la bonne signature
    - on construit un `Pack` pour les arguments, et un pour les valeurs de retour
- on compile le point d'entrée de la librairie C
- on compile les fonctions : pour chaque fonction,
    - on ajoute les paramètres de la fonction à la portée initiale
    - on compile le corps de la fonction
    - on vérifie que toutes les branches de la fonction ont atteint une instruction `return`, le cas échéant
- on compile les littéraux de chaîne de caractères
- on vérifie qu'il y a une fonction `main`
- on vérifie que `fmt` est utilisé, s'il est importé

Le code produit fait usage des [étiquettes locales](https://sourceware.org/binutils/docs/as/Symbol-Names.html#Local-Labels-1).

La librairie C est utilisée pour l'affichage (`printf` et `putchar`) et pour l'allocation sur le tas (`sbrk`).

Les variables locales sont allouées sur la pile, sauf si elles sont utilisées comme opérande de l'opérateur `&`, auquel cas elles sont allouées sur le tas. Les variables allouées sur le tas sont représentées par un pointeur sur la pile.

Toutes les variables sont initialement mises à zéro, puisque c'est la valeur par défaut pour chacun des types de base.

Les booléens sont représentés par 0 (`false`) ou 1 (`true`).

La comparaison des chaînes de caractères ne nécessite que de comparer les adresses, puisque toute chaîne de caractères est soit la chaîne vide (représentée par 0), soit un pointeur vers un *unique* littéral associé au contenu de la chaîne dans le segment de données.
