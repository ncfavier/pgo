# pgo

## Instructions

Ce projet est réalisé en Haskell, et dépend des bibliothèques et extensions fournies par GHC.

## Compte-rendu

### Parse.hs

J'ai choisi d'utiliser la bibliothèque [Parsec](https://hackage.haskell.org/package/parsec) pour l'analyse lexicale et syntaxique. Elle est basée sur les combinateurs de parseurs (*parser combinators*), qui sont une approche plutôt élégante à mon goût.

J'avais déjà utilisé le module ReadP de la bibliothèque standard de Haskell, qui est basé sur le même principe. Parsec est plus flexible et plus efficace, au coût d'être plus difficile à manier, notamment à cause de l'absence de rebroussement (*backtracking*) implicite ; il faut le demander explicitement avec `try`.

La principale difficulté de cette partie était l'insertion de point-virgules automatique. J'ai utilisé la fonctionnalité *user state* de Parsec, qui permet de maintenir un état défini par l'utilisateur durant le parsage. L'idée est d'utiliser une valeur booléenne signifiant la présence d'un point-virgule automatique : quand on rencontre un saut de ligne après un token considéré comme **final**, on met l'état à `True` pour signifier que le prochain token lu doit être un point-virgule.

Le combinateur important du fichier est `lexeme` :

`lexeme final p` parse un lexème (token) correspondant au parseur `p` :
- si l'état est à `True` (c'est-à-dire qu'on a atteint un point virgule automatique), on échoue systématiquement ; le seul moyen de parser quoi que ce soit dans cet état est d'appeler `semicolon` directement (voir plus bas)
- on parse `p`
- on ignore le `whitespace` qui suit, en récupérant le numéro de ligne avant et après
- si on a changé de ligne et que l'argument `final` est à `True`, on met l'état du parseur à `True`
- enfin, on renvoie le résultat de `p`

Les autres parseurs du module utilisent `lexeme` plus ou moins directement, à l'exception de `semicolon`, qui vérifie d'abord si l'état est à `True`, et renvoie directement un point-virgule le cas échéant, sans rien parser.
