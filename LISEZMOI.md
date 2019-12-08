# pgo

## Instructions

Ce projet est réalisé en Haskell, et dépend (uniquement) des bibliothèques et extensions fournies par le compilateur [GHC](https://www.haskell.org/ghc/).

## Compte-rendu

### Analyse lexico-syntaxique

J'utilise la bibliothèque [parsec](https://hackage.haskell.org/package/parsec), basée sur les combinateurs de parseurs (*parser combinators*), pour l'analyse lexicale et syntaxique.

La principale difficulté de cette partie a été l'insertion automatique de point-virgules. J'ai utilisé la fonctionnalité *user state* de Parsec, qui permet de maintenir un état défini par l'utilisateur durant le parsage. L'état est une valeur booléenne traduisant la présence d'un point-virgule automatique : quand on rencontre un saut de ligne après un lexème considéré comme **final**, on met l'état à `True` pour signifier que le prochain lexème parsé doit être un point-virgule.

Il y a sans doute des façons plus élégantes de faire ça, notamment en séparant l'analyse lexicale et l'analyse syntaxique.

### Compilation

J'ai choisi de faire le typage et la production de code en une seule étape.


