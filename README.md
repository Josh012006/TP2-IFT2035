# TP 2– Ajout du typage statique au langage Psil en Haskell

## Description
Ce projet consiste à compléter l'interpréteur pour le langage **Psil** défini au niveau du [TP1](https://github.com/Josh012006/TP1-IFT2035), écrit en Haskell, afin d'y ajouter une dynamique de **typage statique**. L’objectif principal est de :
1. Corriger le type `Lexp` pour ajouter les modifications de syntaxe;
2. Ajouter les types algébriques à (`s2l`)  et la syntaxe du typage statique;
3. Compléter la fonction d'évaluation `eval` pour gérer l'évaluation des Lexp;
4. Implémenter la fonction de vérification de typage correct (`check`) ;
5. Fournir au moins six tests Psil dans `tests.psil` pour valider l'implémentation ;
6. Rédiger un rapport (`rapport.tex`) décrivant votre démarche (problèmes rencontrés, choix effectués, etc.).

Le langage Psil supporte :
- Les entiers signés et les variables ;
- Les appels de fonctions (forme curried) ;
- Les abstractions et définitions locales (`abs`, `def`) ;
- Les opérations arithmétiques prédéfinies (`+`, `–`, `*`, `/`, …) ;
- Les constructeurs et le filtrage sur données (pattern matching) ;
- Les expressions conditionnelles sous forme de `filter` (équivalent de `if`);
- La définition de types algébriques (`adt`).
- Le typage statique et la vérification de types.

## Structure du projet
```
├── psil.hs            # Code source Haskell fourni ; à compléter (s2l et eval)
├── tests.psil         # Fichier de tests Psil : contient ≥ 5 exemples commentés
├── rapport.tex        # Rapport (LaTeX) décrivant l’expérience et les choix
└── README.md          # Ce fichier
```

## Compilation et exécution
```bash
    ghci> :load "psil.hs"
    ghci> run "tests.psil"
```

## Auteurs

- **Josué Mongan**

GitHub : [Josh012006](https://github.com/Josh012006)

- **David Stanescu**

GitHub : [DavidStanescu13](https://github.com/DavidStanescu13)