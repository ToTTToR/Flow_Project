Voici notre rendu de projet en Ocaml, par le binome Viktor Mouton et Kilian Le Nezet.

Les 3 phases du projet ont été plus ou moins abouties, il manquerait le fait de restranscrire
visuellement la phase 3 avec un problème concret, or cela a été fait à la phase 2 et cela aurait
pris beaucoup trop de temps pour le résultat.

Pour pouvoir executer le programme, il faut taper dans un terminal :
    make run in=[nom du graphe] s=[Noeud source du graphe] p=[Noeud puit]

Les graphes "graph2" et "testannale" correspondent à des graphes de flots.
Le graphe "graph5" correspond aux graphes de la phase 2 du projet, où l'on a en entrée différents
métiers, et des personnes auxquelles on voudrait leur attribuer un métier parmis ceux qu'ils souhaitent.
Les graphes "graph3" et "graph4" correspondent à des graphes de flots sous contraintes de cout.

Firefox s'ouvrira automatiquement après l'execution, montrant le graphe initial puis le graphe final.
Si Firefox ne se lance pas pour cause de système d'exploitation ou autre, les fichiers se trouveront alors
dans le dossier courant (init.svg et viz_graph.svg).

Ce projet a été intéressant à réaliser! Merci!