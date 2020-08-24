# Projet OPEN MAPPING

## Présentation

Dans le cadre de ce projet, un ensemble d'algorithmesa été implémenté permettant de manipuler des données cartographiques, et de fournir un service web pour afficher les résultats. Les données initiales que nous manipulons sont au format OSM (OpenStreetMap), dérivé du language \textit{xml}. L'objectif final est d'en extraire un format de graphe permettant différents calculs notamment de distance puis de réaliser un server web affichant la map et différents services.

## Auteurs

Ce projet a été réalisé:
- Ducluseau Marc
- Pavia Pierre
- Ouédraogo Sonia
- Trocherie Lucas

## Tests

Taper 'make test' à la racine du projet

## Utilisation

Lancer le projet avec la commande `racket src/server.rkt maps/<my_map>.osm` à la racine de celui-ci pour ouvrir une carte `my_map`. Puis, taper dans un navigateur :
- http://localhost:9000 : affiche la carte courante avec un conteneur svg.
- http://localhost:9000/route?start=<id>&end=<id> : affiche l'itinéraire entre les deux points spécifiés par les ids start et end si ils sont dans la même composante connexe, et la phrase Disconnected Universe Error sinon.
- http://localhost:9000/distance?start=<id>&end=<id> : affiche la distance entre les deux points spécifiés par les ids start et end si ils sont dans la même composante connexe, et la phrase Disconnected Universe Error sinon.
- (non implémenté) http://localhost:9000/cycle?nodes=<id>,...,<id> : affiche un cycle de longueur minimale passant par tous les points dont les ids sont spécifiés dans la liste nodes.

Lancer les tests avec la commande `racket test/all-tests.rkt`.
