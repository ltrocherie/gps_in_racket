#####################################################
PROJECT : OPEN MAPPING

#####################################################
AUTHORS:
D. Marc
P. Pierre
O. Sonia
T. Lucas

#####################################################
TESTS:
Type 'make test' on the root directory

#####################################################
HOWTO:
Lancer le projet avec la commande `racket src/server.rkt maps/forrest.osm` dans le répertoire du projet. Puis, taper dans le navigateur :
- http://localhost:9000 : affiche la carte courante avec un conteneur svg.
- http://localhost:9000/route?start=<id>&end=<id> : affiche l'itinéraire entre les deux points spécifiés par les ids start et end si ils sont dans la même composante connexe, et la phrase Disconnected Universe Error sinon.
- http://localhost:9000/distance?start=<id>&end=<id> : affiche la distance entre les deux points spécifiés par les ids start et end si ils sont dans la même composante connexe, et la phrase Disconnected Universe Error sinon.
- (non implémenté) http://localhost:9000/cycle?nodes=<id>,...,<id> : affiche un cycle de longueur minimale passant par tous les points dont les ids sont spécifiés dans la liste nodes.

Lancer les tests avec la commande `racket test/all-tests.rkt`.
