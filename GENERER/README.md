GENERATEUR DE PROGRAMME

Introduction :
Dans le cadre d’un projet il est important de fixer une règle de développement commune à tous les développeurs participant au projet. Ceci facilitera la recette et la rétroingéniering.
En utilisant un générateur de programme, la forme des programmes sera imposée.

Description :
Nous avons le parti pris de ne faire qu’un programme en sortie du générateur et non pas une copy pour chaque partie du programme.
Pour atteindre notre objectif et donner la possibilité de personnaliser le résultat final, nous avons découpé en morceaux la structure d’un programme en différentes parties.
Nous avons également distingué la génération d’un programme sans accès SQL d’un programme travaillant avec une base de données. En effet, pour ce dernier, le fonctionnement du compilateur ne permet l’utilisation de copy. Le programme utilisant sql n’en contient donc pas.
Le programme principal génère le squelette du programme sans SQL. 
Il appelle un sous-programme dans le cas d’une génération d’un squelette avec accès à une base de données.
Pour chacun des programmes de génération, nous avons utilisé un maximum de copy afin de ne laisser dans le corps du programme que les paragraphes de personnalisation et les déclarations.
Les fichiers contenant le sectionnement du programme initial se trouvent dans un répertoire copy et ont pour extension txt. 
Les 2 programmes et les copy utilisées par ceux-ci sont dans la racine de l’environnement. 
 

Programme sans SQL :
La génération de programme sans SQL est assurée par generprg.cbl.
Description de l’environnement :
Répertoire/sous-répertoire	Non du fichier	Description
Racine	Output.cbl	Livrable : programme généré

Racine	generprg.cbl	Programme génération

Racine	PRG-3000.cpy	Copy contenant les paragraphes numérotés 3000. Ces paragraphes font ouverture/lecture/fermeture d’un fichier txt sans personnalisation et écriture du fichier de sortie.

Racine	FILE-IO.cpy	ENVIRONMENT DIVISION de generprg.cbl

Racine	FILE-SECT.cpy	FILE SECTION de generprg.cbl

Racine	FILE-STAT.cpy	Déclaration des statuts pour tous les fichiers txt utilisés par generprg.cbl

Racine	FILE-GEST.cpy	Paragraphes 2000 à 2030 correspondant aux ouvertures, écriture, lectures et fermetures de tous les fichiers txt utilisés par generprg.cbl

Racine	FILEI-GEST.cpy	Livrable : copy utilisée par le futur programme (output.cbl) qui gère les ouvertures, lectures, fermetures des fichiers en entrée du programme. Une copy par fichier.

Racine	FILEO-GEST.cpy	Livrable : copy utilisée par le futur programme (output.cbl) qui gère les ouvertures, écritures, fermetures des fichiers en sortie du programme. Une copy par fichier.

Racine	runIM	Exécutable de l’application de génération

Racine	SCREEN-GEST.cpy	Copy contenant les paragraphes 4000 du programme generprg.cbl.
Ces paragraphes gèrent l’affichage des écrans décrits dans la screen section.

Racine	TST-STATUT.cpy	Livrable : copy utilisée par le futur programme (output.cbl) qui gère le contrôle du statut pour chaque fichier utilisé dans le programme.
Ce même copy est utilisé pour generprg.cbl également.

Racine/Copy	CALL-SSPRG.txt	Section contenant l’appel à un sous-programme. 
Personnalisable avec 
•	nom du sous-programme
•	nom du paramètre à transmettre (0, 1 ou plusieurs)
•	Type de transmission (reference ou content)
•	Paramètre attendu en retour (0 ou 1)
•	Nom du paramètre en retour

Racine/Copy	 DATA-DIV.txt	 Section contenant le libellé de la data division.

Racine/Copy	 ENV-DIV.txt	 Section contenant le libellé de l’environnement division avec la configuration secteur en commentaire.

Racine/Copy	 FILE-CONT.txt	 Section contenant le libellé de la file control section.

Racine/Copy	 FILEF-SECT.txt	 Section contenant le descriptif d’un fichier de la file section avec enregistrement de longueur fixe.
Personnalisable avec 
•	nom du fichier
•	longueur de l’enregistrement

Racine/Copy	 FILE-IO.txt	 Section contenant le descriptif d’un fichier de la file section avec enregistrement de longueur fixe.
Personnalisable avec 
•	nom du fichier

Racine/Copy	 FILE-SECT.txt	 Section contenant le libellé de la file section

Racine/Copy	 FILE-STATUS.txt	 Section contenant la déclaration des statuts dans la working-storage section.
Personnalisable avec 
•	nom du fichier

Racine/Copy	 FILEF-SECT.txt	 Section contenant le descriptif d’un fichier de la file section avec enregistrement de longueur variable.
Personnalisable avec 
•	nom du fichier
•	longueur de l’enregistrement

Racine/Copy	 GEST-FILI.txt	 Section contenant la ligne du copy de FILEI-GEST .
Personnalisable avec 
•	nom du fichier

Racine/Copy	 GEST-FILO.txt	 Section contenant la ligne du copy de FILEO-GEST .
Personnalisable avec 
•	nom du fichier

Racine/Copy	IDENT-DIV.txt	Section contenant le bloc de commentaire pour décrire les fonctionnalités du programme et l’identification division.
Personnalisable avec 
•	nom du programme
•	nom de l’auteur
•	la date de création (date système géré par generprg.cbl)

Racine/Copy	IO-SECT.txt	Section contenant le libellé de l’input output section

Racine/Copy	PROC-DIV.txt	Section contenant la procédure division. Le programme vient insérer  l’appel à sous-programme si paramétré.

Racine/Copy	TST-STAT.txt	Section contenant la ligne du copy de TST-STATUT.
Personnalisable avec 
•	nom du fichier

Racine/Copy	WS-SECTION.txt	Section contenant le libellé du working-storage section ainsi que la variable utilisée pour renvoyer le message d’erreur en cours de manipulation de fichier (cf :TST-STATUT.cpy)

Programme avec SQL :
La génération de programme avec SQL est assurée par genersql.cbl.
Ce programme est appelé par generprg.cbl si l’utilisateur a indiqué qu’il travaillerait avec une base de données.
L’environnement de generprg.cbl est donc complété par les éléments ci-dessous.

Description de l’environnement :
Répertoire/sous-répertoire	Non du fichier	Description
Racine	Outputsql.cbl	Livrable : programme généré

Racine	generprg.cbl	Programme génération appelant

Racine	genersql.cbl	Programme génération appelé 

Racine	FILEI-GEST.cpy	copy utilisée pour écrire les ouvertures, lectures, fermetures des fichiers en entrée du programme généré.
Personnalisable avec 
•	nom du fichier

Racine	FILEO-GEST.cpy	copy utilisée pour écrire les ouvertures, écritures, fermetures des fichiers en sortie du programme généré.
Personnalisable avec 
•	nom du fichier

Racine	runIM	Exécutable de l’application de génération

Racine	TST-STATUT.cpy	copy utilisée pour écrire le contrôle du statut pour chaque fichier dans le programme généré.
Ce même copy est utilisé pour genersql.cbl également.
Personnalisable avec 
•	nom du fichier

Racine	SQFILE-GEST.cpy	Paragraphes 2000 à 2030 de genersql.cbl

Racine	SQFILE-IO.cpy	ENVIRONMENT DIVISION de genersql.cbl 

Racine	SQFILE-SECT.cpy	FILE SECTION de genersql.cbl

Racine	SQFILE-STAT.cpy	Déclaration des statuts de fichier de genersql.cbl

Racine/SQLCopy	 SQL-DECLARE.txt	 Section contenant la déclaration SQL dans la working-storage section.
Personnalisable avec 
•	nom de la base de données
•	nom de l’utilisateur de connexion 
•	mot de passe de connexion

Racine/SQLCopy	 SQL-STATUT.txt	 Section contenant le paragraphe de gestion d’erreur SQL.

Racine/SQLCopy	 SQLPROC-DIV.txt	Section contenant la procédure division. Le programme vient insérer  l’appel à sous-programme si paramétré.
Cette section contient aussi les blocs sql exec pour :
•	connexion à la base
•	déclaration de curseur
•	ouverture de curseur
•	fetch
•	fermeture de curseur
•	déconnexion de la base




