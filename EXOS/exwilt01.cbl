      *=============================================================*
      *    Exercice 1: Calcul du nombre de pizza à commander        *
      *                Sachant que chaque convive mange 1.1 pizza   *
      *                Le nombre de convives est passé en entrée    *
      *                                                             *      
      *    Exercice 2: Application de la conjecture de Syracuse     *
      *                renvoie la suite d'opérations                *
      *                Le nombre à tester est passé en entrée       *
      *                                                             * 
      *    Exercice 3: Lire le fichier client et extraire le plus   *
      *                grand et le plus petit salaire               *
      *                                                             * 
      *    Exercice 4: Jouons aux échecs avec 2 reines              *
      *                l'échiquier présente 8 colonnes et 8 lignes  *
      *                Je choisis une position pour la dame noire   *
      *                L'utilisateur choisit la position de la dame *
      *                blanche. L'application lui dit s'il peut at- *
      *                taquer la dame noire.(diaonale, ligne ou co- *
      *                lonne)                                       *
      *                                                             *
      *    ACCESSIBLE VIA LES ENTREES SECONDAIRES                   * 
      *                                                             *
      *    auteur : Isabelle Marand                                 *
      *    Date création 09/05/2024                                 *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. exwilt01.
       AUTHOR . Isabelle Marand.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT FIC-IN ASSIGN TO 'FICHIERCLIENT.txt'   
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICIN.


           SELECT FIC-CLI ASSIGN TO 'FICCLITRI.txt'  
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICCLI.

           SELECT WORK ASSIGN TO 'FICCLIWRK.txt'.  

       DATA DIVISION.
       FILE SECTION.
       FD  FIC-IN.
       01  E-REC-IN   PIC X(100).

       FD  FIC-CLI.
       01  E-REC-CLI  PIC X(100).
       01  E-REC-CLI-DET.
           05 CLI-ID      PIC X(09)   .
           05 CLI-NOM     PIC X(20)   .
           05 CLI-PRENOM  PIC X(20)   .
           05 CLI-METIER  PIC X(14)   .
           05 CLI-DEPT     PIC X(2)   .
           05 CLI-SALAIRE  PIC 9(6)   .
           05 CLI-SALAIRE-X REDEFINES CLI-SALAIRE PIC X(6)   .
           05 CLI-FILLER   PIC X(29)  .


       SD  WORK.
       01  E-REC-WRK.
           05 WRK-ID      PIC X(09)   .
           05 WRK-NOM     PIC X(20)   .
           05 WRK-PRENOM  PIC X(20)   .
           05 WRK-METIER  PIC X(14)   .
           05 WRK-DEPT    PIC X(2)    .
           05 WRK-SALAIRE PIC 9(6)    .
           05 WRK-FILLER  PIC X(29)   .    

       WORKING-STORAGE SECTION.
      * variables pour le fichier
       01  WS-STAT-FICIN       PIC XX                      .
           88  WS-STAT-FICINOK             VALUE '00'      .
           88  WS-STAT-FICINFIN            VALUE '10'      .       
       01  WS-STAT-FICCLI       PIC XX                     .
           88  WS-STAT-FICOK             VALUE '00'        .
           88  WS-STAT-FICFIN            VALUE '10'        .

      * variables de travail     
       01  WS-NBGOURMAND     PIC 999     VALUE 0           .
       01  WS-NBPIZZA        PIC 9999V9  VALUE 0           .
       01  WS-CHARPIZZA      REDEFINES WS-NBPIZZA PIC X(6) .
       01  WS-CHAR           PIC X                         .
       01  WS-NBPIZZAZ       PIC ZZZ9    VALUE 0           .
       01  WS-DECIMAL        PIC 9V9     VALUE 0           .
       01  WS-NBPART         PIC 9V9     VALUE 0           .
       01  WS-CHIFFRE        PIC 9(5)    VALUE 0           .
       01  WS-RESTE          PIC 99      VALUE 0           .
       01  WS-PREM           PIC 9       VALUE 1           .
       01  WS-OPESYRACUSE    PIC X(250)                    .  
       01  WS-MIN            PIC 9(6)    VALUE 0           .
       01  WS-MAX            PIC 9(6)    VALUE 0           .
       01  WS-MINZ           PIC Z(5)9                     .
       01  WS-MAXZ           PIC Z(5)9                     . 
       01  WS-LIG-RAP        PIC X(100)                    .
       
       01  WS-ECHEC-TAB.
           05 WS-T-LIG OCCURS 8 TIMES.
              10 WS-T-COL   OCCURS 8 TIMES.
                 15 WS-T-POS    PIC 9    VALUE 0           .
       
       01  WS-NOM OCCURS 8 TIMES.
           05  WS-NOM-COL    PIC X                         .
       01  WS-IND            PIC 9       VALUE 0           .
       01  WS-POS-DB         PIC XX                        .
       01  WS-CHAR-POS-DB  REDEFINES WS-POS-DB             .
           05 WS-CHAR-T OCCURS 2 TIMES .
              10 WS-CHAR-POS PIC X                         .           
       01  WS-POS-DN-COL     PIC 9                         .
       01  WS-POS-DN-COL-DEC PIC 9V9999                    .
       01  WS-DN-COL         PIC 9                         .
       01  WS-POS-DN-LIG     PIC 9                         .
       01  WS-POS-DN-LIG-DEC PIC 9V9999                    .
       01  WS-DN-LIG         PIC 9                         .
       01  WS-ECHEC-VAL      PIC 9                         .
           88  WS-ECHEC-OK               VALUE 1           .
           88  WS-ECHEC-KO               VALUE 0           .

      * gestion du dialogue avec l'utilisateur
      *    pizza 
       01  WS-ACCUEIL        PIC X(50)  VALUE
           'Bonjour, Veuillez saisir le nombre de convives: ' .
       01  WS-REPONSE        PIC X(50)  VALUE
           'Le nombre de pizzas à commander est:'             . 
      *    Syracuse 
       01  WS-SYRACUSE       PIC X(50)  VALUE
           'Donnez moi un nombre entier inférieur à 100000 :' . 
       01  WS-REPSYRACUSE    PIC X(60)  VALUE  
           'Pour obtenir 1, nous avons fait les opérations suivantes:'.
       01  WS-PUIS           PIC X(5)   VALUE ' puis'         .     
      *    Fichier CLIENT 
       01  WS-LIG-MAX        PIC X(40)  VALUE
           'Le plus gros salaire trouvé est:'                 . 
       01  WS-LIG-MIN        PIC X(40)  VALUE  
           'Le plus petit salaire est le suivant:'            . 
      *    Jeu d'échec  
       01  WS-ECHEC           PIC X(120)                      .
       01  WS-ECHEC1          PIC X(58)   VALUE 
           'Donnez la position de votre reine blanche et je vous dirai'.
       01  WS-ECHEC2          PIC X(50)   VALUE
           ' si vous pouvez attaquer la reine noire: 1 lettre '.
       01  WS-ECHEC3          PIC X(9)    VALUE '1 chiffre'    .
       01  WS-ECHEC-PRECIS    PIC X(60)   VALUE 
           'Choisissez 1 chiffre entre 1 et 8 , 1 lettre entre A et H'.     
       01  WS-LIG-ECHEC-OK   PIC X(12)   VALUE 'A l''attaque!'. 
       01  WS-LIG-ECHEC-KO   PIC X(11)   VALUE 'Perdu :-(( '  .
       01  WS-LIG-DN         PIC X(35)   VALUE 
           'La reine noire était en position '                .
       01  WS-LIG-POS-DN     PIC XX                           . 
       01  WS-CHAR-LIG-DN  REDEFINES WS-LIG-POS-DN.
           05 WS-CHAR-DN-T OCCURS 2 TIMES .
              10 WS-CHAR-DN  PIC X                            . 

       PROCEDURE DIVISION .

      * 0000-MAIN-START.
       ENTRY 'nbpizzas' .
           PERFORM 1000-NBPIZZAS-START THRU 1000-NBPIZZAS-END  .
           GOBACK .

       ENTRY 'syracuse'.
           PERFORM 1010-SYRACUSE-START THRU 1010-SYRACUSE-END  .
           GOBACK.

       ENTRY 'client'.
           PERFORM 1020-CLIENT-START   THRU 1020-CLIENT-END    .
           GOBACK.

       ENTRY 'echec'.
           PERFORM 1030-ECHEC-START    THRU 1030-ECHEC-END     .
           GOBACK.

      * 0000-MAIN-END.    



        1000-NBPIZZAS-START.
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.

           DISPLAY WS-ACCUEIL WITH NO ADVANCING.
           ACCEPT WS-NBGOURMAND.
           PERFORM 2000-CALC-NBPIZZAS-START THRU 2000-CALC-NBPIZZAS-END.
           INITIALIZE WS-LIG-RAP .
           DISPLAY WS-LIG-RAP.
           DISPLAY WS-REPONSE SPACE WS-NBPIZZAZ.

           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.
       1000-NBPIZZAS-END.
           EXIT. 

       1010-SYRACUSE-START.
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.

           INITIALIZE WS-OPESYRACUSE.
           DISPLAY WS-SYRACUSE WITH NO ADVANCING.
           ACCEPT WS-CHIFFRE.
           INITIALIZE WS-LIG-RAP .
           DISPLAY WS-LIG-RAP.
           PERFORM 2010-CALC-SYRACUSE-START 
                    THRU 2010-CALC-SYRACUSE-END.
           DISPLAY WS-OPESYRACUSE.
           
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.
       1010-SYRACUSE-END.
           EXIT.
           
       1020-CLIENT-START.
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.

           INITIALIZE WS-MIN.
           INITIALIZE WS-MAX.
           PERFORM 2040-CHERCHE-MINMAX-START
                 THRU 2040-CHERCHE-MINMAX-END.
           INITIALIZE WS-LIG-RAP .
           DISPLAY WS-LIG-RAP.
           DISPLAY WS-LIG-MIN SPACE WS-MINZ .
           DISPLAY WS-LIG-MAX SPACE WS-MAXZ .
           
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.
       1020-CLIENT-END.
           EXIT.
       
       1030-ECHEC-START.
           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.

      *  Initialize la table des noms de colonne  
           MOVE 'A' TO WS-NOM-COL(1).
           MOVE 'B' TO WS-NOM-COL(2).
           MOVE 'C' TO WS-NOM-COL(3).
           MOVE 'D' TO WS-NOM-COL(4).
           MOVE 'E' TO WS-NOM-COL(5).
           MOVE 'F' TO WS-NOM-COL(6).
           MOVE 'G' TO WS-NOM-COL(7).
           MOVE 'H' TO WS-NOM-COL(8).

      *  Réceptionne la position de la dame blanche
           STRING 
              WS-ECHEC1 WS-ECHEC2 WS-ECHEC3 
           DELIMITED BY SIZE
           INTO WS-ECHEC.
           PERFORM 1040-ECHEC-SAISIE-START THRU 1040-ECHEC-SAISIE-END.

           PERFORM UNTIL ((FUNCTION NUMVAL(WS-CHAR-T (2)) < 9) 
                             AND (WS-IND < 9) )
               PERFORM 1040-ECHEC-SAISIE-START 
                 THRU 1040-ECHEC-SAISIE-END
           END-PERFORM.
           PERFORM 2050-CALC-POS-DN-START THRU 2050-CALC-POS-DN-END.
           PERFORM 2060-VALIDE-POS-START  THRU 2060-VALIDE-POS-END .
           INITIALIZE WS-LIG-RAP .
           DISPLAY WS-LIG-RAP.
           IF WS-ECHEC-OK THEN
              DISPLAY WS-LIG-ECHEC-OK
           ELSE 
              DISPLAY WS-LIG-ECHEC-KO
           END-IF. 
           DISPLAY WS-LIG-DN SPACE WS-LIG-POS-DN.

           MOVE ALL '=' TO WS-LIG-RAP.
           DISPLAY WS-LIG-RAP.   
       1030-ECHEC-END.
           EXIT .

       1040-ECHEC-SAISIE-START.
           DISPLAY WS-ECHEC .
           DISPLAY WS-ECHEC-PRECIS WITH NO ADVANCING.
           ACCEPT WS-POS-DB. 
           MOVE FUNCTION UPPER-CASE (WS-POS-DB) TO WS-POS-DB.
           PERFORM VARYING WS-IND FROM 1 BY 1
              UNTIL (WS-NOM-COL(WS-IND) = WS-CHAR-POS(1))
                    OR (WS-IND > 8)
                 INITIALIZE WS-ECHEC-VAL 
           END-PERFORM.
       1040-ECHEC-SAISIE-END.

       2000-CALC-NBPIZZAS-START.
      *    Je calcule le nombre de part à manger
           MULTIPLY WS-NBGOURMAND BY 1.1 GIVING WS-NBPIZZA .

      *    J'isole la partie décimale de WS-NBPIZZA
           MOVE FUNCTION REVERSE (WS-CHARPIZZA) TO WS-CHAR.
           DIVIDE FUNCTION NUMVAL(WS-CHAR) BY 10 GIVING WS-DECIMAL.

      *    Je calcule le nb de parts manquantes pour les ajouter
      *    au nombre de parts mangées pour le nb de pizzas à acheter      
           SUBTRACT WS-DECIMAL FROM 1 GIVING WS-NBPART.
           ADD WS-NBPART TO WS-NBPIZZA GIVING WS-NBPIZZA.
           MOVE WS-NBPIZZA TO WS-NBPIZZAZ.
       2000-CALC-NBPIZZAS-END.
           EXIT.

       2010-CALC-SYRACUSE-START. 
           PERFORM UNTIL (WS-CHIFFRE = 1) 
              DIVIDE WS-CHIFFRE BY 2 GIVING WS-NBGOURMAND
              REMAINDER WS-RESTE
              IF (WS-RESTE = 0) THEN
                 PERFORM 2020-CALC-PAIR-START 
                    THRU 2020-CALC-PAIR-END
              ELSE
                 PERFORM 2030-CALC-IMPAIR-START 
                    THRU 2030-CALC-IMPAIR-END
              END-IF
           END-PERFORM.
       2010-CALC-SYRACUSE-END.
           EXIT.

       2020-CALC-PAIR-START. 
           IF (FUNCTION LENGTH (FUNCTION TRIM(WS-OPESYRACUSE)) > 240) 
           THEN
              IF (WS-PREM = 1) THEN 
                 DISPLAY WS-REPSYRACUSE
                 SET WS-PREM TO 0
              END-IF   
              DISPLAY FUNCTION TRIM(WS-OPESYRACUSE) 
              INITIALIZE WS-OPESYRACUSE 
           END-IF.
           STRING
              FUNCTION TRIM(WS-OPESYRACUSE)
              SPACE
              WS-CHIFFRE
              '/2 ='
           DELIMITED BY SIZE
           INTO WS-OPESYRACUSE.

           DIVIDE WS-CHIFFRE BY 2 GIVING WS-CHIFFRE.

           IF (WS-CHIFFRE = 1) THEN
               STRING
                 FUNCTION TRIM(WS-OPESYRACUSE)
                 WS-CHIFFRE
              DELIMITED BY SIZE
              INTO WS-OPESYRACUSE
           ELSE
              STRING
                 FUNCTION TRIM(WS-OPESYRACUSE)
                 WS-CHIFFRE
                 WS-PUIS
              DELIMITED BY SIZE
              INTO WS-OPESYRACUSE
           END-IF.
       2020-CALC-PAIR-END.
           EXIT.

       2030-CALC-IMPAIR-START. 
           IF (FUNCTION LENGTH (FUNCTION TRIM(WS-OPESYRACUSE) ) > 220) 
           THEN
              IF (WS-PREM = 1) THEN 
                 DISPLAY WS-REPSYRACUSE
                 SET WS-PREM TO 0
              END-IF   
              DISPLAY FUNCTION TRIM(WS-OPESYRACUSE) 
              INITIALIZE WS-OPESYRACUSE 
           END-IF.
           STRING
              FUNCTION TRIM(WS-OPESYRACUSE)
              SPACE
              '('
              WS-CHIFFRE
              '*3)+1 ='
           DELIMITED BY SIZE
           INTO WS-OPESYRACUSE.

           MULTIPLY WS-CHIFFRE BY 3 GIVING WS-CHIFFRE.
           ADD 1 TO WS-CHIFFRE GIVING WS-CHIFFRE.

           IF (WS-CHIFFRE = 1) THEN
               STRING
                 FUNCTION TRIM(WS-OPESYRACUSE)
                 WS-CHIFFRE
              DELIMITED BY SIZE
              INTO WS-OPESYRACUSE
           ELSE
              STRING
                 FUNCTION TRIM(WS-OPESYRACUSE)
                 WS-CHIFFRE
                 WS-PUIS
              DELIMITED BY SIZE
              INTO WS-OPESYRACUSE
           END-IF.
       2030-CALC-IMPAIR-END.
           EXIT.

       2040-CHERCHE-MINMAX-START.

           SORT WORK ON ASCENDING KEY  WRK-SALAIRE
           USING FIC-IN GIVING FIC-CLI.

           OPEN INPUT FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.
           READ FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.
      
      * Premier enregistrement = min
           MOVE CLI-SALAIRE TO WS-MINZ.
           
           PERFORM UNTIL WS-STAT-FICFIN 
      * Dernier enregistrement = max + gestion des enregistrements vides
              IF (CLI-SALAIRE-X NOT = SPACES) THEN
                 MOVE CLI-SALAIRE TO WS-MAXZ
              END-IF            
              READ FIC-CLI
              PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END
           END-PERFORM.
           
           CLOSE FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.           
       2040-CHERCHE-MINMAX-END.
           EXIT.

       2050-CALC-POS-DN-START.
           SET WS-POS-DN-COL TO 9.
           SET WS-POS-DN-LIG TO 9.       
      *  J'utilise un random pour calculer la position de la Reine Noire
      *    Pour la colonne
           PERFORM UNTIL (WS-POS-DN-COL < 9) AND (WS-POS-DN-COL > 0)
              MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(9:8)) 
                 TO WS-POS-DN-COL-DEC
              COMPUTE WS-POS-DN-COL ROUNDED = WS-POS-DN-COL-DEC * 10
           END-PERFORM.
      *    Pour la ligne
           PERFORM UNTIL (WS-POS-DN-LIG < 9) AND (WS-POS-DN-LIG > 0)
              MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(9:8)) 
                 TO WS-POS-DN-LIG-DEC
              COMPUTE WS-POS-DN-LIG ROUNDED = WS-POS-DN-LIG-DEC * 10
           END-PERFORM.
      *    Je renseigne mon champs de sortie avec 1 lettre et 1 chiffre     
           MOVE WS-NOM-COL(WS-POS-DN-COL) TO WS-CHAR-DN(1).
           MOVE WS-POS-DN-LIG TO WS-CHAR-DN(2).

      *    Je note dans la table toutes les positions possibles 
      *    pour attaquer la Reine Noire (diagonales, ligne ,colonne).
      *    Je flague toutes les lignes de la colonne de DN
           PERFORM VARYING WS-IND FROM 1 BY 1 
                    UNTIL (WS-IND > 8)
               IF (WS-IND NOT EQUAL WS-POS-DN-LIG) THEN
                 SET WS-T-POS(WS-IND,WS-POS-DN-COL) TO 1
               END-IF 
           END-PERFORM. 
            
      *    Je flague toutes les colonnes de la ligne de DN
           PERFORM VARYING WS-IND FROM 1 BY 1 
                    UNTIL (WS-IND > 8)
               IF (WS-IND NOT EQUAL WS-POS-DN-COL) THEN
                    SET WS-T-POS(WS-POS-DN-LIG,WS-IND) TO 1
               END-IF
           END-PERFORM. 
            
      *    Je flague toutes les diagonales de la position de DN
           PERFORM VARYING WS-IND FROM 1 BY 1 
                 UNTIL (WS-IND > 8)
                    INITIALIZE WS-DN-COL WS-DN-LIG
                    SUBTRACT WS-IND FROM WS-POS-DN-COL GIVING WS-DN-COL
                    SUBTRACT WS-IND FROM WS-POS-DN-LIG GIVING WS-DN-LIG
                    IF (WS-DN-LIG > 0) AND (WS-DN-COL > 0) THEN
                       SET WS-T-POS(WS-DN-LIG,WS-DN-COL) TO 1
                    END-IF   
                    INITIALIZE WS-DN-COL WS-DN-LIG
                    SUBTRACT WS-IND FROM WS-POS-DN-COL GIVING WS-DN-COL
                    ADD WS-IND TO WS-POS-DN-LIG GIVING WS-DN-LIG
                    IF (WS-DN-LIG < 9) AND (WS-DN-COL > 0) THEN
                       SET WS-T-POS(WS-DN-LIG,WS-DN-COL) TO 1
                    END-IF 
                    INITIALIZE WS-DN-COL WS-DN-LIG
                    ADD WS-IND TO WS-POS-DN-COL GIVING WS-DN-COL
                    ADD WS-IND TO WS-POS-DN-LIG GIVING WS-DN-LIG
                    IF (WS-DN-LIG < 9) AND (WS-DN-COL < 9) THEN
                       SET WS-T-POS(WS-DN-LIG,WS-DN-COL) TO 1  
                    END-IF
                    INITIALIZE WS-DN-COL WS-DN-LIG
                    ADD WS-IND TO WS-POS-DN-COL GIVING WS-DN-COL
                    SUBTRACT WS-IND FROM WS-POS-DN-LIG GIVING WS-DN-LIG
                    IF (WS-DN-LIG > 0) AND (WS-DN-COL < 9) THEN
                       SET WS-T-POS(WS-DN-LIG,WS-DN-COL) TO 1
                    END-IF
           END-PERFORM.
       2050-CALC-POS-DN-END.

       2060-VALIDE-POS-START.
      *    Je compare la position avec celles notées attaquable
           PERFORM VARYING WS-IND FROM 1 BY 1
              UNTIL (WS-NOM-COL(WS-IND) = WS-CHAR-POS(1))
                 INITIALIZE WS-ECHEC-VAL 
           END-PERFORM.

           IF 
            (WS-T-POS(FUNCTION NUMVAL(WS-CHAR-POS(2)), WS-IND)
             = 1) THEN 
                    SET WS-ECHEC-VAL TO 1
           END-IF.
       2060-VALIDE-POS-END.
           EXIT.


       9000-TEST-STATUT-START.
           IF (NOT WS-STAT-FICOK) AND (NOT WS-STAT-FICFIN) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE WS-STAT-FICCLI 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              STOP RUN
           END-IF. 
       9000-TEST-STATUT-END.
           EXIT.  
           
