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
      *                taquer la dame noire.(diagonale, ligne ou co-*
      *                lonne)                                       *
      *                                                             *
      *    [IM] - MAJV2: Ajout des chiffres romains                 *
      *    Exercice 5: Les chiffres romains                         *
      *                L'utilisateur saisit un nombre entier compris*
      *                entre 1 et 3999. L'application traduit ce    *
      *                nombre en chiffres romains.                  *
      *                                                             * 
      *    [IM] - MAJV2: gestion des échanges avec l'utilisateur via*
      *          des screen sections    le 02/07/2024               *
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

      ******************************************************************
       DATA DIVISION.
      ****************************************************************** 
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

      ******************************************************************
       WORKING-STORAGE SECTION.
      * variables pour le fichier
       01  WS-STAT-FICIN       PIC XX                      .
           88  WS-STAT-FICINOK             VALUE '00'      .
           88  WS-STAT-FICINFIN            VALUE '10'      .       
       01  WS-STAT-FICCLI       PIC XX                     .
           88  WS-STAT-FICOK             VALUE '00'        .
           88  WS-STAT-FICFIN            VALUE '10'        .
  
       01  SC-RETURN         PIC X       VALUE SPACE       . 

      *    pizzas        
       01  WS-NBGOURMAND     PIC 999     VALUE 0           .
       01  WS-NBPIZZA        PIC 9999V9  VALUE 0           .
       01  WS-CHARPIZZA      REDEFINES WS-NBPIZZA PIC X(6) .
       01  WS-CHAR           PIC X                         .
       01  WS-NBPIZZAZ       PIC ZZZ9    VALUE 0           .
       01  WS-DECIMAL        PIC 9V9     VALUE 0           .
       01  WS-NBPART         PIC 9V9     VALUE 0           .
      
      *    Syracuse 
       01  WS-CHIFFRE        PIC 9(5)    VALUE 0           .
       01  WS-CHIFFRE-2    REDEFINES WS-CHIFFRE.
           05 WS-CHIFFRE-1C  PIC 9.
           05 WS-CHIFFRE-TAB OCCURS 4 TIMES.
              10 WS-CHIFFRE-T   PIC 9.
           
           05 WS-CHIFFRE-4C  PIC 9(04).
       01  WS-RESTE          PIC 99      VALUE 0           .
       01  WS-PREM           PIC 9       VALUE 1           .
       01  WS-OPESYRACUSE    PIC X(250)                    .  
       01  WS-REPSYRACUSE    PIC X(60)   VALUE  
           'Pour obtenir 1, nous avons fait les operations suivantes:'.
       01  WS-PUIS           PIC X(5)    VALUE ' puis'     .
       01  SC-SYRACUSE       PIC X(60)   VALUE SPACES      .
       01  SC-OPESYRACUSE1   PIC X(250)  VALUE SPACES      .
       01  SC-OPESYRACUSE2   PIC X(250)  VALUE SPACES      .
       01  SC-OPESYRACUSE3   PIC X(250)  VALUE SPACES      .
       01  SC-OPESYRACUSE4   PIC X(250)  VALUE SPACES      .
       01  SC-OPESYRACUSE5   PIC X(250)  VALUE SPACES      .
       01  WS-NB-WRITE       PIC 9       VALUE ZERO        .

      *    Fichier client 
       01  WS-MIN            PIC 9(6)    VALUE 0           .
       01  WS-MAX            PIC 9(6)    VALUE 0           .
       01  WS-MINZ           PIC Z(5)9                     .
       01  WS-MAXZ           PIC Z(5)9                     . 

      *    nom ?? 
       01  WS-NOM OCCURS 8 TIMES.
           05  WS-NOM-COL    PIC X                         .
       

      *    Echec 
       01  WS-ECHEC-TAB.
           05 WS-T-LIG OCCURS 8 TIMES.
              10 WS-T-COL   OCCURS 8 TIMES.
                 15 WS-T-POS    PIC 9    VALUE 0           .      
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
       01  WS-LIG-ECHEC-OK   PIC X(15)   VALUE 'A l''attaque!'. 
       01  WS-LIG-ECHEC-KO   PIC X(15)   VALUE 'Perdu :-(( '  .
       01  WS-LIG-DN         PIC X(35)   VALUE 
           'La reine noire etait en position '             .
       01  WS-LIG-POS-DN     PIC XX                        . 
       01  WS-CHAR-LIG-DN  REDEFINES WS-LIG-POS-DN.
           05 WS-CHAR-DN-T OCCURS 2 TIMES .
              10 WS-CHAR-DN  PIC X                         . 
       01  SC-LIG-ECHEC      PIC X(15)   VALUE SPACES      .
       01  SC-LIG-DN         PIC X(40)   VALUE SPACES      .

      *    [IM]MAJV2: Conversion d'un nombre en chiffres romains
       01  WS-IND            PIC 9       VALUE 0           .
       01  WS-CHIFFREZ       PIC Z(4)9                     .
       01  WS-CHIFFRE-ROMAIN PIC X(15)   VALUE SPACES      .
       01  WS-ROMAIN-CHIFFRE REDEFINES   WS-CHIFFRE-ROMAIN .
           05 WS-ROMAIN-TAB  OCCURS   15  TIMES            .
              10 WS-ROMAIN-T    PIC X                      .
       01  WS-IND-ROMAIN     PIC 9(02)   VALUE ZERO        .
       01  WS-IND-ROM-T      PIC 9(02)   VALUE ZERO        .
       01  SC-ROMAIN         PIC X(250)  VALUE SPACES      .
       01  WS-ROMAIN         PIC X(28)  
           VALUE 'donne en chiffres romains: ' .

       SCREEN SECTION.
           COPY './copy/screen-pizza.cpy'      .
           COPY './copy/screen-syracuse.cpy'   .
           COPY './copy/screen-file.cpy'       .
           COPY './copy/screen-echec.cpy'      .
           COPY './copy/screen-romain.cpy'     .

      ******************************************************************
       PROCEDURE DIVISION .
       0000-MAIN-START.

      ******************************************************************
       ENTRY 'nbpizzas' .
           PERFORM 1000-NBPIZZAS-START THRU END-1000-NBPIZZAS  .
           GOBACK .
      ******************************************************************
       ENTRY 'syracuse'.
           PERFORM 1010-SYRACUSE-START THRU END-1010-SYRACUSE  .
           GOBACK.
      ******************************************************************
       ENTRY 'client'.
           PERFORM 1020-FILE-START   THRU END-1020-FILE    .
           GOBACK.
      ******************************************************************
       ENTRY 'echec'.
           PERFORM 1030-ECHEC-START    THRU END-1030-ECHEC     .
           GOBACK.
      ******************************************************************
      *    [IM] MAJV2: ajout des chiffres romains
       ENTRY 'romain'.
           PERFORM 1040-ROMAIN-START    THRU END-1040-ROMAIN   .
           GOBACK.
      ******************************************************************

       0000-MAIN-END.    

      ******************************************************************
        1000-NBPIZZAS-START.
           PERFORM 8000-INITIALIZE-ALL-START 
                             THRU END-8000-INITIALIZE-ALL.
           PERFORM UNTIL (FUNCTION UPPER-CASE(SC-RETURN) = 'X')
              PERFORM 8000-INITIALIZE-ALL-START 
                             THRU END-8000-INITIALIZE-ALL
              ACCEPT SCR-PIZZAS
              PERFORM 2000-CALC-NBPIZZAS-START 
                                THRU END-2000-CALC-NBPIZZAS
           END-PERFORM.
       END-1000-NBPIZZAS.
           EXIT. 

       1010-SYRACUSE-START.
           PERFORM 8000-INITIALIZE-ALL-START 
                             THRU END-8000-INITIALIZE-ALL. 
           PERFORM UNTIL (FUNCTION UPPER-CASE(SC-RETURN) = 'X')
      *         PERFORM 8000-INITIALIZE-ALL-START 
      *                       THRU END-8000-INITIALIZE-ALL         
              ACCEPT SCR-SYRACUSE
              PERFORM 2010-CALC-SYRACUSE-START 
                                THRU END-2010-CALC-SYRACUSE
              ADD 1 TO WS-NB-WRITE
              EVALUATE (WS-NB-WRITE)
                 WHEN 1
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE1
                 WHEN 2 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE2                 
                 WHEN 3 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE3                 
                 WHEN 4 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE4                 
                 WHEN 5 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE5                 
              END-EVALUATE
           END-PERFORM.
       END-1010-SYRACUSE.
           EXIT.
           
       1020-FILE-START.
           PERFORM 8000-INITIALIZE-ALL-START 
                    THRU END-8000-INITIALIZE-ALL.
           PERFORM 2040-CHERCHE-MINMAX-START
                 THRU END-2040-CHERCHE-MINMAX.
           ACCEPT SCR-FILE.
       END-1020-FILE.
           EXIT.
       
       1030-ECHEC-START.
      *  Initialize la table des noms de colonne  
           MOVE 'A' TO WS-NOM-COL(1).
           MOVE 'B' TO WS-NOM-COL(2).
           MOVE 'C' TO WS-NOM-COL(3).
           MOVE 'D' TO WS-NOM-COL(4).
           MOVE 'E' TO WS-NOM-COL(5).
           MOVE 'F' TO WS-NOM-COL(6).
           MOVE 'G' TO WS-NOM-COL(7).
           MOVE 'H' TO WS-NOM-COL(8).

           PERFORM 8000-INITIALIZE-ALL-START 
                             THRU END-8000-INITIALIZE-ALL.
           PERFORM UNTIL (FUNCTION UPPER-CASE (SC-RETURN) = 'X')
      *        PERFORM 8000-INITIALIZE-ALL-START 
      *                    THRU END-8000-INITIALIZE-ALL
              ACCEPT SCR-ECHEC
              PERFORM 1100-ECHEC-SAISIE-START THRU END-1100-ECHEC-SAISIE
              PERFORM UNTIL ((FUNCTION NUMVAL(WS-CHAR-T (2)) < 9) 
                             AND (WS-IND < 9) )
                 PERFORM 1100-ECHEC-SAISIE-START 
                             THRU END-1100-ECHEC-SAISIE
              END-PERFORM
              PERFORM 2050-CALC-POS-DN-START THRU END-2050-CALC-POS-DN
              PERFORM 2060-VALIDE-POS-START  THRU END-2060-VALIDE-POS 

              IF WS-ECHEC-OK THEN
                 MOVE WS-LIG-ECHEC-OK TO SC-LIG-ECHEC
              ELSE 
                 MOVE WS-LIG-ECHEC-KO TO SC-LIG-ECHEC
              END-IF 
              STRING WS-LIG-DN SPACE WS-LIG-POS-DN
              DELIMITED BY SIZE INTO SC-LIG-DN
           END-PERFORM.
       END-1030-ECHEC.
           EXIT.

      *    [IM] MAJV2: Ajout des chiffres romains
       1040-ROMAIN-START.
           PERFORM 8000-INITIALIZE-ALL-START 
                    THRU END-8000-INITIALIZE-ALL.
           PERFORM UNTIL (FUNCTION UPPER-CASE (SC-RETURN) = 'X')
              INITIALIZE WS-CHIFFRE-ROMAIN WS-CHIFFRE WS-IND-ROMAIN
              ACCEPT SCR-ROMAIN
              IF (WS-CHIFFRE > 0) AND (WS-CHIFFRE < 4000) THEN
                 MOVE WS-CHIFFRE TO WS-CHIFFREZ
                 PERFORM VARYING WS-IND FROM 1 BY 1 
                       UNTIL WS-IND > 4
                    IF (WS-IND = 1) THEN
                       IF WS-CHIFFRE-T(WS-IND) > 0 THEN                          
                           PERFORM 1110-BCL-ROMAIN-START
                                   THRU END-1110-BCL-ROMAIN
                       END-IF                       
                    ELSE
                       If WS-CHIFFRE-T(WS-IND) = 9 THEN
                          ADD 1 TO WS-IND-ROMAIN GIVING WS-IND-ROMAIN
                          EVALUATE (WS-IND)
                             WHEN 4 
                                MOVE 'I' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                MOVE 'X' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                             WHEN 3 
                                MOVE 'X' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                MOVE 'C' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                             WHEN 2 
                                MOVE 'C' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                MOVE 'M' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                          END-EVALUATE   
                       ELSE                 
                          If WS-CHIFFRE-T(WS-IND) > 4 THEN 
                             ADD 1 TO WS-IND-ROMAIN GIVING WS-IND-ROMAIN                            
                             EVALUATE (WS-IND)
                                WHEN 4 
                                   MOVE 'V' 
                                      TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                   PERFORM 1115-5-ROMAIN-START 
                                      THRU END-1115-5-ROMAIN
                                WHEN 3 
                                   MOVE 'L' 
                                      TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                   PERFORM 1115-5-ROMAIN-START 
                                      THRU END-1115-5-ROMAIN 
                                WHEN 2 
                                   MOVE 'D' 
                                      TO WS-ROMAIN-T(WS-IND-ROMAIN)           
                                   PERFORM 1115-5-ROMAIN-START 
                                      THRU END-1115-5-ROMAIN   
                             END-EVALUATE                         
                          ELSE
                             If WS-CHIFFRE-T(WS-IND) = 4 THEN
                                ADD 1 TO WS-IND-ROMAIN 
                                            GIVING WS-IND-ROMAIN
                                EVALUATE (WS-IND)
                                   WHEN 4
                                      MOVE 'I' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                      ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                      MOVE 'V' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                   WHEN 3 
                                      MOVE 'X' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                      ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                      MOVE 'L' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                   WHEN 2 
                                      MOVE 'C' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)
                                      ADD 1 TO WS-IND-ROMAIN 
                                         GIVING WS-IND-ROMAIN
                                      MOVE 'D' 
                                         TO WS-ROMAIN-T(WS-IND-ROMAIN)                                      
                                END-EVALUATE                           
                             ELSE
                                PERFORM 1110-BCL-ROMAIN-START
                                         THRU END-1110-BCL-ROMAIN                               
                             END-IF           
                          END-IF
                       END-IF
                    END-IF               
                 END-PERFORM
                 STRING WS-CHIFFREZ SPACE WS-ROMAIN WS-CHIFFRE-ROMAIN
                 DELIMITED BY SIZE
                 INTO SC-ROMAIN
              END-IF
           END-PERFORM.           
       END-1040-ROMAIN.
           EXIT.

       1100-ECHEC-SAISIE-START.
           MOVE FUNCTION UPPER-CASE (WS-POS-DB) TO WS-POS-DB.
           PERFORM VARYING WS-IND FROM 1 BY 1
              UNTIL (WS-NOM-COL(WS-IND) = WS-CHAR-POS(1))
                    OR (WS-IND > 8)
                 INITIALIZE WS-ECHEC-VAL 
           END-PERFORM.
       END-1100-ECHEC-SAISIE.
           EXIT.

       1110-BCL-ROMAIN-START.
           PERFORM VARYING WS-IND-ROM-T FROM 1 BY 1
                  UNTIL (WS-IND-ROM-T > WS-CHIFFRE-T(WS-IND))
              ADD 1 TO WS-IND-ROMAIN  
              EVALUATE (WS-IND)
                 WHEN 4 
                    MOVE 'I' TO WS-ROMAIN-T(WS-IND-ROMAIN)                                         
                 WHEN 3 
                    MOVE 'X' TO WS-ROMAIN-T(WS-IND-ROMAIN) 
                 WHEN 2
                    MOVE 'C' TO WS-ROMAIN-T(WS-IND-ROMAIN)
                 WHEN 1
                    MOVE 'M' TO WS-ROMAIN-T(WS-IND-ROMAIN)                      
              END-EVALUATE                               
           END-PERFORM.          
       END-1110-BCL-ROMAIN.
           EXIT.                        

       1115-5-ROMAIN-START .
           SUBTRACT 5 FROM WS-CHIFFRE-T(WS-IND) 
                             GIVING WS-CHIFFRE-T(WS-IND).                                         
           PERFORM 1110-BCL-ROMAIN-START THRU END-1110-BCL-ROMAIN.                                         
       END-1115-5-ROMAIN.
           EXIT.                              

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
       END-2000-CALC-NBPIZZAS.
           EXIT.

       2010-CALC-SYRACUSE-START. 
           PERFORM UNTIL (WS-CHIFFRE = 1) 
              DIVIDE WS-CHIFFRE BY 2 GIVING WS-NBGOURMAND
              REMAINDER WS-RESTE
              IF (WS-RESTE = 0) THEN
                 PERFORM 2020-CALC-PAIR-START 
                    THRU END-2020-CALC-PAIR
              ELSE
                 PERFORM 2030-CALC-IMPAIR-START 
                    THRU END-2030-CALC-IMPAIR
              END-IF
           END-PERFORM.
       END-2010-CALC-SYRACUSE.
           EXIT.

       2020-CALC-PAIR-START. 
           IF (FUNCTION LENGTH (FUNCTION TRIM(WS-OPESYRACUSE)) > 240) 
           THEN
              IF (WS-PREM = 1) THEN 
                 MOVE WS-REPSYRACUSE TO SC-SYRACUSE
                 SET WS-PREM TO 0
              END-IF
              ADD 1 TO WS-NB-WRITE GIVING WS-NB-WRITE 
              EVALUATE (WS-NB-WRITE)
                 WHEN 1
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE1
                 WHEN 2 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE2                 
                 WHEN 3 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE3                 
                 WHEN 4 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE4                 
                 WHEN 5 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE5                 
              END-EVALUATE                    
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
       END-2020-CALC-PAIR.
           EXIT.

       2030-CALC-IMPAIR-START. 
           IF (FUNCTION LENGTH (FUNCTION TRIM(WS-OPESYRACUSE) ) > 240) 
           THEN
              IF (WS-PREM = 1) THEN 
                 MOVE WS-REPSYRACUSE TO SC-SYRACUSE
                 SET WS-PREM TO 0
              END-IF   
              ADD 1 TO WS-NB-WRITE GIVING WS-NB-WRITE 
              EVALUATE (WS-NB-WRITE)
                 WHEN 1
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE1
                 WHEN 2 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE2                 
                 WHEN 3 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE3                 
                 WHEN 4 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE4                 
                 WHEN 5 
                    MOVE FUNCTION TRIM(WS-OPESYRACUSE) 
                                   TO SC-OPESYRACUSE5                 
              END-EVALUATE  
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
       END-2030-CALC-IMPAIR.
           EXIT.

       2040-CHERCHE-MINMAX-START.
      * Je trie le fichier en ascending sur le champ salaire
           SORT WORK ON ASCENDING KEY  WRK-SALAIRE
           USING FIC-IN GIVING FIC-CLI.

           OPEN INPUT FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU END-9000-TEST-STATUT.
           READ FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU END-9000-TEST-STATUT.
      
      * Premier enregistrement = min
           MOVE CLI-SALAIRE TO WS-MINZ.
           
           PERFORM UNTIL WS-STAT-FICFIN 
      * Dernier enregistrement = max + gestion des enregistrements vides
              IF (CLI-SALAIRE-X NOT = SPACES) THEN
                 MOVE CLI-SALAIRE TO WS-MAXZ
              END-IF            
              READ FIC-CLI
              PERFORM 9000-TEST-STATUT-START THRU END-9000-TEST-STATUT
           END-PERFORM.
           
           CLOSE FIC-CLI.
           PERFORM 9000-TEST-STATUT-START THRU END-9000-TEST-STATUT.           
       END-2040-CHERCHE-MINMAX.
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
       END-2050-CALC-POS-DN.

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
       END-2060-VALIDE-POS.
           EXIT.

       8000-INITIALIZE-ALL-START. 
           INITIALIZE  SC-RETURN WS-POS-DB SC-LIG-ECHEC SC-LIG-DN
                       WS-LIG-POS-DN WS-OPESYRACUSE WS-MAX WS-MIN
                       WS-CHIFFRE-ROMAIN SC-ROMAIN WS-CHIFFRE.
       END-8000-INITIALIZE-ALL.
           EXIT.

      ******************************************************************
      *                      GESTION DES ERREURS                       *
      ******************************************************************
       9000-TEST-STATUT-START.
           IF (NOT WS-STAT-FICOK) AND (NOT WS-STAT-FICFIN) THEN 
 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE WS-STAT-FICCLI 
 
              STOP RUN
           END-IF. 
       END-9000-TEST-STATUT.
           EXIT.  
           
