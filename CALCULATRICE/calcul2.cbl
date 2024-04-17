      *=============================================================*
      * CALCULATRICE:                                               *
      *    L'objectif: Concevoir un programme permettant d'imiter le*
      *    fonctionnement d'une calculatrice.                       *
      *    Les étapes:                                              *        
      *    Je demande à l'utilisateur de saisir l'opération qu'il   *
      *    veut faire, puis les chiffres qu'il veut mettre en oeuvre*
      *    je teste les données reçues et boucle sur la saisie si ko*
      *    Je calcule le résultat                                   *
      *    J'affiche l'opération et le résultat                     *
      *    auteur : Isabelle Marand                                 *
      *    Date création 09/04/2024                                 *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcul2.
       AUTHOR. Isabelle Marand.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SIGN PIC X(1).
       01  WS-NUM1 PIC 9(3) VALUE 0.
       01  WS-NUM1-CHAR PIC X(3).
       01  WS-NUM1-AFF  REDEFINES WS-NUM1-CHAR PIC Z(3).
       01  WS-CODE  PIC X(3) .
       88  STOPPER VALUE 'X'.
       01  WS-DETAIL PIC X VALUE 'O'.
       01  WS-BON-OPE PIC 9 VALUE 1.
       01  WS-NUM2 PIC 9(3) VALUE 0.
       01  WS-NUM2-CHAR PIC X(3).
       01  WS-NUM2-AFF  REDEFINES WS-NUM2-CHAR PIC Z(3).
       01  WS-AFF-RESULT PIC Z(9)9 VALUE 0.
       01  WS-RESULT PIC 9(10) VALUE 0.
       01  WS-AFF-ERR1 PIC X(50) VALUE 
           'Impossible de soustraire par un nombre plus grand'.
       01  WS-AFF-ERR2 PIC X(46) VALUE 
           'Impossible de diviser par un nombre plus grand'.
       01  WS-AFF-ERR3 PIC X(30) VALUE 'Impossible de diviser par zero'.
       01  WS-AFF-STOP PIC X(38) VALUE 
           'Pour arrêter de jouer saisissez X'.
       01  WS-AFF-BONJOUR PIC X(50) VALUE           
           'Bonjour, saisissez la première série à calculer'.
       01  WS-AFF-DETAIL PIC X(50) VALUE 
           'Voulez-vous afficher l''opération, Oui O, Non N'.
       01  WS-AFF-SAI-SIGN PIC X(60) VALUE           
           'Saisissez le signe de l''opération + ou - ou * ou / ou P'.   

      **************************************************************
      * Exécution du programme                                      
      **************************************************************
       PROCEDURE DIVISION.
       
      * 0000-MAIN-START.

      * DIs bonjour et récupère le premier chiffre
           PERFORM DIS-BONJOUR.
           PERFORM UNTIL STOPPER
              PERFORM SAI-WS-SIGN THRU SAI-WS-NUM2

      * Selon le type d'opération demandée, on applique le calcul 
      * correspondant et on affiche le résultat     
              EVALUATE WS-SIGN
                 WHEN '+'
                    PERFORM CALC-ADD                  
                 WHEN '-'
                    PERFORM CALC-SUBTR  
                 WHEN '*'
                    PERFORM CALC-MULTIP
                 WHEN '/'
                    PERFORM CALC-DIVISE
                 WHEN 'P'   
                    PERFORM CALC-P
                 WHEN OTHER
                    DISPLAY 'Cette operation n''est pas prévue'
                    MOVE 0 TO WS-BON-OPE 
              END-EVALUATE
      * On affiche le résultat que si la saisie est OK        
              IF WS-BON-OPE = 1 THEN PERFORM AFF-RESULT
              ELSE MOVE 1 TO WS-BON-OPE 
              END-IF   
                      
           END-PERFORM.


      * 0000-MAIN-END.    
           STOP RUN.
      ************************************************************** 

      *============================================================*
      *    Les sous-paragraphes
      *============================================================*

      **************************************************************
      * Récupère la saisie et teste les valeurs saisies            *
      **************************************************************
       DIS-BONJOUR.
           DISPLAY WS-AFF-BONJOUR. 
           DISPLAY WS-AFF-STOP.
           DISPLAY WS-AFF-DETAIL WITH NO ADVANCING.
           ACCEPT WS-DETAIL.
           MOVE FUNCTION TRIM(WS-DETAIL) TO WS-DETAIL.
           MOVE FUNCTION UPPER-CASE(WS-DETAIL) TO WS-DETAIL.
           INITIALIZE WS-NUM1 .
           DISPLAY 'Saisissez le premier chiffre - 3 positions maxi '
            WITH NO ADVANCING.
           ACCEPT WS-NUM1 .
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir
           MOVE  FUNCTION TRIM(WS-NUM1-CHAR) TO WS-CODE.
           PERFORM TST-CODE.
           MOVE WS-NUM1 TO WS-NUM1-CHAR . 
           INSPECT WS-NUM1-CHAR CONVERTING '0' TO SPACE.

       SAI-WS-SIGN.
           INITIALIZE WS-SIGN.    
           DISPLAY WS-AFF-SAI-SIGN WITH NO ADVANCING.
           ACCEPT WS-SIGN.   
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir
           MOVE FUNCTION TRIM(WS-SIGN) TO WS-CODE.
           MOVE FUNCTION TRIM(WS-SIGN) TO WS-SIGN.
           MOVE FUNCTION UPPER-CASE(WS-SIGN) TO WS-SIGN.
           PERFORM TST-CODE.
           
       SAI-WS-NUM2.    
           INITIALIZE WS-NUM2.
           DISPLAY 'Saisissez le deuxième chiffre - 3 positions maxi '
            WITH NO ADVANCING.
           ACCEPT WS-NUM2. 
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir           
           MOVE FUNCTION TRIM(WS-NUM2-CHAR) TO WS-CODE.
           PERFORM TST-CODE. 
      * Vérifie que le deuxième chiffre est correct vis à vis du premier
           PERFORM TST-VALEUR . 
           MOVE WS-NUM2 TO WS-NUM2-CHAR.
           INSPECT WS-NUM2-CHAR CONVERTING '0' TO SPACE.       

       TST-VALEUR.
           IF (WS-NUM1 < WS-NUM2) AND (WS-SIGN = '-') THEN
               DISPLAY WS-AFF-ERR1 
               PERFORM REINIT-NUM2
           ELSE    
               IF (WS-NUM1 < WS-NUM2) AND (WS-SIGN = '/') THEN 
                     DISPLAY WS-AFF-ERR2 
                     PERFORM REINIT-NUM2
               ELSE
                 IF (WS-NUM2 =0) AND (WS-SIGN = '/') THEN
                     DISPLAY WS-AFF-ERR3 
                     PERFORM REINIT-NUM2
                 END-IF
               END-IF
           END-IF.         

      * Si pas OK on demande une nouvelle saisie du Num2
       REINIT-NUM2.
           PERFORM SAI-WS-NUM2.
        

       CALC-ADD.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.

       CALC-SUBTR.
           SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT .          

       CALC-MULTIP.
           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.

       CALC-DIVISE.
           DIVIDE  WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.

       CALC-P.
           COMPUTE WS-RESULT = WS-NUM1 ** WS-NUM2.

      * Affiche a x b = c
       AFF-RESULT.
           
           MOVE WS-RESULT TO WS-AFF-RESULT . 
           IF WS-DETAIL = 'O' THEN
              DISPLAY WS-NUM1-AFF SPACE WS-SIGN 
              SPACE WS-NUM2-AFF SPACE '=' SPACE WS-AFF-RESULT  
           ELSE 
               DISPLAY WS-AFF-RESULT  
           END-IF.
      *    CHarge le résultat dans WS-NUM1 pour le prochain calcul 
      *    et laisse la main à la boucle
      *    qui gère l'initialisation et l'affichage pour la saisie
           MOVE WS-RESULT TO WS-NUM1.
           INITIALIZE WS-RESULT.
           
      * Test si l'utilisateur veut sortir, si oui le programme s'arrete
       TST-CODE.
           MOVE FUNCTION UPPER-CASE (WS-CODE) TO WS-CODE.
           IF STOPPER THEN
              STOP RUN
           END-IF.
