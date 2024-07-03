      *=============================================================*
      *    Choicexo permet de choisir l'exercice que l'utilisateur  *
      *    veut exécuter.                                           *
      *    Pour stopper le programme il lui suffira de choisir 0    *
      *    [IM]MAJV1: le 11/06/2024                                 *
      *    Ajout d'une screen section pour afficher le choix        *
      *    [IM]MAJV2: le 01/07/2024                                 *
      *    Ajout de l'exercice numéro 5                             * 
      *    auteur : Isabelle Marand                                 *
      *    Date création 09/05/2024                                 *
      *=============================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. choicexo.
       AUTHOR . Isabelle Marand.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE      PIC 99      VALUE 0        .
       01  WS-STOP        PIC X       VALUE SPACE    .

      *    Variables pour le dialogue avec l'utilisateur
       01  WS-LIG-EDIT    PIC X(57)                                   .
       01  WS-LIG-SORRY   PIC X(58)   VALUE 
           'Désolé, cet exercice n''est pas encore finalisé. '.
       01  WS-LIG-ERROR   PIC X(58)   VALUE 
           'Le numéro d''exercice choisi n''existe pas. '.

       SCREEN SECTION.
           COPY './copy/screen-choice-exo.cpy'.

       PROCEDURE DIVISION .
       0000-MAIN-START.       
           PERFORM UNTIL FUNCTION UPPER-CASE (WS-STOP) EQUAL 'X'
              PERFORM 1000-CHOICEEXO-START THRU END-1000-CHOICEEXO
           END-PERFORM.
       END-0000-MAIN.    
           STOP RUN.
      
       1000-CHOICEEXO-START. 
           ACCEPT SCR-CHOICEEXO.
           IF (FUNCTION UPPER-CASE (WS-STOP) EQUAL 'X') THEN
              STOP RUN
           ELSE
              IF (FUNCTION NUMVAL(FUNCTION TRIM(WS-CHOICE)) 
                 IS NOT NUMERIC ) 
              THEN
               DISPLAY WS-LIG-ERROR       
              ELSE
                 EVALUATE WS-CHOICE 
                    WHEN 1
                       CALL 'nbpizzas'
                    WHEN 2 
                       CALL 'syracuse'
                    WHEN 3 
                       CALL 'client'
                    WHEN 4 
                       CALL 'echec'
                    WHEN 5 
                       CALL 'romain'                       
                    WHEN OTHER 
                       DISPLAY WS-LIG-ERROR
                 END-EVALUATE       
              END-IF
           END-IF.     
       END-1000-CHOICEEXO.



