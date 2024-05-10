      *=============================================================*
      *    Choicexo permet de choisir l'exercice que l'utilisateur  *
      *    veut exécuter.                                           *
      *    Pour stopper le programme il lui suffira de choisir 0    *
      *    auteur : Isabelle Marand                                 *
      *    Date création 09/05/2024                                 *
      *=============================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. choicexo.
       AUTHOR . Isabelle Marand.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUMEXO      PIC XX      VALUE SPACE      .

      *    Variables pour le dialogue avec l'utilisateur
       01  WS-LIG-NUMEXO  PIC X(13)   VALUE 'Exo numéro:'             .
       01  WS-LIG-EDIT    PIC X(57)                                   .
       01  WS-LIG-BLC56   PIC X(55)   VALUE    SPACES                 .
       01  WS-LIG-STOP    PIC X(57)   VALUE 
           '* POUR ARRETER, choisissez 0                            *'.
       01  WS-LIG-SORRY   PIC X(58)   VALUE 
           'Désolé, cet exercice n''est pas encore finalisé. '.
       01  WS-LIG-ERROR   PIC X(58)   VALUE 
           'Le numéro d''exercice choisi n''existe pas. '.
       01  WS-LIG-EXO     PIC X(51)   VALUE
           '* Choisissez l''exercice que vous voulez faire :   '       .
       01  WS-LIG-FIL1    PIC X(58)   VALUE 
           '*            LISTE DES EXERCICES DISPONIBLES            *'.
       01  WS-LIG-FIL2    PIC X(58)   VALUE 
           '* EXERCICE 1: A partir d''un nombre de gourmands que     *'.
       01  WS-LIG-FIL3    PIC X(58)   VALUE 
           '*             vous renseignez, le programme calcule le  *'.
       01  WS-LIG-FIL4    PIC X(58)   VALUE 
           '*             nombre de pizzas à acheter                *'.
       01  WS-LIG-FIL5    PIC X(58)   VALUE 
           '* EXERCICE 2: Application de la conjecture de Syracuse  *'.           
       01  WS-LIG-FIL6    PIC X(58)   VALUE 
           '*             sur un nombre que vous saisirez           *'.
       01  WS-LIG-FIL7    PIC X(58)   VALUE 
           '* EXERCICE 3: Recherche des salaires min et max dans un *'.            
       01  WS-LIG-FIL8    PIC X(58)   VALUE 
           '*             un fichier client.                        *'.
       01  WS-LIG-FIL9    PIC X(58)   VALUE 
           '* EXERCICE 4: L''attaque de la reine.                    *'.            
       01  WS-LIG-FIL10   PIC X(58)   VALUE 
           '*             vous saisissez la position de la reine    *'.
       01  WS-LIG-FIL11   PIC X(58)   VALUE 
           '*             blanche, l''application calcule            *'.
       01  WS-LIG-FIL12   PIC X(58)   VALUE 
           '*             aléatoirement la position de la reine     *'.           
       01  WS-LIG-FIL13   PIC X(58)   VALUE 
           '*             noire et vous dit si la reine blanche     *'.                                         
       01  WS-LIG-FIL14   PIC X(58)   VALUE 
           '*             peut attaquer la reine noire.             *'.       

       
       
       PROCEDURE DIVISION .
      * 0000-MAIN-START.       
           MOVE ALL '*' TO WS-LIG-EDIT.
           DISPLAY WS-LIG-EDIT.
           DISPLAY '*' WS-LIG-BLC56 '*'
           DISPLAY WS-LIG-FIL1  .
           DISPLAY WS-LIG-FIL2  .
           DISPLAY WS-LIG-FIL3  .
           DISPLAY WS-LIG-FIL4  .
           DISPLAY WS-LIG-FIL5  .
           DISPLAY WS-LIG-FIL6  .
           DISPLAY WS-LIG-FIL7  .
           DISPLAY WS-LIG-FIL8  .
           DISPLAY WS-LIG-FIL9  .
           DISPLAY WS-LIG-FIL10 .
           DISPLAY WS-LIG-FIL11 .
           DISPLAY WS-LIG-FIL12 .
           DISPLAY WS-LIG-FIL13 .
           DISPLAY WS-LIG-FIL14 .
           DISPLAY WS-LIG-STOP  .
           DISPLAY '*' WS-LIG-BLC56 '*'
           DISPLAY WS-LIG-EDIT.

       RECOMMENCE-PRG.
           PERFORM 6000-INVITE-JEU-START THRU 6000-INVITE-JEU-END.
           IF (FUNCTION NUMVAL(FUNCTION TRIM(WS-NUMEXO)) NOT NUMERIC ) 
              THEN
               DISPLAY WS-LIG-ERROR
               GO TO RECOMMENCE-PRG       
           ELSE
              PERFORM UNTIL WS-NUMEXO = '0'
                 EVALUATE WS-NUMEXO 
                    WHEN '1'
                       DISPLAY WS-LIG-EXO WS-NUMEXO 
                       CALL 'nbpizzas'
                    WHEN '2' 
                       DISPLAY WS-LIG-EXO WS-NUMEXO 
                       CALL 'syracuse'
                    WHEN '3' 
                       DISPLAY WS-LIG-EXO WS-NUMEXO
                       CALL 'client'
                    WHEN '4'
                       DISPLAY WS-LIG-EXO WS-NUMEXO 
                       CALL 'echec'
                    WHEN OTHER 
                       DISPLAY WS-LIG-ERROR
                       GO TO RECOMMENCE-PRG 
                 END-EVALUATE       
                 PERFORM 6000-INVITE-JEU-START THRU 6000-INVITE-JEU-END
              END-PERFORM
           END-IF.      

      * 0000-MAIN-END.    
           STOP RUN.
      
       6000-INVITE-JEU-START.
           DISPLAY WS-LIG-STOP . 
           DISPLAY WS-LIG-EXO   WITH NO ADVANCING .
           ACCEPT WS-NUMEXO.
       6000-INVITE-JEU-END.
           EXIT.

