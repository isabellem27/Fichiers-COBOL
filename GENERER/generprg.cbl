      *=============================================================*
      *    Echange d'informations avec l'utilisateur                *
      *    Creation d'un fichier output.cbl contenant le skelette   *
      *    du programme à écrire                                    *
      *                                                             *      
      *    auteur : Isabelle Marand                                 *
      *    Date création 14/05/2024                                 *
      *=============================================================*    
       IDENTIFICATION DIVISION.
       PROGRAM-ID. generprg.
       AUTHOR . Isabelle Marand.

      
      * Environnement division et déclaration de tous les fichiers    
       COPY FILE-IO.


       DATA DIVISION.
      * File section pour tous les fichiers    
       COPY FILE-SECT.


       WORKING-STORAGE SECTION.
      * gestion des status des fichiers 
           COPY FILE-STAT.


      * variables 
       01  WS-ENCORE      PIC X       VALUE 'N'   .
       01  WS-NBENCORE    PIC 99      VALUE 0     .
       01  WS-PERSO       PIC X       VALUE SPACE .
       01  WS-ETAT        PIC 9       VALUE 0     .
       88  WS-FIN         VALUE 1                 .

       01  WS-PRG-NOM     PIC X(8)    VALUE SPACE .
       01  WS-PRG-AUT     PIC X(20)   VALUE SPACE .

       01  WS-NBSSPRG     PIC 99      VALUE 0     .
       01  WS-SSPRGNOM    PIC X(8)    VALUE SPACE . 
       01  WS-RET         PIC X       VALUE SPACE .
       01  WS-NBPARAM     PIC 99      VALUE 0     . 
       01  WS-PARAMNOM    PIC X(50)   VALUE SPACE .
       01  WS-PARAMTYP    PIC X       VALUE SPACE .
       01  WS-CNT         PIC 99      VALUE 0     .

       01  WS-OUTPUT      PIC 99      VALUE 0     .      
       01  WS-INPUT       PIC 99      VALUE 0     .
       01  WS-FICNOM      PIC X(20)   VALUE SPACE . 
       01  WS-FICTYP      PIC X       VALUE SPACE . 
       01  WS-LGENREG     PIC 999     VALUE 0     .            

       01  WS-SQL         PIC X       VALUE SPACE .
      
       01  WS-IND         PIC 999     VALUE 0     .
       01  WS-IND-NOM     PIC 999     VALUE 0     .

       01  WS-POS-CHAINE  PIC 99      VALUE 0     .
       01  WS-LNGFIN      PIC 99      VALUE 0     .
       01  WS-LNGDEB      PIC 99      VALUE 0     .
       01  WS-NBCARAC     PIC 99      VALUE 0     .
       01  WS-REF1        PIC 99      VALUE 0     .  
       01  WS-CNT1        PIC 9       VALUE 1     .     
       01  WS-NB-LIG      PIC 9       VALUE 0     .
       01  WS-LIG-CALL    PIC 9       VALUE 4     .
       01  WS-CONTENT     PIC X(7)    VALUE 'CONTENT'.
       01  WS-REFERENCE   PIC X(10)   VALUE 'REFERENCE'.
       01  WS-REPONSE     PIC X(21)   VALUE '           RETURNING '.
       01  WS-LIG-REFPARAM PIC X(81)  VALUE SPACE . 
       01  WS-LIG-CNTPARAM PIC X(81)  VALUE SPACE .

      
      * Table des paramètres de la base de données 
       01  WS-DBNAME.
           05 WS-SQLNOM      PIC X(20)   VALUE SPACE.
           05 WS-SQLUSER     PIC X(20)   VALUE SPACE.
           05 WS-SQLPSW      PIC X(20)   VALUE SPACE.

      * Table des fichiers à créer 
       01  WS-FIC.
           03  WS-NB-FIC     PIC 999      VALUE 0 .
           03  WS-FIC-TAB  OCCURS 1 TO 999 
                             DEPENDING WS-NB-FIC
                             INDEXED BY IDX-FIC   .
              05 WS-FIC-ID       PIC X            .
              05 WS-FIC-VF       PIC X            .
              05 WS-FIC-NOM      PIC X(9)         .
              05 WS-FIC-ENREG    PIC 999          . 

      * Table des sous-programmes à créer 
       01  WS-SSPRG.
           03  WS-NB-SSPRG      PIC 99      VALUE 0  .
           03  WS-SSPRG-TAB  OCCURS 1 TO 99 
                             DEPENDING WS-NB-SSPRG
                             INDEXED BY IDX-SSPRG    .
              05 WS-SSPRG-NOM      PIC X(8)          .
              05 WS-SSPRG-REP      PIC X             .
              05 WS-REP-NOM        PIC X(20)         . 
              05 WS-NB-PARAM       PIC 99            .             
              05 WS-PARAM-TAB  OCCURS 10             . 
                 10 WS-PARAM-TYP   PIC X             .
                 10 WS-PARAM-NOM   PIC X(20)         .  

      * gestion de la date
       01  WS-DATE-YMD.
           05 WS-DAT-YY   PIC 9(4).
           05 WS-DAT-MM   PIC 99.
           05 WS-DAT-JJ   PIC 99.
           05 WS-DAT-REST PIC X(13).

       01  WS-DATE-DMY.
           05 WS-DAT-JJ   PIC 99      .
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-MM   PIC 99      .
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-YY   PIC 9(4)    .
          
      * Gestion du dialogue avec l'utilisateur
       01  WS-LIG-RAP      PIC X(100)  VALUE   SPACE    .   
       01  WS-LIG-TITRE    PIC X(40)   VALUE 
           'PERSONNALISATION DES FICHIERS EN SORTIE'    . 
       01  WS-LIG-BLC      PIC X(10)   VALUE   SPACES   .  
      *01  FIC-TITRE       PIC X(40)  .

      * debug
       01  PAUSE-TIME PIC S9(09) BINARY VALUE 500.
       01  WS-DEBUG    PIC X(5) VALUE SPACE.
       

       SCREEN SECTION. 
      * Debugger le programme?
       01  DEBUG-SCREEN.  
           05 FILLER PIC X(5) USING WS-DEBUG     LINE 9 COL 10.
      *    05 FILLER PIC X(80) USING PAUSE-TIME   LINE 10 COL 10.  
                
      * Faire un nouveau programme?
       01  AGAIN-SCREEN.
           05 BLANK SCREEN .  
           05 FILLER FROM 'Voulez vous faire un autre programme?' 
                 LINE 6 COL 10.
           05 FILLER PIC X USING WS-ENCORE COL 47.   

      * Paramétrage de l'identification division et 
      * choix de la personnalisation ou non
       01  IDENT-SCREEN.
           05 BLANK SCREEN .
           05 FILLER FROM 
           'BIENVENUE DANS LA GENERATION AUTOMATIQUE DE PROGRAMMES' 
           LINE 2 COL 25.
           05 FILLER FROM 
           '======================================================' 
           LINE 3 COL 25.
           05 FILLER FROM 'Quel sera le nom du programme?' 
                 LINE 6 COL 10.
           05 FILLER PIC X(8) USING WS-PRG-NOM COL 46.
           05 FILLER FROM 'Quel est l''auteur du programme?'
             LINE 7 COL 10.
           05 FILLER PIC X(20) USING WS-PRG-AUT COL 46.
           05 FILLER FROM 'Voulez vous personnaliser le programme?'
             LINE 8 COL 10.
           05 FILLER FROM 'Repondez par O pour Oui, N pour Non.' 
             LINE 9 COL 10.            
           05 FILLER PIC X USING WS-PERSO COL 46.     
      * Menu personnalisation 
      * nb fichier, nbssprog, sql
       01  PERSO-SCREEN.
           05 BLANK SCREEN .
           05 FILLER FROM 
           'PERSONNALISATION DU PROGRAMME' 
           LINE 2 COL 25.
           05 FILLER FROM 
           '=============================' 
           LINE 3 COL 25.
           05 FILLER FROM 'Combien de fichiers en entree?' 
                 BLANK SCREEN LINE 6 COL 10.
           05 FILLER PIC 99 USING WS-INPUT COL 46.
           05 FILLER FROM 'Combien de fichiers en sortie?'
             LINE 7 COL 10.
           05 FILLER PIC 99 USING WS-OUTPUT COL 46.
           05 FILLER FROM 'Combien de sous-programmes ou entry?'
             LINE 8 COL 10.            
           05 FILLER PIC X USING WS-NBSSPRG COL 47.   
           05 FILLER FROM 
              'Le programme utilise une base de donnees? '
             LINE 9 COL 10.            
           05 FILLER PIC X USING WS-SQL COL 67.     
           05 FILLER FROM 
              'Quel est le nom de base de donnees? '
             LINE 10 COL 10.            
           05 FILLER PIC X(20) USING WS-SQLNOM COL 46. 
           05 FILLER FROM 
              'Quel est le user se connecte a la base de donnees? '
             LINE 11 COL 10.            
           05 FILLER PIC X(20) USING WS-SQLUSER COL 61. 
           05 FILLER FROM 
              'Quel est le mot de passe de la base de donnees? '
             LINE 12 COL 10.            
           05 FILLER FROM
            'Respectez la casse. Mettre blanc si pas de mot de passe '
             LINE 13 COL 10. 
           05 FILLER PIC X(20) TO WS-SQLPSW SECURE COL 65. 
      * Menu fichier 
      * nom fichier, longueur enreg, enreg. fixe/variable
       01  FIC-SCREEN.
           05 BLANK SCREEN .
           05 FIC-TITRE FROM 
           'PERSONNALISATION DES FICHIERS EN ENTREE' 
           LINE 2 COL 25.
          05 FILLER FROM 
           '=======================================' 
           LINE 3 COL 25.
           05 FILLER FROM 'Nom logique du fichier' 
                 BLANK SCREEN LINE 6 COL 10.
           05 FILLER PIC X(20) USING WS-FICNOM COL 34.
           05 FILLER FROM 
              'Quel type d''enregistrement V (variable) ou F(fixe)?'
             LINE 7 COL 10.
           05 FILLER PIC X USING WS-FICTYP COL 62.
           05 FILLER FROM 'Longueur max. de l''enregistrement '
             LINE 8 COL 10.            
           05 FILLER PIC 999 USING WS-LGENREG COL 45.   
      * Menu sous-programmes ou entry 
      * nom ssprog., nb param, nom param retour
       01  SSPRG-SCREEN.
           05 BLANK SCREEN .
             05 FILLER FROM 
           'PERSONNALISATION DES SOUS-PROGRAMME OU ENTRY' 
           LINE 2 COL 25.
           05 FILLER FROM 
           '============================================' 
           LINE 3 COL 25.
           05 FILLER FROM 'Nom du sous-programme ' 
                 BLANK SCREEN LINE 6 COL 10.
           05 FILLER PIC X(20) USING WS-SSPRGNOM COL 36.
           05 FILLER FROM 
              'Le programme attend t''il un retour? '
             LINE 7 COL 10.
           05 FILLER PIC X USING WS-RET COL 45.
           05 FILLER FROM 'Nom du parametre ' LINE 8 COL 10.            
           05 FILLER PIC X(50) USING WS-PARAMNOM COL 30.   
           05 FILLER FROM 
           'Combien de parametres a transmettre (maxi 10)?'
             LINE 9 COL 10.            
           05 FILLER PIC 99 USING WS-NBPARAM COL 60.   

      * Menu parametre 
      * nom parametre, type parametre
       01  PARAM-SCREEN.
           05 BLANK SCREEN .
           05 FILLER FROM 
           'PERSONNALISATION DES PARAMETRES A TRANSMETTRE' 
           LINE 2 COL 25.
           05 FILLER FROM 
           '=============================================' 
           LINE 3 COL 25.
           05 FILLER FROM 'Nom du parametre ' LINE 6 COL 10.            
           05 FILLER PIC X(50) USING WS-PARAMNOM COL 30. 
           05 FILLER FROM 
              'Type du parametre R pour reference, C pour content'
             LINE 7 COL 10.
           05 FILLER PIC X USING WS-PARAMTYP COL 66. 
 
       PROCEDURE DIVISION. 
      *0000-main-start.
      *   MOVE 'O' TO WS-ENCORE.
      *    PERFORM UNTIL (FUNCTION UPPER-CASE(WS-ENCORE) = 'N')

           MOVE FUNCTION CURRENT-DATE TO WS-DATE-YMD.
           MOVE CORR WS-DATE-YMD TO WS-DATE-DMY.

           PERFORM 4000-DISPLAY-IDENT-START 
                          THRU 4000-DISPLAY-IDENT-END
           PERFORM 1000-PARAM-UTI-START THRU 1000-PARAM-UTI-END
           PERFORM 1050-PRG-GENERE-START THRU 1050-PRG-GENERE-START
      *       ACCEPT AGAIN-SCREEN 
      *    END-PERFORM.
      *0000-main-end.
           STOP RUN.

       1000-PARAM-UTI-START.          
      * le skelete doit il etre personnalisé?
           IF (FUNCTION UPPER-CASE(WS-PERSO) = 'O') THEN 
              PERFORM 4000-DISPLAY-PERSO-START 
                          THRU 4000-DISPLAY-PERSO-END 
              PERFORM 1010-PARAM-FIC-START THRU 1010-PARAM-FIC-END 
              PERFORM 1020-PARAM-SSPRG-START THRU 1020-PARAM-SSPRG-END           
           END-IF.
       1000-PARAM-UTI-END.
           EXIT.

       1010-PARAM-FIC-START.
      *       Fichiers en entrée
              IF WS-INPUT > 0 THEN             
                 PERFORM VARYING WS-IND-NOM  FROM 1 BY 1 
                 UNTIL (WS-IND-NOM > (WS-INPUT))
                    ADD 1 TO WS-NB-FIC GIVING WS-NB-FIC
                    PERFORM 4000-DISPLAY-FIC-START 
                          THRU 4000-DISPLAY-FIC-END
      *             Chargement dans table fichier
                    MOVE 'I' TO WS-FIC-ID (WS-IND-NOM)
                    MOVE FUNCTION UPPER-CASE(WS-FICTYP)  
                                   TO WS-FIC-VF (WS-IND-NOM)                     
                    MOVE FUNCTION UPPER-CASE(WS-FICNOM) 
                                   TO WS-FIC-NOM (WS-IND-NOM)
                    MOVE WS-LGENREG TO WS-FIC-ENREG (WS-IND-NOM)
                    INITIALIZE WS-FICTYP WS-FICNOM WS-LGENREG
                 END-PERFORM
              END-IF.

      *       Fichiers en sortie
              IF WS-OUTPUT > 0 THEN
                 ADD 1 TO WS-INPUT GIVING WS-IND
                 ADD WS-OUTPUT TO WS-INPUT GIVING WS-NB-FIC   
                 PERFORM VARYING WS-IND-NOM  FROM WS-IND BY 1  
                 UNTIL (WS-IND-NOM > WS-NB-FIC)
                    MOVE WS-LIG-TITRE TO FIC-TITRE
                    PERFORM 4000-DISPLAY-FIC-START 
                          THRU 4000-DISPLAY-FIC-END      
      *             Chargement dans table fichier
                    MOVE 'O' TO WS-FIC-ID (WS-IND-NOM)
                    MOVE FUNCTION UPPER-CASE(WS-FICTYP)  
                                   TO WS-FIC-VF (WS-IND-NOM)                    
                    MOVE FUNCTION UPPER-CASE(WS-FICNOM)
                                   TO WS-FIC-NOM (WS-IND-NOM)
                    MOVE WS-LGENREG TO WS-FIC-ENREG (WS-IND-NOM)
                    INITIALIZE WS-FICTYP WS-FICNOM WS-LGENREG
                 END-PERFORM
              END-IF.
       1010-PARAM-FIC-END.
           EXIT.

       1020-PARAM-SSPRG-START.         
              INITIALIZE WS-RET.
      *       Sous-programme ou entry
              IF WS-NBSSPRG > 0 THEN
                 SET WS-NB-SSPRG TO WS-NBSSPRG   
                 PERFORM VARYING WS-IND-NOM  FROM 1 BY 1  
                 UNTIL (WS-IND-NOM > WS-NB-SSPRG)
                    INITIALIZE WS-NBPARAM 
                    PERFORM 4000-DISPLAY-SSPRG-START 
                          THRU 4000-DISPLAY-SSPRG-END 
      *             Chargement dans table sous-programme
                    MOVE FUNCTION UPPER-CASE(WS-SSPRGNOM)
                                    TO WS-SSPRG-NOM (WS-IND-NOM)  
                    MOVE FUNCTION UPPER-CASE(WS-RET) 
                                   TO WS-SSPRG-REP (WS-IND-NOM)  
                    MOVE FUNCTION UPPER-CASE(WS-PARAMNOM)
                                   TO WS-REP-NOM (WS-IND-NOM)  
                    SET WS-NB-PARAM(WS-IND-NOM) TO WS-NBPARAM 
                    INITIALIZE WS-PARAMNOM WS-RET WS-SSPRGNOM

      *             Détail des parametres 
                    IF (WS-NBPARAM > 0) AND (WS-NBPARAM < 11) THEN    
                       PERFORM VARYING WS-IND  FROM 1 BY 1  
                       UNTIL (WS-IND > WS-NBPARAM)
                          PERFORM 4000-DISPLAY-PARAM-START 
                          THRU 4000-DISPLAY-PARAM-END                       
      *                   Chargement dans table paramètre du ssprg
                          EVALUATE FUNCTION UPPER-CASE(WS-PARAMTYP)
      * pour charger les paramètres en fonction de leur type
      * en début de table les références en fin de table les contents 
                            WHEN 'R'
                                MOVE FUNCTION UPPER-CASE(WS-PARAMNOM) 
                                TO WS-PARAM-NOM (WS-IND-NOM,WS-IND) 
                                MOVE FUNCTION UPPER-CASE(WS-PARAMTYP) 
                                TO WS-PARAM-TYP (WS-IND-NOM,WS-IND)
                             WHEN 'C'                      
                                SUBTRACT WS-IND FROM 11 GIVING WS-CNT
                                MOVE FUNCTION UPPER-CASE(WS-PARAMNOM) 
                                TO WS-PARAM-NOM (WS-IND-NOM,WS-CNT) 
                                MOVE FUNCTION UPPER-CASE(WS-PARAMTYP) 
                                TO WS-PARAM-TYP (WS-IND-NOM,WS-CNT)
                           END-EVALUATE  
 
      *                       display  ws-paramtyp ' tab ' 
      *                      WS-PARAM-TYP (WS-IND-NOM,WS-CNT) line 30

                           INITIALIZE WS-PARAMNOM WS-PARAMTYP                                                                     
                       END-PERFORM   
                    END-IF
                 END-PERFORM
              END-IF.
       1020-PARAM-SSPRG-END.
           EXIT.       

       1050-PRG-GENERE-START.
           IF FUNCTION UPPER-CASE(WS-SQL) = 'O' THEN 
              CALL 'genersql' USING BY REFERENCE WS-PRG-NOM WS-PRG-AUT 
                         WS-SSPRG WS-DBNAME WS-DATE-DMY WS-FIC
           ELSE
              PERFORM 2000-OPEN-PRG-START THRU 2000-OPEN-PRG-END
              PERFORM 3030-IDENT-DIV-START THRU 3030-IDENT-DIV-END
              PERFORM 3000-ENV-DIV-START THRU 3000-ENV-DIV-END          
              EVALUATE FUNCTION UPPER-CASE(WS-PERSO)
                 WHEN 'O'
                    PERFORM 1070-PRG-PERSO-START 
                          THRU 1070-PRG-PERSO-END
                 WHEN OTHER 
                    PERFORM 1060-PRG-SIMPLE-START 
                          THRU 1060-PRG-SIMPLE-END
              END-EVALUATE
              PERFORM 2020-CLOSE-PRG-START THRU 2020-CLOSE-PRG-END
              SET WS-ETAT TO 1
           END-IF.
       1050-PRG-GENERE-END.
           EXIT.

       1060-PRG-SIMPLE-START.
           PERFORM 3000-DATA-DIV-START 
                          THRU 3000-DATA-DIV-END.
           INITIALIZE REC-F-PRG .
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END. 
           PERFORM 3000-WS-SECT-START 
                          THRU 3000-WS-SECT-END.       
           INITIALIZE REC-F-PRG.                      
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END.
           PERFORM 3030-PROC-DIV-START THRU 3030-PROC-DIV-END.
       1060-PRG-SIMPLE-END.
           EXIT.

       1070-PRG-PERSO-START.
           PERFORM 3000-IO-FILE-START THRU 3000-IO-FILE-END.
           IF WS-NB-FIC > 0 THEN 
              PERFORM 3000-FILE-CONT-START THRU 3000-FILE-CONT-END
           END-IF.
           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL (WS-IND > WS-NB-FIC)
              PERFORM 3030-FILE-IO-START THRU 3030-FILE-IO-END           
           END-PERFORM.   

           PERFORM 3000-DATA-DIV-START 
                          THRU 3000-DATA-DIV-END. 
           PERFORM 3000-FILE-SECT-START 
                          THRU 3000-FILE-SECT-END. 

           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL (WS-IND > WS-NB-FIC)
                 EVALUATE WS-FIC-VF(WS-IND)
                    WHEN 'V'
                       PERFORM 3035-FILEV-SECT-START 
                          THRU 3035-FILEV-SECT-END
                    WHEN OTHER
                       PERFORM 3035-FILEF-SECT-START 
                          THRU 3035-FILEF-SECT-END
                 END-EVALUATE                  
           END-PERFORM. 

           INITIALIZE REC-F-PRG .
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
           PERFORM 3000-WS-SECT-START 
                          THRU 3000-WS-SECT-END. 

           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL (WS-IND > WS-NB-FIC)
                   PERFORM 3035-FILE-STATUS-START 
                             THRU 3035-FILE-STATUS-END                
           END-PERFORM. 
           INITIALIZE REC-F-PRG.                      
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END.
           PERFORM 3030-PROC-DIV-START THRU 3030-PROC-DIV-END.
           
           PERFORM 2000-OPEN-GEST-FILI-START 
                          THRU 2000-OPEN-GEST-FILI-END.
           PERFORM 2030-READ-GEST-FILI-START 
                          THRU 2030-READ-GEST-FILI-END. 
           PERFORM 2000-OPEN-GEST-FILO-START 
                          THRU 2000-OPEN-GEST-FILO-END.
           PERFORM 2030-READ-GEST-FILO-START 
                          THRU 2030-READ-GEST-FILO-END.                           
           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL (WS-IND > WS-NB-FIC)
                 EVALUATE WS-FIC-ID(WS-IND)
                    WHEN 'I'
                       PERFORM 3035-GEST-FILI-START 
                          THRU 3035-GEST-FILI-END
                    WHEN OTHER
                       PERFORM 3035-GEST-FILO-START 
                          THRU 3035-GEST-FILO-END
                 END-EVALUATE                                   
           END-PERFORM.  
           PERFORM 2020-CLOSE-GEST-FILI-START 
                       THRU 2020-CLOSE-GEST-FILI-END.
           PERFORM 2020-CLOSE-GEST-FILO-START 
                       THRU 2020-CLOSE-GEST-FILO-END.            

           PERFORM 3035-TST-STAT-START 
                             THRU 3035-TST-STAT-END.        
       1070-PRG-PERSO-END.
           EXIT.

      * open, read, write, close de tous les fichiers 
      * utilisés dans generprg
           COPY FILE-GEST.
       
       3000-ENV-DIV-START.
           PERFORM 2000-OPEN-ENV-DIV-START THRU 2000-OPEN-ENV-DIV-END.
           PERFORM 2030-READ-ENV-DIV-START THRU 2030-READ-ENV-DIV-END.
           IF (WS-NB-FIC = 0) THEN
              STRING REC-ENV-DIV(1:6)
                       '*'
                     REC-ENV-DIV(8:71)  
              DELIMITED BY SIZE
              INTO REC-ENV-DIV 
           END-IF.
           PERFORM UNTIL ENV-DIV-STATUS-EOF
              MOVE REC-ENV-DIV TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-ENV-DIV-START THRU 2030-READ-ENV-DIV-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-ENV-DIV-START THRU 2020-CLOSE-ENV-DIV-END.
       3000-ENV-DIV-END.
           EXIT.
       
       3000-DATA-DIV-START.
           PERFORM 2000-OPEN-DATA-DIV-START THRU 2000-OPEN-DATA-DIV-END.
           PERFORM 2030-READ-DATA-DIV-START THRU 2030-READ-DATA-DIV-END.
           PERFORM UNTIL DATA-DIV-STATUS-EOF
              MOVE REC-DATA-DIV TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-DATA-DIV-START 
                          THRU 2030-READ-DATA-DIV-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-DATA-DIV-START 
                       THRU 2020-CLOSE-DATA-DIV-END.
       3000-DATA-DIV-END.
           EXIT.
       
       3000-FILE-SECT-START. 
           PERFORM 2000-OPEN-FILE-SECTION-START 
                       THRU 2000-OPEN-FILE-SECTION-END.
           PERFORM 2030-READ-FILE-SECTION-START 
                       THRU 2030-READ-FILE-SECTION-END.
           PERFORM UNTIL FILE-SECTION-STATUS-EOF
              MOVE REC-FILE-SECTION TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-SECTION-START 
                          THRU 2030-READ-FILE-SECTION-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-SECTION-START 
                       THRU 2020-CLOSE-FILE-SECTION-END.       
       3000-FILE-SECT-END.
           EXIT.
       3000-FILE-CONT-START.
           PERFORM 2000-OPEN-FILE-CONT-START 
                       THRU 2000-OPEN-FILE-CONT-END.
           PERFORM 2030-READ-FILE-CONT-START 
                       THRU 2030-READ-FILE-CONT-END.
           PERFORM UNTIL FILE-CONT-STATUS-EOF
              MOVE REC-FILE-CONT TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-CONT-START 
                          THRU 2030-READ-FILE-CONT-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-CONT-START 
                       THRU 2020-CLOSE-FILE-CONT-END.       
       3000-FILE-CONT-END.
           EXIT.

       3000-IO-FILE-START. 
           PERFORM 2000-OPEN-IO-SECTION-START 
                    THRU 2000-OPEN-IO-SECTION-END.
           PERFORM 2030-READ-IO-SECTION-START 
                    THRU 2030-READ-IO-SECTION-END.
           PERFORM UNTIL IO-SECTION-STATUS-EOF
              MOVE REC-IO-SECTION TO REC-F-PRG                            
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-IO-SECTION-START 
                    THRU 2030-READ-IO-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-IO-SECTION-START 
                    THRU 2020-CLOSE-IO-SECTION-END.
       3000-IO-FILE-END.
           EXIT.   

       3000-WS-SECT-START.
           PERFORM 2000-OPEN-WS-SECTION-START 
                          THRU 2000-OPEN-WS-SECTION-END.
           PERFORM 2030-READ-WS-SECTION-START 
                               THRU 2030-READ-WS-SECTION-END.
           PERFORM UNTIL WS-SECTION-STATUS-EOF
              MOVE REC-WS-SECTION TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-WS-SECTION-START 
                               THRU 2030-READ-WS-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-WS-SECTION-START 
                             THRU 2020-CLOSE-WS-SECTION-END.
       3000-WS-SECT-END.
           EXIT.
      
       3030-IDENT-DIV-START.
           PERFORM 2000-OPEN-IDENT-DIV-START 
                    THRU 2000-OPEN-IDENT-DIV-END.
           PERFORM 2030-READ-IDENT-DIV-START 
                    THRU 2030-READ-IDENT-DIV-END.
           PERFORM UNTIL IDENT-DIV-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-IDENT-DIV 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMPRG:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMPRG:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 9 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING REC-IDENT-DIV(1:WS-LNGDEB)
                      FUNCTION UPPER-CASE(FUNCTION TRIM(WS-PRG-NOM))
                      REC-IDENT-DIV(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
              INSPECT REC-IDENT-DIV 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':DATECREA:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':DATECREA:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 11 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING REC-IDENT-DIV(1:WS-LNGDEB)
                      WS-DATE-DMY REC-IDENT-DIV(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              END-IF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
              INSPECT REC-IDENT-DIV 
              TALLYING WS-LNGDEB FOR CHARACTERS 
                                      BEFORE ':NOMAUT:            ' 
                       WS-LNGFIN FOR CHARACTERS 
                                      AFTER ':NOMAUT:            '
              IF (WS-LNGDEB < 80) THEN
                 ADD 21 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING REC-IDENT-DIV(1:WS-LNGDEB)
                    FUNCTION UPPER-CASE(FUNCTION TRIM(WS-PRG-AUT)) '.'
                      REC-IDENT-DIV(WS-POS-CHAINE:WS-LNGFIN)        
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              END-IF 
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-IDENT-DIV TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-IDENT-DIV-START 
                    THRU 2030-READ-IDENT-DIV-END
           END-PERFORM.
           PERFORM 2020-CLOSE-IDENT-DIV-START 
                    THRU 2020-CLOSE-IDENT-DIV-END.
       3030-IDENT-DIV-END.
           EXIT.

       3030-FILE-IO-START. 
           PERFORM 2000-OPEN-FILE-IO-START 
                    THRU 2000-OPEN-FILE-IO-END.
           PERFORM 2030-READ-FILE-IO-START 
                    THRU 2030-READ-FILE-IO-END.
           PERFORM UNTIL FILE-IO-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-FILE-IO 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMFILE:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMFILE:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 10 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-FILE-IO(1:WS-LNGDEB)
                          FUNCTION TRIM (WS-FIC-NOM (WS-IND))
                          REC-FILE-IO(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-FILE-IO TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-IO-START 
                    THRU 2030-READ-FILE-IO-END
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-IO-START 
                    THRU 2020-CLOSE-FILE-IO-END.
       3030-FILE-IO-END.
           EXIT.
 
       3030-PROC-DIV-START. 
           PERFORM 2000-OPEN-PROC-DIV-START 
                          THRU 2000-OPEN-PROC-DIV-END.
           PERFORM 2030-READ-PROC-DIV-START 
                               THRU 2030-READ-PROC-DIV-END.
           PERFORM UNTIL PROC-DIV-STATUS-EOF
              ADD 1 TO WS-NB-LIG
              MOVE REC-PROC-DIV TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-PROC-DIV-START 
                               THRU 2030-READ-PROC-DIV-END
              IF (WS-NB-SSPRG > 0) AND (WS-NB-LIG = WS-LIG-CALL) THEN
                 PERFORM VARYING WS-IND FROM 1 BY 1 
                 UNTIL (WS-IND > WS-NB-SSPRG) 
                       PERFORM 3035-CALL-SSPRG-START 
                               THRU 3035-CALL-SSPRG-END
                 END-PERFORM
              END-IF
           END-PERFORM.
           PERFORM 2020-CLOSE-PROC-DIV-START 
                             THRU 2020-CLOSE-PROC-DIV-END.
       3030-PROC-DIV-END.
           EXIT.

       3035-CALL-SSPRG-START.           
           SET WS-REF1 TO 1 .
           PERFORM 2000-OPEN-CALL-SSPRG-START 
                          THRU 2000-OPEN-CALL-SSPRG-END.                       
           PERFORM 2030-READ-CALL-SSPRG-START 
                               THRU 2030-READ-CALL-SSPRG-END.                  
           PERFORM UNTIL CALL-SSPRG-STATUS-EOF 
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-CALL-SSPRG 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMSSPRG:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMSSPRG:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 11 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-CALL-SSPRG(1:WS-LNGDEB)
                          FUNCTION TRIM (WS-SSPRG-NOM (WS-IND))
                          REC-CALL-SSPRG(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF

              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN 
              INSPECT REC-CALL-SSPRG 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':REF:'
                       WS-LNGFIN FOR CHARACTERS BEFORE 'USING'
              IF (WS-LNGDEB < 80) THEN
      * gestion des paramètres à transmettre        
                 IF WS-NB-PARAM (WS-IND) > 0 THEN
                    PERFORM 3035-SSPRG-PARAM-START 
                          THRU 3035-SSPRG-PARAM-END               
                 END-IF         
      * le sous-programme renvoie une réponse           
                 SET WS-REF1 TO 1
                 IF (WS-SSPRG-REP(WS-IND)) = 'O' THEN
                    INITIALIZE REC-F-PRG 
                    STRING   WS-REPONSE
                         SPACE 
                         FUNCTION TRIM (WS-REP-NOM(WS-IND))
                    DELIMITED BY SIZE 
                    INTO REC-F-PRG  
                 ELSE 
                    PERFORM 2030-READ-CALL-SSPRG-START 
                           THRU 2030-READ-CALL-SSPRG-END                          
                 END-IF
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-CALL-SSPRG TO REC-F-PRG 
              END-IF    

              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END        
              PERFORM 2030-READ-CALL-SSPRG-START 
                    THRU 2030-READ-CALL-SSPRG-END  
           END-PERFORM.
           PERFORM 2020-CLOSE-CALL-SSPRG-START 
                             THRU 2020-CLOSE-CALL-SSPRG-END.        
       3035-CALL-SSPRG-END.
           EXIT.
       
       3035-SSPRG-PARAM-START. 
           SET WS-REF1 TO 1.
           SET WS-CNT1 TO 1.
           ADD 6 TO WS-LNGDEB GIVING WS-POS-CHAINE
           INITIALIZE REC-F-PRG WS-LIG-REFPARAM WS-LIG-CNTPARAM.
           PERFORM VARYING WS-IND-NOM FROM 1 BY 1 
           UNTIL (WS-IND-NOM > 10)
              
      *       string '/' WS-PARAM-TYP(WS-IND, WS-IND-NOM) '/'
      *       DElimited by size
      *       into WS-DEBUG              
      *       display debug-screen 
      *       DISPLAY " " LINE 9 COL 10 
      *       ACCEPT WS-debug AT LINE 9 COL 10
              
              EVALUATE WS-PARAM-TYP(WS-IND, WS-IND-NOM)
                 WHEN 'R'
                    PERFORM 3035-PARAM-REF-START 
                          THRU 3035-PARAM-REF-END 
                 WHEN 'C'
      *    Ecriture des derniers paramètres by référence           
                    IF (FUNCTION LENGTH(
                          FUNCTION TRIM(WS-LIG-REFPARAM)
                                         ) > 0) THEN 
                       MOVE WS-LIG-REFPARAM(2:80) TO REC-F-PRG 
                       PERFORM 2010-WRITE-PRG-START 
                                   THRU 2010-WRITE-PRG-END
                       INITIALIZE WS-LIG-REFPARAM REC-F-PRG 
                    END-IF                             
                    PERFORM 3035-PARAM-CNT-START 
                          THRU 3035-PARAM-CNT-END
                 WHEN OTHER 
                    SET WS-REF1 TO 0         
              END-EVALUATE                 
           END-PERFORM.                          
           INITIALIZE REC-F-PRG
           IF (FUNCTION LENGTH(FUNCTION TRIM(WS-LIG-CNTPARAM)) > 0)
           THEN 
      * Ecriture des derniers paramètres content
              MOVE WS-LIG-CNTPARAM(2:80) TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              INITIALIZE WS-LIG-CNTPARAM REC-F-PRG
           ELSE 
      *    Si pas de parametre content, il reste peut-être des 
      *    paramètres reference à écrire
              IF (FUNCTION LENGTH(
                          FUNCTION TRIM(WS-LIG-REFPARAM)
                                         ) > 0) THEN 
                   MOVE WS-LIG-REFPARAM(2:80) TO REC-F-PRG 
                   PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
                   INITIALIZE WS-LIG-REFPARAM REC-F-PRG                    
               END-IF                             
           END-IF.                              
       3035-SSPRG-PARAM-END.
           EXIT.

       3035-PARAM-REF-START.
           ADD FUNCTION LENGTH(
            FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                                )
           TO FUNCTION LENGTH (FUNCTION TRIM (WS-LIG-REFPARAM)) 
           GIVING WS-NBCARAC
           IF WS-NBCARAC < 80 THEN
               IF (WS-REF1 = 1) THEN 
                   SET WS-REF1 TO 0
                   STRING '*'
                          REC-CALL-SSPRG(1:WS-LNGDEB)
                          SPACE
                          WS-REFERENCE
                          SPACE
                   FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                   DELIMITED BY SIZE
                   INTO WS-LIG-REFPARAM   
               ELSE
                   STRING FUNCTION TRIM(WS-LIG-REFPARAM)
                           SPACE 
                   FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                   DELIMITED BY SIZE
                   INTO WS-LIG-REFPARAM
               END-IF 
           ELSE 
               MOVE WS-LIG-REFPARAM(2:80) TO REC-F-PRG
               PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
               INITIALIZE WS-LIG-REFPARAM REC-F-PRG
               STRING '*'
                       REC-CALL-SSPRG(1:WS-LNGFIN)
                       SPACE
               FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
               DELIMITED BY SIZE
               INTO WS-LIG-REFPARAM                 
           END-IF.
       3035-PARAM-REF-END.
           EXIT.

       3035-PARAM-CNT-START.                  
           ADD FUNCTION LENGTH(
            FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                                )
           TO FUNCTION LENGTH (FUNCTION TRIM (WS-LIG-CNTPARAM)) 
           GIVING WS-NBCARAC
           IF WS-NBCARAC < 80 THEN
               IF (WS-CNT1 = 1) THEN 
                   SET WS-REF1 TO 0
                   SET WS-CNT1 TO 0                   
                   STRING '*'
                          REC-CALL-SSPRG(1:WS-LNGDEB)
                          SPACE
                          WS-CONTENT
                          SPACE
                   FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                   DELIMITED BY SIZE
                   INTO WS-LIG-CNTPARAM   
               ELSE
                   STRING FUNCTION TRIM(WS-LIG-CNTPARAM)
                          SPACE 
                   FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
                   DELIMITED BY SIZE
                   INTO WS-LIG-CNTPARAM
               END-IF 
           ELSE 
               MOVE WS-LIG-CNTPARAM(2:80) TO REC-F-PRG
               PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
               INITIALIZE WS-LIG-CNTPARAM REC-F-PRG
               STRING  '*'
                       REC-CALL-SSPRG(1:WS-LNGFIN)
               FUNCTION TRIM (WS-PARAM-NOM (WS-IND,WS-IND-NOM))
               DELIMITED BY SIZE
               INTO WS-LIG-CNTPARAM                 
           END-IF.           
       3035-PARAM-CNT-END.
           EXIT.

       3035-TST-STAT-START. 
           PERFORM 2000-OPEN-TST-STAT-START THRU 2000-OPEN-TST-STAT-END.
           PERFORM 2030-READ-TST-STAT-START THRU 2030-READ-TST-STAT-END.           
           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL (WS-IND > WS-NB-FIC)
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-TST-STAT 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':WS-FICNOM:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':WS-FICNOM:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 12 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-TST-STAT(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-TST-STAT(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-TST-STAT TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END               
           END-PERFORM. 
           PERFORM 2020-CLOSE-TST-STAT-START 
                       THRU 2020-CLOSE-TST-STAT-END.       
       3035-TST-STAT-END.
           EXIT.

       3035-GEST-FILI-START.
           INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG.
           INSPECT REC-GEST-FILI 
           TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':WS-FICNOM:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':WS-FICNOM:'.
           IF (WS-LNGDEB < 80) THEN
               ADD 12 TO WS-LNGDEB GIVING WS-POS-CHAINE
               STRING   REC-GEST-FILI(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-GEST-FILI(WS-POS-CHAINE:WS-LNGFIN)
               DELIMITED BY SIZE
               INTO REC-F-PRG
           ELSE 
               INITIALIZE REC-F-PRG
           END-IF.
           IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
               MOVE REC-GEST-FILI TO REC-F-PRG 
           END-IF.                 
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END.                       
       3035-GEST-FILI-END.
           EXIT.

       3035-GEST-FILO-START.
           INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG.
           INSPECT REC-GEST-FILO 
           TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':WS-FICNOM:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':WS-FICNOM:'.
           IF (WS-LNGDEB < 80) THEN
               ADD 12 TO WS-LNGDEB GIVING WS-POS-CHAINE
               STRING   REC-GEST-FILO(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-GEST-FILO(WS-POS-CHAINE:WS-LNGFIN)
               DELIMITED BY SIZE
               INTO REC-F-PRG
           ELSE 
               INITIALIZE REC-F-PRG
           END-IF.
           IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
               MOVE REC-GEST-FILO TO REC-F-PRG 
           END-IF.                 
           PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END.                       
       3035-GEST-FILO-END.
           EXIT.

       3035-FILE-STATUS-START. 
           PERFORM 2000-OPEN-FILE-STATUS-START 
                    THRU 2000-OPEN-FILE-STATUS-END.     
           PERFORM 2030-READ-FILE-STATUS-START 
                    THRU 2030-READ-FILE-STATUS-END.

           PERFORM UNTIL FILE-STATUS-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-FILE-STATUS 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMFILE:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMFILE:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 10 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-FILE-STATUS(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-FILE-STATUS(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-FILE-STATUS TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-STATUS-START 
                    THRU 2030-READ-FILE-STATUS-END
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-STATUS-START 
                    THRU 2020-CLOSE-FILE-STATUS-END.       
       3035-FILE-STATUS-END.
           EXIT.

       3035-FILEV-SECT-START.
           PERFORM 2000-OPEN-FILEV-SECTION-START 
                    THRU 2000-OPEN-FILEV-SECTION-END.
           PERFORM 2030-READ-FILEV-SECTION-START 
                    THRU 2030-READ-FILEV-SECTION-END.
           PERFORM UNTIL FILEV-SECT-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-FILEV-SECTION 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMFILE:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMFILE:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 10 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-FILEV-SECTION(1:WS-LNGDEB)
                         FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-FILEV-SECTION(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF      
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
              INSPECT REC-FILEV-SECTION 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NBCAR:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NBCAR:'         
              IF (WS-LNGDEB < 80) THEN
                 IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
      * il n'y a que nbcar sur cet enreg          
                    ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                    STRING   REC-FILEV-SECTION(1:WS-LNGDEB)
                             WS-FIC-ENREG (WS-IND) 
                             REC-FILEV-SECTION(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG
                 ELSE 
      * Enregistrement contient les 2 mots clé
                    INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
                    INSPECT REC-F-PRG 
                    TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NBCAR:' 
                              WS-LNGFIN FOR CHARACTERS AFTER ':NBCAR:' 
                    ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                    STRING   REC-F-PRG(1:WS-LNGDEB)
                             WS-FIC-ENREG (WS-IND) 
                             REC-F-PRG(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG               
                 END-IF
              END-IF
            
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-FILEV-SECTION TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILEV-SECTION-START 
                    THRU 2030-READ-FILEV-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-FILEV-SECTION-START 
                    THRU 2020-CLOSE-FILEV-SECTION-END.
       3035-FILEV-SECT-END.
           EXIT.

       3035-FILEF-SECT-START.
            PERFORM 2000-OPEN-FILEF-SECTION-START 
                    THRU 2000-OPEN-FILEF-SECTION-END.
           PERFORM 2030-READ-FILEF-SECTION-START 
                    THRU 2030-READ-FILEF-SECTION-END.
           PERFORM UNTIL FILEF-SECT-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-FILEF-SECTION 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NOMFILE:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NOMFILE:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 10 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-FILEF-SECTION(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-FILEF-SECTION(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
              INSPECT REC-FILEF-SECTION 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NBCAR:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':NBCAR:'                      
              IF (WS-LNGDEB < 80) THEN
                 IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
      * il n'y a que nbcar sur cet enreg          
                    ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                    STRING   REC-FILEF-SECTION(1:WS-LNGDEB)
                             WS-FIC-ENREG (WS-IND) 
                             REC-FILEF-SECTION(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG
                 ELSE  
                    INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN
      * Enregistrement contient les 2 mots clé
                    INSPECT REC-F-PRG 
                    TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':NBCAR:' 
                              WS-LNGFIN FOR CHARACTERS AFTER ':NBCAR:' 
                    ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                    STRING   REC-F-PRG(1:WS-LNGDEB)
                             WS-FIC-ENREG (WS-IND) 
                             REC-F-PRG(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG                                   
                 END-IF
              END-IF
            
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-FILEF-SECTION TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILEF-SECTION-START 
                    THRU 2030-READ-FILEF-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-FILEF-SECTION-START 
                    THRU 2020-CLOSE-FILEF-SECTION-END.      
       3035-FILEF-SECT-END.
           EXIT.

           COPY SCREEN-GEST.

           COPY TST-STATUT REPLACING ==:FNAME:== BY ==F-PRG==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==IDENT-DIV==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==ENV-DIV==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==DATA-DIV==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==PROC-DIV==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==WS-SECTION==.   
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==CALL-SSPRG==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILE-SECTION==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILEV-SECT==.               
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILEF-SECT==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILE-STATUS==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==GEST-FILI==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==GEST-FILO==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==IO-SECTION==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==TST-STAT==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILE-IO==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FILE-CONT==. 
