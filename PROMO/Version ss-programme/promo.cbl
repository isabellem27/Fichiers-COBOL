      ****************************************************************** 
      * Lecture du fichier input afin de générer un bulletin
      * avec les moyennes par classe, par élève, par matière
      * Gestion du coefficient par matière.
      *Les enregistrements sont typés:
      * 01 = description de l'élève
      * 02 = note et coefficient par matière
      *
      * Date de création : le 28/04/2024   
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. promo.
       AUTHOR. CobolP3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.            

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2       PIC 9(02).

       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       
           66 R-NOM RENAMES R-LASTNAME THRU R-FIRSTNAME.       

       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC X(3).       
           03 R-GRADE          PIC X(5).       

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS       PIC X(02)   VALUE SPACE .
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  WS-IND-CO            PIC 99      VALUE 0     .
       01  WS-NOUV-COURS        PIC X(21)               .
       01  WS-COURS .
           03  WS-NB-COURS          PIC 99      VALUE 0     .
           03  WS-COURS-TAB  OCCURS 1 TO 99 
                             DEPENDING WS-NB-COURS
                             INDEXED BY IDX-COURS.
              05 WS-COURS-ID       PIC 99                  .
              05 WS-COURS-LIB      PIC X(21)               .
              05 WS-COURS-COEF     PIC 9V9. 
              05 WS-COURS-MOY      PIC 9(3)V99   VALUE 0   .
              05 WS-COURS-DIV      PIC 99        VALUE 0   .   


       01  WS-IND-EL            PIC 99      VALUE 0     .
       01  WS-NOUV-ELEVE        PIC 9       VALUE 0     .
       01  WS-ELEVE .
           03  WS-NB-ELEVE          PIC 99      VALUE 0 .
           03  WS-ELEVE-TAB  OCCURS 1 TO 99 
                             DEPENDING WS-NB-ELEVE
                             INDEXED BY IDX-ELEVE    .
              05 WS-ELEVE-ID       PIC 99               .
              05 WS-ELEVE-NOM      PIC X(13)            . 
              05 WS-ELEVE-AGE      PIC 99               .
              05 WS-ELEVE-MOY      PIC 999V99           .
       
    
       01  WS-IND-CL            PIC 99      VALUE 0     .
       01  WS-ID1               PIC 99      VALUE 0     .
       01  WS-ID2               PIC 99      VALUE 0     .
       
       01  WS-CLASSE.
           03  WS-NB-ENREG          PIC 999     VALUE 0     .
           03  WS-CLASSE-TAB    OCCURS 1 TO 999 
                                DEPENDING   WS-NB-ENREG
                                ASCENDING KEY WS-CLASSE-ID1 
                                            WS-CLASSE-ID2
                                INDEXED BY IDX-CLASSE.
              05 WS-CLASSE-ID1     PIC 99      .
              05 WS-CLASSE-ID2     PIC 99      .
              05 WS-CLASSE-NOTE    PIC 99V99.

       01  WS-DIVCLASSE         PIC 99V99    VALUE 0 .
       01  WS-DIVELEVE          PIC 99V99    VALUE 0 .
       01  WS-LIG-RAP           PIC X(250)           .           
     
       01  WS-CLASSE-MOY        PIC 999V99   VALUE 0    .

       PROCEDURE DIVISION.
      *0000-Main-start
           PERFORM 1000-INITIALIZE-START THRU 1010-INITIALIZE-END.
           PERFORM 2000-OPEN-FILE-START THRU 2010-OPEN-FILE-END.
           PERFORM 2020-READ-FILE-START THRU 2030-READ-FILE-END.         

           PERFORM UNTIL (F-INPUT-STATUS-EOF) 
               EVALUATE (REC-F-INPUT-2)
                    WHEN '01'
                          PERFORM 5000-CHARGE-ELEVE-START
                             THRU 5010-CHARGE-ELEVE-END
                    WHEN '02' 
                          PERFORM 5020-CHARGE-COURS-START
                             THRU 5025-CHARGE-COURS-END
                    WHEN OTHER 
                       DISPLAY 'Le type d''enregistrement' SPACE 
                       REC-F-INPUT-2 SPACE 'n''est pas géré.'
                       'Arrêt du programme!'
                       PERFORM 2050-CLOSE-FILE-START
                                THRU 2060-CLOSE-FILE-END
                       GO TO 0010-STOP-PRG
               END-EVALUATE
               PERFORM 2020-READ-FILE-START THRU 2030-READ-FILE-END 
           END-PERFORM.
           
           PERFORM 2050-CLOSE-FILE-START THRU 2060-CLOSE-FILE-END.
           PERFORM 6000-CALC-AVG-START   THRU 6010-CALC-AVG-END.

      * environnement d'exécution/compilation UNIX: j'utilise les cotes
      * pour designer un élément de la bibliotheque.
      * environnement mainframe avec jcl le nom logique suffit donc pas
      * de cotes. 
           CALL 'ecritfo'  USING BY REFERENCE WS-ELEVE
                               BY REFERENCE WS-COURS 
                               BY REFERENCE WS-CLASSE
                               BY REFERENCE WS-CLASSE-MOY
           EXCEPTION
              DISPLAY 'PB lors de l''appel de ECRIT-FO' SPACE 
                       'ARRÊT DU PROGRAMME'
              GO TO 0010-STOP-PRG.
          
      *0000-main-end
       0010-STOP-PRG.
           STOP RUN.

       1000-INITIALIZE-START.
           INITIALIZE WS-NOUV-COURS   .
           INITIALIZE WS-NOUV-ELEVE   .
           INITIALIZE WS-NB-COURS     .
           INITIALIZE WS-NB-ELEVE     .
           INITIALIZE WS-ID1          .
           INITIALIZE WS-ID2          .
           INITIALIZE WS-NB-ENREG     .
       1010-INITIALIZE-END.
       
       2000-OPEN-FILE-START.
           OPEN INPUT F-INPUT         .
           PERFORM 9000-TEST-STATUT-START THRU 9010-TEST-STATUT-END.
       2010-OPEN-FILE-END. 

       2020-READ-FILE-START.
           READ F-INPUT.
           PERFORM 9000-TEST-STATUT-START THRU 9010-TEST-STATUT-END.
       2030-READ-FILE-END.

       2050-CLOSE-FILE-START.
           CLOSE F-INPUT.
           PERFORM 9000-TEST-STATUT-START THRU 9010-TEST-STATUT-END.
       2060-CLOSE-FILE-END.

       5000-CHARGE-ELEVE-START.
           SET WS-NB-ELEVE UP BY 1.
           STRING WS-NB-ELEVE R-NOM R-AGE 
           DELIMITED BY SIZE 
           INTO WS-ELEVE-TAB(WS-NB-ELEVE).
           MOVE WS-NB-ELEVE TO WS-ID1.
       5010-CHARGE-ELEVE-END.

       5020-CHARGE-COURS-START.
      * Je cherche dans ma table si les infos du cours sont déjà chargées 
           SET IDX-COURS TO 1.
           SEARCH  WS-COURS-TAB 
           AT END
      * Si pas trouvé je crée l'enregistrement dans la table
      * puis je charge la table classe
              
              SET WS-NB-COURS UP BY 1
              MOVE WS-NB-COURS TO WS-COURS-ID(WS-NB-COURS)
              MOVE R-LABEL TO WS-COURS-LIB(WS-NB-COURS) 
              MOVE R-COEF TO  WS-COURS-COEF(WS-NB-COURS)  

              MOVE WS-NB-COURS TO WS-ID2
              PERFORM 5060-CHARGE-CLASSE-START 
                       THRU 5065-CHARGE-CLASSE-END
      * Sinon je vais charger la table classe
           WHEN WS-COURS-LIB (IDX-COURS) = R-LABEL
               
              MOVE WS-COURS-ID(IDX-COURS) TO WS-ID2 
              PERFORM 5060-CHARGE-CLASSE-START
                 THRU 5065-CHARGE-CLASSE-END

           END-SEARCH.                                                  
       5025-CHARGE-COURS-END.
       
       5060-CHARGE-CLASSE-START.
      * chargement linéaire donc je peux créer direct la ligne dans la table
           SET WS-NB-ENREG UP BY 1.
           MOVE WS-ID1 TO WS-CLASSE-ID1(WS-NB-ENREG).
           MOVE WS-ID2 TO WS-CLASSE-ID2(WS-NB-ENREG).
           MOVE R-GRADE TO WS-CLASSE-NOTE(WS-NB-ENREG).

       5065-CHARGE-CLASSE-END.

       5070-PREPA-AVG-START.
      * Je cherche l'élève dans ma table classe 
           SET IDX-CLASSE TO 1.
           SEARCH WS-CLASSE-TAB
           WHEN WS-CLASSE-ID1(IDX-CLASSE) = WS-ID1
              MOVE IDX-CLASSE TO WS-IND-CL 
           END-SEARCH.   
      * Pour chaque note trouvée, je recherche dans la table
      * cours le cours concerné. 
      * pour récupérer le coeff et charger les variables pour avg
           PERFORM VARYING WS-IND-CL FROM WS-IND-CL BY 1  
              UNTIL (WS-IND-CL > WS-NB-ENREG) 
                 OR (WS-CLASSE-ID1(WS-IND-CL) NOT EQUAL WS-ID1)
                 
                 PERFORM 5080-PREPA-C-AVG-START 
                       THRU 5085-PREPA-C-AVG-END
           END-PERFORM.
       5075-PREPA-AVG-END.

       5080-PREPA-C-AVG-START.
           SET IDX-COURS TO 1.          
           SEARCH WS-COURS-TAB
              AT END 
                 DISPLAY 'Cours non trouvé'  
              WHEN (WS-CLASSE-ID2(WS-IND-CL) = WS-COURS-ID(IDX-COURS))
      *    Prépare les variables qui seront utilisée pour le 
      *    calcul de la moyenne élève 
           
                 COMPUTE WS-ELEVE-MOY(WS-ID1) =
                          WS-ELEVE-MOY(WS-ID1) + 
                          ( WS-CLASSE-NOTE(WS-IND-CL) 
                                * WS-COURS-COEF(IDX-COURS)) 
                 SET WS-DIVELEVE UP BY WS-COURS-COEF(IDX-COURS)

      *    Comme je suis sur le cours, j'en profite pour préparer les
      *    variables qui seront utilisées pour le calcul de la 
      *    moyenne du cours
                 ADD 1 TO WS-COURS-DIV(IDX-COURS)
                 COMPUTE WS-COURS-MOY(IDX-COURS) 
                            = WS-COURS-MOY(IDX-COURS) 
                             + WS-CLASSE-NOTE(WS-IND-CL)              
           END-SEARCH. 
       5085-PREPA-C-AVG-END.

       6000-CALC-AVG-START.
      * Je boucle sur ma table élève pour faire les calculs 
      * de moyenne par élève et préparer les autres calculs
       
           INITIALIZE WS-ID1.
           PERFORM VARYING WS-IND-EL FROM 1 BY 1
              UNTIL (WS-IND-EL > WS-NB-ELEVE)
           
                 INITIALIZE WS-DIVELEVE
                 SET WS-ID1 TO WS-ELEVE-ID (WS-IND-EL) 
      * Calcul et charge somme des notes et diviseurs           
                 PERFORM 5070-PREPA-AVG-START THRU 5075-PREPA-AVG-END
      * La moyenne de chaque élève 
                 PERFORM 6060-CALC-ELEVE-AVG-START
                    THRU   6070-CALC-ELEVE-AVG-END

                  
           END-PERFORM. 

      * La moyenne pour chaque cours 
           PERFORM 6020-CALC-COURS-AVG-START
                 THRU   6030-CALC-COURS-AVG-END. 
           
      * Moyenne de la classe    
           PERFORM 6040-CALC-CLASSE-AVG-START
                 THRU   6050-CALC-CLASSE-AVG-END.
       6010-CALC-AVG-END.

       6020-CALC-COURS-AVG-START.
           PERFORM VARYING WS-IND-CO FROM 1 BY 1
                 UNTIL (WS-IND-CO > WS-NB-COURS)

                 COMPUTE WS-COURS-MOY(WS-IND-CO) ROUNDED =
                    WS-COURS-MOY(WS-IND-CO) / WS-COURS-DIV(WS-IND-CO) 
              
      * Je charge mes variables de la classe avec les informations
      * du cours
                 COMPUTE WS-CLASSE-MOY = WS-CLASSE-MOY +
                 (WS-COURS-MOY(WS-IND-CO) * WS-COURS-COEF(WS-IND-CO))
                 SET WS-DIVCLASSE UP BY WS-COURS-COEF(WS-IND-CO)           
           END-PERFORM.
       6030-CALC-COURS-AVG-END.

       6040-CALC-CLASSE-AVG-START.

           COMPUTE WS-CLASSE-MOY ROUNDED = WS-CLASSE-MOY / WS-DIVCLASSE.
       6050-CALC-CLASSE-AVG-END.

       6060-CALC-ELEVE-AVG-START.
           COMPUTE WS-ELEVE-MOY(WS-ID1) ROUNDED  =
                    WS-ELEVE-MOY(WS-ID1) / WS-DIVELEVE.
       6070-CALC-ELEVE-AVG-END.

      *COPY TST-STATUT REPLACING ==:FNAME:== BY ==F-INPUT==.
       9000-TEST-STATUT-START.
           IF (NOT F-INPUT-STATUS-OK) 
              AND (NOT F-INPUT-STATUS-EOF) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE F-INPUT-STATUS 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              GO TO 0010-STOP-PRG 
           END-IF. 
       9010-TEST-STATUT-END.       
             
                 
   
