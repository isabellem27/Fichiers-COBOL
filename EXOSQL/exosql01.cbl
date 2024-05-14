      ****************************************************************** 
      * EXOSQL01: Déclarations sql pour récupérer les informations     *
      *           suivantes: L’âge maximum, L’âge minimum,             *
      *           Le nombre d’individus par âge                        *
      *           le nom, prénom, email et citation pour les individus *
      *           de Belgique                                          *
      *           Affichage à l'écran des résultats                    *   
      *                                                                *
      * Date de création : le 10/05/2024                               * 
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. exosql01.
       AUTHOR. Isabelle Marand.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS. 

       DATA DIVISION.
       FILE SECTION.
       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT        PIC X(250).

       WORKING-STORAGE SECTION.
       01  F-OUTPUT-STATUS      PIC X(02)   VALUE SPACE .
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

      * Variables internes 
       01  WS-SQL-LIB     PIC X(80)   VALUE SPACES   .
       01  FIN            PIC S9(9)   VALUE 100      . 
       01  WS-POURCENT    PIC 99V99   VALUE 0        .            

      * variables pour le dialogue console
       01  WS-LIG-MIN     PIC X(24)   VALUE 'LE PLUS JEUNE AGE EST: '  .  
       01  WS-LIG-MAX     PIC X(24)   VALUE 'LE PLUS GRAND AGE EST: '  .
       01  WS-LIG-CPTAGE  PIC X(35)   VALUE 'INDIVIDUS PAR AGE'        .
       01  WS-LIG-CPTAGE-ENT.
           05 WS-LIG-AGE        PIC X(6)    VALUE 'AGE'          .
           05 FILLER            PIC X       VALUE SPACE          .
           05 WS-LIG-NBAGE      PIC X(50)   VALUE 'NB INDIVIDUS' .
       01  WS-LIG-AN            PIC X(5)    VALUE ' ans '  . 
       01  WS-LIG-NBAGEZ        PIC Z(4)9                  .  
       01  WS-LIG-NBLIGZ        PIC ZZ9                    .            
       01  WS-LIG-NBLIG         PIC X(30)   VALUE
           'NB de lignes mises à jour:'                    . 
       01  WS-LIG-BEFR          PIC X(50)   VALUE
           'Bascule des Belges vers France:'               .       
       01  WS-LIG-UCASE         PIC X(25)   VALUE
           'Mise en majuscules:'                           .
       01  WS-LIG-CORR          PIC X(50)   VALUE
           'Correction des codes pays:'                    .

       01  WS-LIG-BELGE   PIC X(45)   VALUE
           'LISTE DES INDIVIDUS RATTACHES A LA BELGIQUE'   . 
       01  WS-LIG-BELGE-ENT1.
           05 WS-LIG-ID         PIC X(40)   VALUE 'ID'     .
           05 FILLER            PIC X       VALUE SPACE    .
           05 WS-LIG-LASTNAME   PIC X(50)   VALUE 'NOM'    .
           05 FILLER            PIC X       VALUE SPACE    .
           05 WS-LIG-FIRSTNAME  PIC X(50)   VALUE 'PRENOM' .
           05 FILLER            PIC X       VALUE SPACE    . 
       01  WS-LIG-BELGE-ENT2.
           05 WS-LIG-EMAIL      PIC X(50)   VALUE 'EMAIL'  .
           05 FILLER            PIC X       VALUE SPACE    .       
           05 WS-LIG-PHRASE     PIC X(50)   VALUE 'PHRASE NATIONALE'.
       01  WS-LIG-BLC           PIC X(10)   VALUE SPACES   .  
       
       01  WS-LIG-TITRE         PIC X(50)   VALUE 
           'ETUDE DE LA POPULATION'                           .
       01  WS-LIG-AGE-ENT.
           05 WS-LIG-ENT-MIN    PIC X(7)    VALUE 'AGE MIN'   .
           05 FILLER            PIC X       VALUE SPACE       .
           05 WS-LIG-ENT-MAX    PIC X(7)    VALUE 'AGE MAX'   .
           05 FILLER            PIC X       VALUE SPACE       .
           05 WS-LIG-ENT-MED    PIC X(10)   VALUE 'AGE MEDIAN'.

       01  WS-LIG-GENDER-ENT.
           05 WS-LIG-COUNTRY    PIC X(20)  VALUE 'COUNTRY'    .
           05 FILLER            PIC X      VALUE SPACE        .
           05 WS-LIG-GENDER     PIC X(15)  VALUE 'GENDER'     .
           05 FILLER            PIC X      VALUE SPACE        .
           05 WS-LIG-POURCENT   PIC X(20)  VALUE '% GENDER IN COUNTRY'. 

       01  WS-LIG-GENDER-DET.
           05 WS-LIGD-COUNTRY   PIC X(20)                .
           05 FILLER            PIC X      VALUE SPACE   .
           05 WS-LIGD-GENDER    PIC X(15)                .
           05 FILLER            PIC X      VALUE SPACE   .
           05 WS-LIGD-POURCENT  PIC 99.99                . 

       01  WS-LIG-FIN           PIC X(50)  VALUE 
           'FIN DU RAPPORT'                              .

      * Déclaration des variables correspondant à sql 
       EXEC SQL BEGIN DECLARE SECTION END-EXEC             .
      * paramètres pour connexion à la base 
       01  DBNAME     PIC X(30)    VALUE 'dgse'            .
       01  USERNAME   PIC X(30)    VALUE 'cobol'           .
       01  PASSWD     PIC X(10)    VALUE  SPACE            .
      * 01  PASSWD     PIC X(10)    VALUE 'cbl85'           .

      * description des enregistrements attendus pour chaque curseur
       01  SQL-CPTAGE.
              05 SQL-CPT-ID        PIC 9(5)   .
              05 SQL-AGE           PIC 99     .

       01  SQL-BELGE.
              05 SQL-BE-ID         PIC X(40)  .
              05 SQL-BE-LASTNAME   PIC X(50)  .
              05 SQL-BE-FIRSTNAME  PIC X(50)  .        
              05 SQL-BE-EMAIL      PIC X(50)  .       
              05 SQL-BE-PHRASE     PIC X(50)  .  

       01  SQL-CURSEUR-COUNTRY.
              05 SQL-COUNTRY       PIC X(50)  .    
              05 SQL-GENDER        PIC X(50)  .
              05 SQL-NBPEOPLE      PIC 9(4)   .
              05 SQL-NBBYGENDER    PIC 9(4)   .

      * variables de travail
       01  SQL-AGE-MIN         PIC 99     VALUE 0.
       01  SQL-AGE-MAX         PIC 99     VALUE 0.
       01  SQL-AGE-MEDIAN      PIC 99     VALUE 0.
       01  SQL-NB-LIG          PIC 9(3)   VALUE 0. 

       EXEC SQL END DECLARE SECTION END-EXEC. 
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
      *0000-Main-start
           
           EXEC SQL 
              CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
               MOVE 'CONNECTION BASE' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

      *    Je m'occupe des informations portant sur l'âge
           PERFORM 1010-PARTIE1-START THRU 1010-PARTIE1-END.

      *    Je m'occupe des renseignements sur les belges
           PERFORM 1020-PARTIE2-START THRU 1020-PARTIE2-END.

      *    Je met à jour des données
           PERFORM 1030-PARTIE3-START THRU 1030-PARTIE3-END.

      *    J'écrit mon rapport
           PERFORM 1040-PARTIE4-START THRU 1040-PARTIE4-END.     

      *0000-main-end
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DISCONNECTION BASE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           STOP RUN.


       1010-PARTIE1-START.
           EXEC SQL 
              SELECT MIN(AGE) INTO :SQL-AGE-MIN FROM DATABANK
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               MOVE 'AGE MIN' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           DISPLAY WS-LIG-MIN SPACE SQL-AGE-MIN .

           EXEC SQL 
              SELECT MAX(AGE) INTO :SQL-AGE-MAX FROM DATABANK
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               MOVE 'AGE MAX' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           DISPLAY WS-LIG-MAX SPACE SQL-AGE-MAX .
           
           INITIALIZE SQLCODE.
           DISPLAY WS-LIG-CPTAGE .
           DISPLAY WS-LIG-CPTAGE-ENT .
           EXEC SQL 
              DECLARE CRCPTAGE CURSOR FOR
                 SELECT  AGE, COUNT(*) AS NBAGE FROM DATABANK
                 GROUP BY AGE 
                 ORDER BY NBAGE DESC 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION CPTAGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              OPEN CRCPTAGE
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE CPTAGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           EXEC SQL 
              FETCH CRCPTAGE 
              INTO  :SQL-AGE, :SQL-CPT-ID
           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
              MOVE 'LECTURE 1 CPTAGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
            
           PERFORM UNTIL (SQLCODE = FIN)
              MOVE SQL-CPT-ID TO WS-LIG-NBAGEZ
              DISPLAY  SQL-AGE WS-LIG-AN WS-LIG-NBAGEZ 

              EXEC SQL 
                 FETCH CRCPTAGE
                 INTO  :SQL-AGE, :SQL-CPT-ID
              END-EXEC
              IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
                 MOVE 'LECTURE SUIVANTE CPTAGE' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF   
           END-PERFORM.
       1010-PARTIE1-END.
           EXEC SQL
              CLOSE CRCPTAGE
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE CPTAGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXIT.   

       1020-PARTIE2-START.
           DISPLAY WS-LIG-BELGE. 
           DISPLAY WS-LIG-BELGE-ENT1.
           DISPLAY WS-LIG-BLC WS-LIG-BELGE-ENT2.
           EXEC SQL 
              DECLARE CRBELGE CURSOR FOR
                 SELECT  d.ID, d.LAST_NAME, d.FIRST_NAME, d.EMAIL,
                       p.PHRASE
                 FROM DATABANK d INNER JOIN PHRASE p
                 ON d.COUNTRY_CODE = p.COUNTRY_CODE  
                 WHERE d.COUNTRY_CODE = 'BE'
                 ORDER BY LAST_NAME, FIRST_NAME 
    
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              OPEN CRBELGE
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           EXEC SQL 
              FETCH CRBELGE 
              INTO :SQL-BE-ID, :SQL-BE-LASTNAME, :SQL-BE-FIRSTNAME,
                   :SQL-BE-EMAIL, :SQL-BE-PHRASE
           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
              MOVE 'LECTURE 1 BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           PERFORM UNTIL (SQLCODE = FIN)

              DISPLAY  SQL-BE-ID SPACE SQL-BE-LASTNAME SPACE 
                       SQL-BE-FIRSTNAME  
              DISPLAY  WS-LIG-BLC SQL-BE-EMAIL SPACE 
                       SQL-BE-PHRASE

              EXEC SQL 
                 FETCH CRBELGE
                 INTO  :SQL-BE-ID, :SQL-BE-LASTNAME, :SQL-BE-FIRSTNAME,
                       :SQL-BE-EMAIL, :SQL-BE-PHRASE
              END-EXEC
              IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
                 MOVE 'LECTURE SUIVANTE BELGE' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF   
           END-PERFORM.
       1020-PARTIE2-END.
           EXEC SQL
              CLOSE CRBELGE
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXIT.

       1030-PARTIE3-START.
           EXEC SQL 
              SELECT COUNT(*) INTO :SQL-NB-LIG FROM DATABANK
              WHERE COUNTRY_CODE = 'BE'
              AND AGE BETWEEN 36 AND 39
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'COUNT BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           EXEC SQL 
              UPDATE DATABANK SET COUNTRY_CODE = 'FR'
              WHERE COUNTRY_CODE = 'BE'
              AND AGE BETWEEN 36 AND 39
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'UPDATE BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXEC SQL COMMIT END-EXEC.
           MOVE SQL-NB-LIG TO WS-LIG-NBLIGZ.
           DISPLAY WS-LIG-BEFR.
           DISPLAY WS-LIG-NBLIG SPACE WS-LIG-NBLIGZ.

           EXEC SQL 
              SELECT COUNT(*) INTO :SQL-NB-LIG FROM DATABANK
              WHERE COUNTRY_CODE||COUNTRY != 'BEBelgium'
              AND COUNTRY_CODE||COUNTRY != 'FRFrance'
              AND COUNTRY_CODE||COUNTRY != 'CHSwitzerland'
              AND COUNTRY_CODE||COUNTRY != 'LULuxembourg'
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'COUNT CORR' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              UPDATE DATABANK   SET COUNTRY_CODE = 'BE'              
              WHERE COUNTRY_CODE||COUNTRY != 'BEBelgium'
              AND COUNTRY_CODE||COUNTRY != 'FRFrance'
              AND COUNTRY_CODE||COUNTRY != 'CHSwitzerland'
              AND COUNTRY_CODE||COUNTRY != 'LULuxembourg'
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'UPDATE BELGE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXEC SQL COMMIT END-EXEC.
           MOVE SQL-NB-LIG TO WS-LIG-NBLIGZ.
           DISPLAY WS-LIG-CORR.
           DISPLAY WS-LIG-NBLIG SPACE WS-LIG-NBLIGZ.

           EXEC SQL 
              SELECT COUNT(*) INTO :SQL-NB-LIG FROM DATABANK
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'COUNT CORR' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL 
              UPDATE DATABANK 
              SET SPOKEN = UPPER(SPOKEN)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'UPDATE SPOKEN' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXEC SQL COMMIT END-EXEC.
           MOVE SQL-NB-LIG TO WS-LIG-NBLIGZ.
           DISPLAY WS-LIG-UCASE SPACE 'CHAMP SPOKEN'.
           DISPLAY WS-LIG-NBLIG SPACE WS-LIG-NBLIGZ.

           EXEC SQL 
              UPDATE DATABANK 
              SET COUNTRY = UPPER(COUNTRY)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'UPDATE COUTRY' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           EXEC SQL COMMIT END-EXEC.
           DISPLAY WS-LIG-UCASE SPACE 'CHAMP COUNTRY'.
           DISPLAY WS-LIG-NBLIG SPACE WS-LIG-NBLIGZ.
       1030-PARTIE3-END.
           EXIT.
       
       1040-PARTIE4-START.
           EXEC SQL 
              DECLARE  CRGENDER CURSOR FOR
                 SELECT d1.COUNTRY, d1.GENDER, d.NBPEOPLE, 
                        COUNT(*) AS NBBYGENDER
	              FROM DATABANK d1
                 JOIN (
                       SELECT COUNTRY, COUNT(*) as NBPEOPLE
                       FROM DATABANK 
                       WHERE GENDER IS NOT NULL
                       GROUP BY COUNTRY
                     ) AS d
                ON d.COUNTRY = d1.COUNTRY     
	             WHERE d1.GENDER IS NOT NULL
	             GROUP BY d1.COUNTRY, d1.GENDER,d.NBPEOPLE
                ORDER BY d1.COUNTRY,d1.GENDER
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION GENDER' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           EXEC SQL
              SELECT MIN(AGE), MAX(AGE), 
              PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY AGE) AS MEDIAN
              INTO :SQL-AGE-MIN, :SQL-AGE-MAX, :SQL-AGE-MEDIAN
              FROM DATABANK
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'PARTIE4-MINMAX' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.   

           OPEN OUTPUT F-OUTPUT.
           
           INITIALIZE REC-F-OUTPUT.
           STRING 
                 WS-LIG-BLC WS-LIG-BLC WS-LIG-BLC WS-LIG-BLC
                 WS-LIG-TITRE
           DELIMITED BY SIZE        
           INTO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Ecriture du calcul des min max et médian des ages     
           MOVE WS-LIG-AGE-ENT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           STRING 
                 SQL-AGE-MIN WS-LIG-AN SPACE SQL-AGE-MAX WS-LIG-AN 
                 SPACE SQL-AGE-MEDIAN WS-LIG-AN
           DELIMITED BY SIZE        
           INTO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Ecriture du résultat de crgender
           MOVE WS-LIG-GENDER-ENT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           EXEC SQL
              OPEN CRGENDER
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE GENDER' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           PERFORM UNTIL (SQLCODE = FIN)
              EXEC SQL 
                 FETCH CRGENDER
                 INTO :SQL-COUNTRY, :SQL-GENDER, :SQL-NBPEOPLE,
                 :SQL-NBBYGENDER
              END-EXEC
              IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
                 MOVE 'LECTURE GENDER' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF  
              INITIALIZE REC-F-OUTPUT
              MOVE SQL-COUNTRY  TO WS-LIGD-COUNTRY
              MOVE SQL-GENDER   TO WS-LIGD-GENDER
              COMPUTE WS-POURCENT ROUNDED = 
                     (SQL-NBBYGENDER / SQL-NBPEOPLE) *100
              MOVE WS-POURCENT TO WS-LIGD-POURCENT
              MOVE WS-LIG-GENDER-DET TO REC-F-OUTPUT 
              WRITE REC-F-OUTPUT    
           END-PERFORM.
           INITIALIZE REC-F-OUTPUT.
           WRITE REC-F-OUTPUT .
           STRING 
                 WS-LIG-BLC WS-LIG-BLC WS-LIG-BLC WS-LIG-BLC
                 WS-LIG-FIN              
           DELIMITED BY SIZE
           INTO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT .
       1040-PARTIE4-END.
           EXEC SQL
              CLOSE CRGENDER
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE GENDER' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
           CLOSE F-OUTPUT.
           EXIT.
       


       9050-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY WS-SQL-LIB SPACE "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       9050-ERROR-RTN-END.
           STOP RUN.      
       
