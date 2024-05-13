      ****************************************************************** 
      * GESTSQL0: Creation des tables SQL 
      *           Lecture du fichier input 
      *           Chargement des tables SQL
      *
      * Date de création : le 04/05/2024   
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestsql0.
       AUTHOR. Isabelle Marand.
       
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
       01  F-INPUT-STATUS       PIC X(02)      VALUE SPACE .
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  WS-IND-CO            PIC 99         VALUE 0 .
       01  WS-NOUV-COURS        PIC X(21)              .
       01  WS-COURS .
           03  WS-NB-COURS      PIC 99         VALUE 0 .
           03  WS-COURS-TAB  OCCURS 99 TIMES 
      *     03  WS-COURS-TAB  OCCURS 1 TO 99 
      *                       DEPENDING WS-NB-COURS
                             INDEXED BY IDX-COURS.
              05 WS-COURS-ID    PIC 99                 .
              05 WS-COURS-LIB   PIC X(21)              .

   

       
       01  WS-ID1               PIC 99      VALUE 0     .
       01  WS-ID2               PIC 99      VALUE 0     .

      * Gestion des erreurs
       01  WS-SQL-LIB           PIC X(80)               .
       01  WS-LIG-RAP           PIC X(150)              .

      * Déclaration des variables correspondant à sql 
       EXEC SQL BEGIN DECLARE SECTION END-EXEC          .
      * paramètres pour connexion à la base 
       01  DBNAME     PIC X(30)   VALUE'promosql'        .
       01  USERNAME   PIC X(30)   VALUE 'cobol'          .
       01  PASSWD     PIC X(10)   VALUE SPACE            .
      
      * description des champs utilisés pour le chargement SQL 
       01  SQL-STUDENT .
              05 SQL-ST-LASTNAME   PIC X(7)            .
              05 SQL-ST-FIRSTNAME  PIC X(6)            . 
              05 SQL-ST-AGE        PIC 99              .
       01  SQL-COURS .
              05 SQL-CO-LABEL      PIC X(21)            .
              05 SQL-CO-COEF       PIC 9V9              . 
       01  SQL-GRADE.
              05 SQL-GR-ST-ID      PIC 99               .
              05 SQL-GR-CO-ID      PIC 99               .
              05 SQL-GR-GRADE      PIC 99V99            .
              05 SQL-GR-COEF       PIC 9V9     VALUE 1  . 

      * variables de travail
       01  SQL-ID1              PIC 99      .
       01  SQL-ID2              PIC 99      .

       EXEC SQL END DECLARE SECTION END-EXEC. 
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       
       PROCEDURE DIVISION.
      *0000-Main-start
           PERFORM 1000-INITIALIZE-START THRU 1010-INITIALIZE-END.
           EXEC SQL 
              CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
               MOVE 'CONNECTION BASE' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
      * Je crée mes tables SQL     
           PERFORM 3000-CREATE-TAB-START THRU 3000-CREATE-TAB-END.
      * Je charge mes tables SQL avec les données de input     
           PERFORM 3010-CHARGE-TAB-START THRU 3010-CHARGE-TAB-END.
           
          
      *0000-main-end
       0010-STOP-PRG.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DISCONNECTION BASE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           STOP RUN.

       1000-INITIALIZE-START.
           INITIALIZE WS-NB-COURS     .
           INITIALIZE WS-ID1          .
           INITIALIZE WS-ID2          .
       1010-INITIALIZE-END.
           EXIT.

       2000-OPEN-FILE-START.
           OPEN INPUT F-INPUT         .
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.
       2000-OPEN-FILE-END. 
           EXIT.

       2010-READ-FILE-START.
           READ F-INPUT.
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.
       2010-READ-FILE-END.
           EXIT.

       2020-CLOSE-FILE-START.
           CLOSE F-INPUT.
           PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END.
       2020-CLOSE-FILE-END.  
           EXIT. 
       
       3000-CREATE-TAB-START.
           EXEC SQL 
              DROP TABLE IF EXISTS GRADE 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DROP TABLE GRADE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL 
              DROP TABLE IF EXISTS STUDENT 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DROP TABLE STUDENT' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL 
              DROP TABLE IF EXISTS COURSE 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DROP TABLE COURSE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL 
              CREATE TABLE STUDENT (
	             ST_ID SERIAL NOT NULL,
	             ST_LASTNAME CHAR(7) ,
	             ST_FIRSTNAME CHAR(6) ,
	             ST_AGE SMALLINT ,
                CONSTRAINT ID_STUDENT PRIMARY KEY (ST_ID)
                )
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'TABLE STUDENT CREATION' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           EXEC SQL 
              CREATE TABLE COURSE (
	             CO_ID SERIAL NOT NULL,
	             CO_LABEL CHAR(21) ,
	             CO_COEF DECIMAL ,
	             CONSTRAINT ID_COURSE PRIMARY KEY (CO_ID)
              )
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'TABLE COURS CREATION' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
  
           EXEC SQL 
              CREATE TABLE GRADE (
	             GR_ID SERIAL NOT NULL,
	             ST_ID SMALLINT NOT NULL,
	             CO_ID SMALLINT NOT NULL,
	             GR_GRADE DECIMAL ,
	             GR_COEF DECIMAL NOT NULL DEFAULT 1,
	             CONSTRAINT ID_GRADE PRIMARY KEY (GR_ID),
	             FOREIGN KEY (ST_ID) REFERENCES STUDENT(ST_ID),
	             FOREIGN KEY (CO_ID) REFERENCES COURSE(CO_ID)
              )
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'TABLE GRADE CREATION' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
       3000-CREATE-TAB-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXIT.
       
       3010-CHARGE-TAB-START.
           PERFORM 2000-OPEN-FILE-START THRU 2000-OPEN-FILE-END.
           PERFORM 2010-READ-FILE-START THRU 2010-READ-FILE-END.
           PERFORM UNTIL (F-INPUT-STATUS-EOF) 
               EVALUATE (REC-F-INPUT-2)
                    WHEN '01'
                          PERFORM 5000-CHARGE-ELEVE-START
                             THRU 5000-CHARGE-ELEVE-END
                    WHEN '02' 
                          PERFORM 5010-CHARGE-COURS-START
                             THRU 5010-CHARGE-COURS-END
                    WHEN OTHER 
                       DISPLAY 'Le type d''enregistrement' SPACE 
                       REC-F-INPUT-2 SPACE 'n''est pas géré.'
                       'Arrêt du programme!'
                       PERFORM 2020-CLOSE-FILE-START
                                THRU 2020-CLOSE-FILE-END
                       GO TO 0010-STOP-PRG
               END-EVALUATE
               PERFORM 2010-READ-FILE-START THRU 2010-READ-FILE-END 
           END-PERFORM.
           
           PERFORM 2020-CLOSE-FILE-START THRU 2020-CLOSE-FILE-END.
 
       3010-CHARGE-TAB-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXIT.   
       
       5000-CHARGE-ELEVE-START.
           MOVE R-LASTNAME TO SQL-ST-LASTNAME.
           MOVE R-FIRSTNAME TO SQL-ST-FIRSTNAME.
           MOVE R-AGE TO SQL-ST-AGE.
           EXEC SQL
              INSERT INTO STUDENT
              (ST_LASTNAME, ST_FIRSTNAME,ST_AGE)
              VALUES
              ( :SQL-ST-LASTNAME, :SQL-ST-FIRSTNAME, :SQL-ST-AGE)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
               MOVE 'RECORD STUDENT WRITE' 
                       TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL 
              SELECT ST_ID AS SQL_ID1 INTO :SQL-ID1
              FROM STUDENT 
              WHERE (ST_LASTNAME = :SQL-ST-LASTNAME 
                    AND ST_FIRSTNAME = :SQL-ST-FIRSTNAME)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
               MOVE 'RECUPERATION DE l''ID DE L''ELEVE' 
                       TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           MOVE SQL-ID1 TO WS-ID1.
            
       5000-CHARGE-ELEVE-END.
           EXIT.

       5010-CHARGE-COURS-START.
           MOVE R-LABEL      TO    SQL-CO-LABEL
      * Je cherche dans ma table si les infos du cours sont déjà chargées 
           SET IDX-COURS TO 1.
           SEARCH  WS-COURS-TAB 
           AT END
      * Si pas trouvé je crée l'enregistrement dans la table
      * puis je charge la table cours SQL
              SET WS-NB-COURS UP BY 1
              MOVE R-LABEL      TO    WS-COURS-LIB(WS-NB-COURS)   
              MOVE R-COEF       TO    SQL-CO-COEF

              EXEC SQL
                 INSERT INTO COURSE
                 (CO_LABEL, CO_COEF)
                 VALUES
                 (:SQL-CO-LABEL, :SQL-CO-COEF)
              END-EXEC
              IF  SQLCODE NOT = ZERO 
                 MOVE 'RECORD COURS WRITE' 
                       TO WS-SQL-LIB
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF
              EXEC SQL COMMIT WORK END-EXEC
           WHEN WS-COURS-LIB (IDX-COURS) = R-LABEL 
              INITIALIZE WS-ID2        
           END-SEARCH.

      * Je charge la table classe SQL 
           EXEC SQL 
              SELECT CO_ID AS SQL_ID2 INTO :SQL-ID2
              FROM COURSE 
              WHERE (CO_LABEL = :SQL-CO-LABEL)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
               MOVE 'RECUPERATION DE l''ID DU COURS' 
                       TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.

           MOVE SQL-ID2 TO WS-ID2.
 
           PERFORM 5020-CHARGE-CLASSE-START 
                       THRU 5020-CHARGE-CLASSE-END.                                                    
       5010-CHARGE-COURS-END.
           EXIT.

       5020-CHARGE-CLASSE-START.
           MOVE R-GRADE   TO SQL-GR-GRADE.
           MOVE WS-ID1    TO SQL-GR-ST-ID.
           MOVE WS-ID2    TO SQL-GR-CO-ID.

           EXEC SQL
                 INSERT INTO GRADE
                 ( ST_ID, CO_ID, GR_GRADE)
                 VALUES
                 ( :SQL-GR-ST-ID, :SQL-GR-CO-ID, :SQL-GR-GRADE)
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
                 MOVE 'RECORD GRADE WRITE' 
                       TO WS-SQL-LIB
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
       5020-CHARGE-CLASSE-END.
           EXIT.

       9000-TEST-STATUT-START.
           IF (NOT F-INPUT-STATUS-OK) 
              AND (NOT F-INPUT-STATUS-EOF) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE F-INPUT-STATUS 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              STOP RUN 
           END-IF. 
       9000-TEST-STATUT-END. 
      
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
             
       