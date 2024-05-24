      *=============================================================*
      *    Vos commentaires                                         *
      *                                                             *
      *                                                             *
      *                                                             *
      *                                                             *
      *    auteur : MOI.                           *
      *    Date creation 24/05/2024                                 *
      *=============================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BONOBO01.
       AUTHOR . MOI.

       ENVIRONMENT DIVISION.
      * CONFIGURATION SECTION.
      * SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA .
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FICI
           ASSIGN TO 'FICI.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FICI-STATUS.

           SELECT FICO
           ASSIGN TO 'FICO.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FICO-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD  FICI
           RECORD CONTAINS 056 CHARACTERS
           RECORDING MODE IS F.
       01  REC-FICI        PIC X(056) .
       FD  FICO
           RECORD CONTAINS 080 CHARACTERS
           RECORDING MODE IS F.
       01  REC-FICO        PIC X(080) .

       WORKING-STORAGE SECTION.
       01  WS-LIG-RAP     PIC X(200)   VALUE SPACE      .
       01  FICI-STATUS     PIC XX                .
       88  FICI-STATUS-OK         VALUE '00'     .
       88  FICI-STATUS-EOF        VALUE '10'     .
       01  FICO-STATUS     PIC XX                .
       88  FICO-STATUS-OK         VALUE '00'     .
       88  FICO-STATUS-EOF        VALUE '10'     .

      * Gestion des erreurs
       01  WS-SQL-LIB           PIC X(80)               .
       01  FIN                  PIC S9(9)      VALUE 100.

      * Déclaration des variables correspondant à sql
       EXEC SQL BEGIN DECLARE SECTION END-EXEC           .
      * paramètres pour connexion à la base
       01  DBNAME     PIC X(30)   VALUE ':DBNOM:'        .
       01  USERNAME   PIC X(30)   VALUE ':DBUSER:'       .
       01  PASSWD     PIC X(10)   VALUE 'cbl85'        .

       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
      *0000-main-start.

           EXEC SQL
              CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.
           IF  SQLCODE NOT = ZERO
               MOVE 'CONNECTION BASE' TO WS-SQL-LIB
               PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.




      *0000-main-end.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF  SQLCODE NOT = ZERO
              MOVE 'DISCONNECTION BASE' TO WS-SQL-LIB
              PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.
           STOP RUN.

       7000-cursname-START.
      ***********************************************************
      *       Déclaration du curseur cursname
      ***********************************************************
           EXEC SQL
              DECLARE cursname CURSOR FOR
                 SELECT champs à récupérer

                 FROM tables où sont stockés les informations
                 WHERE jointure
                 ORDER BY tri
           END-EXEC.
           IF  SQLCODE NOT = ZERO
              MOVE 'DECLARATION cursname' TO WS-SQL-LIB
              PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.
      ***********************************************************
      *       Ouverture du curseur cursname
      ***********************************************************
           EXEC SQL
              OPEN cursname
           END-EXEC.
           IF  SQLCODE NOT = ZERO
              MOVE 'OUVERTURE cursname' TO WS-SQL-LIB
              PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.
      ***********************************************************
      *       Lecture du curseur cursname
      ***********************************************************
           EXEC SQL
              FETCH cursname
              INTO :SQL-champ1, :SQL-champ2
           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
              MOVE 'LECTURE cursname' TO WS-SQL-LIB
              PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.
      ***********************************************************
      *       Exploitation des données du curseur cursname
      ***********************************************************
           PERFORM UNTIL SQLCODE = FIN

           END-PERFORM.
      ***********************************************************
      *       Fermeture du curseur cursname
      ***********************************************************
           EXEC SQL
              CLOSE cursname
           END-EXEC.
           IF  SQLCODE NOT = ZERO
              MOVE 'FERMETURE cursname' TO WS-SQL-LIB
              PERFORM 9020-SQL-ERROR-START
                   THRU 9020-SQL-ERROR-END
           END-IF.
       7000-cursname-END.
           EXIT.


       8000-OPEN-FICI-START.
           OPEN INPUT FICI.
           PERFORM 9000-TEST-FICI-STATUT-START
                    THRU 9000-TEST-FICI-STATUT-END.
       8000-OPEN-FICI-END.
           EXIT.

       8010-WRITE-FICI-START.
           READ FICI  .
           PERFORM 9000-TEST-FICI-STATUT-START
                       THRU 9000-TEST-FICI-STATUT-END.
       8010-WRITE-FICI-END.
           EXIT.

       8020-CLOSE-FICI-START.
           CLOSE FICI.
           PERFORM 9000-TEST-FICI-STATUT-START
                    THRU 9000-TEST-FICI-STATUT-END.
       8020-CLOSE-FICI-END.
           EXIT.



       8000-OPEN-FICO-START.
           OPEN OUTPUT FICO.
           PERFORM 9000-TEST-FICO-STATUT-START
                    THRU 9000-TEST-FICO-STATUT-END.
       8000-OPEN-FICO-END.
           EXIT.

       8010-WRITE-FICO-START.
           WRITE REC-FICO  .
           PERFORM 9000-TEST-FICO-STATUT-START
                    THRU 9000-TEST-FICO-STATUT-END.
       8010-WRITE-FICO-END.
           EXIT.

       8020-CLOSE-FICO-START.
           CLOSE FICO.
           PERFORM 9000-TEST-FICO-STATUT-START
                    THRU 9000-TEST-FICO-STATUT-END.
       8020-CLOSE-FICO-END.
           EXIT.


       9000-TEST-FICI-STATUT-START.
           IF (NOT FICI-STATUS-OK)
              AND (NOT FICI-STATUS-EOF) THEN
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE FICI-STATUS
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              STOP RUN
           END-IF.
       9000-TEST-FICI-STATUT-END.
           EXIT.


       9000-TEST-FICO-STATUT-START.
           IF (NOT FICO-STATUS-OK)
              AND (NOT FICO-STATUS-EOF) THEN
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE FICO-STATUS
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              STOP RUN
           END-IF.
       9000-TEST-FICO-STATUT-END.
           EXIT.


       9020-SQL-ERROR-START.
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
       9020-SQL-ERROR-END.
           STOP RUN.


