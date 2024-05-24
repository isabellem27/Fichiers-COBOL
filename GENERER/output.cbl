      *=============================================================*
      *    Vos commentaires                                         *
      *                                                             *
      *                                                             *
      *                                                             *
      *                                                             *
      *    auteur : ISABELLE.                           *
      *    Date creation 24/05/2024                                 *
      *=============================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOBONI01.
       AUTHOR . ISABELLE.

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
           RECORD CONTAINS 234 CHARACTERS
           RECORDING MODE IS F.
       01  REC-FICI        PIC X(234) .
       FD  FICO
           RECORD CONTAINS 2 TO 080 CHARACTERS
           RECORDING MODE IS V.
       01  REC-FICO        PIC X(080).


       WORKING-STORAGE SECTION.
       01  WS-LIG-RAP     PIC X(200)   VALUE SPACE      .
       01  FICI-STATUS     PIC XX                .
       88  FICI-STATUS-OK         VALUE '00'     .
       88  FICI-STATUS-EOF        VALUE '10'     .
       01  FICO-STATUS     PIC XX                .
       88  FICO-STATUS-OK         VALUE '00'     .
       88  FICO-STATUS-EOF        VALUE '10'     .

       PROCEDURE DIVISION.
      *0000-main-start.


           CALL 'SSBONOBO'
               USING BY  REFERENCE  PARM PARMC
               USING BY  CONTENT PARMD PARMB
           RETURNING  RETBOBO

           EXCEPTION
              DISPLAY 'PB lors de l''appel de SSBONOBO' SPACE
                       'ARRET DU PROGRAMME'
            STOP RUN
            END-CALL.


      *0000-main-end.
           STOP RUN.
           COPY FILEI-GEST REPLACING ==:FNAME:== BY ==FICI==.
           COPY FILEO-GEST REPLACING ==:FNAME:== BY ==FICO==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FICI==.
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==FICO==.
