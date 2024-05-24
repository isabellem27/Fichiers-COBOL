       ENVIRONMENT DIVISION.                  
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT F-PRG 
           ASSIGN TO 'outputsql.cbl' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS F-PRG-STATUS.
           
           SELECT IDENT-DIV 
           ASSIGN TO './COPY/IDENT-DIV.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS IDENT-DIV-STATUS.
           
           SELECT ENV-DIV 
           ASSIGN TO './COPY/ENV-DIV.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ENV-DIV-STATUS.
           
           SELECT DATA-DIV 
           ASSIGN TO './COPY/DATA-DIV.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS DATA-DIV-STATUS.
           
           SELECT PROC-DIV 
           ASSIGN TO './SQLCOPY/SQPROC-DIV.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS PROC-DIV-STATUS.
           
           SELECT WS-SECTION 
           ASSIGN TO './COPY/WS-SECTION.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-SECTION-STATUS.
               
           SELECT FILE-SECTION 
           ASSIGN TO './COPY/FILE-SECT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILE-SECTION-STATUS.
               
           SELECT FILEF-SECTION 
           ASSIGN TO './COPY/FILEF-SECT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILEF-SECT-STATUS.
                          
           SELECT FILEV-SECTION 
           ASSIGN TO './COPY/FILEV-SECT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILEV-SECT-STATUS.

           SELECT FILE-STATUS 
           ASSIGN TO './COPY/FILE-STATUS.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS-STATUS.
               
           SELECT IO-SECTION 
           ASSIGN TO './COPY/IO-SECT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS IO-SECTION-STATUS.
               
           SELECT TST-STAT 
           ASSIGN TO 'TST-STATUT.cpy' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS TST-STAT-STATUS.
               
           SELECT GEST-FILI 
           ASSIGN TO 'FILEI-GEST.cpy' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS GEST-FILI-STATUS.
               
           SELECT GEST-FILO 
           ASSIGN TO 'FILEO-GEST.cpy' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS GEST-FILO-STATUS.
               
               
           SELECT FILE-IO 
           ASSIGN TO './COPY/FILE-IO.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILE-IO-STATUS.
               
           SELECT FILE-CONT 
           ASSIGN TO './COPY/FILE-CONT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILE-CONT-STATUS.
               
           SELECT CALL-SSPRG 
           ASSIGN TO './COPY/CALL-SSPRG.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS CALL-SSPRG-STATUS.
              
           SELECT SQL-DCL 
           ASSIGN TO './SQLCOPY/SQL-DECLARE.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS SQL-DCL-STATUS.  
              
           SELECT SQL-STAT 
           ASSIGN TO './SQLCOPY/SQL-STATUT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS SQL-STAT-STATUS.  
                                        