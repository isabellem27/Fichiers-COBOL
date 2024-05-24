      *=============================================================*
      *    Creation d'un fichier outputsql.cbl contenant le skelette*
      *    du programme à écrire  qui intègre du sql                *
      *    Pas de copybook dans ce type de skelette pour passer la  *
      *    compile.                                                 *
      *    Programme appellé par generprg si ws-sql= O.             *                                                         *
      *                                                             *      
      *                                                             *
      *    auteur : Isabelle Marand                                 *
      *    Date création 23/05/2024                                 *
      *=============================================================*    
       IDENTIFICATION DIVISION.
       PROGRAM-ID. genersql.
       AUTHOR . Isabelle Marand.
      
      * Environnement division et déclaration de tous les fichiers    
       COPY SQFILE-IO.

       DATA DIVISION.
      * File section pour tous les fichiers    
       COPY SQFILE-SECT.

       WORKING-STORAGE SECTION.
      * gestion des status des fichiers 
           COPY SQFILE-STAT.

      * variables 
       01  WS-ETAT        PIC 9       VALUE 0     .
       88  WS-FIN         VALUE 1                 .    
      
       01  WS-IND         PIC 999     VALUE 0     .
       01  WS-IND-NOM     PIC 999     VALUE 0     .


       01  WS-POS-CHAINE  PIC 99      VALUE 0     .
       01  WS-LNGFIN      PIC 99      VALUE 0     .
       01  WS-LNGDEB      PIC 99      VALUE 0     .
       01  WS-NBCARAC     PIC 99      VALUE 0     .
       01  WS-REF1        PIC 99      VALUE 0     .  
       01  WS-CNT1        PIC 9       VALUE 1     .     
       01  WS-NB-LIG      PIC 9       VALUE 0     .
       01  WS-LIG-CALL    PIC 99      VALUE 16    .
       01  WS-CONTENT     PIC X(7)    VALUE 'CONTENT'.
       01  WS-REFERENCE   PIC X(10)   VALUE 'REFERENCE'.
       01  WS-REPONSE     PIC X(21)   VALUE '           RETURNING '.
       01  WS-LIG-REFPARAM PIC X(81)  VALUE SPACE . 
       01  WS-LIG-CNTPARAM PIC X(81)  VALUE SPACE .


       LINKAGE SECTION.
      
       01  WS-PRG-NOM     PIC X(8)    VALUE SPACE .
       01  WS-PRG-AUT     PIC X(20)   VALUE SPACE .  
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
       01  WS-DATE-DMY.
           05 WS-DAT-JJ   PIC 99      .
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-MM   PIC 99      .
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-YY   PIC 9(4)    .
          
      * Gestion du dialogue avec l'utilisateur
       01  WS-LIG-RAP      PIC X(100)  VALUE   SPACE    .    
       01  WS-LIG-BLC      PIC X(10)   VALUE   SPACES   .  

      * debug
       01  PAUSE-TIME PIC S9(09) BINARY VALUE 500.
       01  WS-DEBUG    PIC X(5) VALUE SPACE.
       
 
       PROCEDURE DIVISION USING WS-PRG-NOM WS-PRG-AUT WS-SSPRG 
                           WS-DBNAME WS-DATE-DMY WS-FIC . 
      *0000-main-start.


           PERFORM 1050-PRG-GENERE-START THRU 1050-PRG-GENERE-START
     
      *0000-main-end.
           STOP RUN.

       1050-PRG-GENERE-START.
           PERFORM 2000-OPEN-PRG-START THRU 2000-OPEN-PRG-END.
           PERFORM 3030-IDENT-DIV-START THRU 3030-IDENT-DIV-END.
           PERFORM 3000-ENV-DIV-START THRU 3000-ENV-DIV-END.           
           PERFORM 1070-PRG-PERSO-START THRU 1070-PRG-PERSO-END.
           PERFORM 2020-CLOSE-PRG-START THRU 2020-CLOSE-PRG-END.
           SET WS-ETAT TO 1.
       1050-PRG-GENERE-END.
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
           PERFORM 3030-SQL-WS-START THRU 3030-SQL-WS-END.
           PERFORM 3030-PROC-DIV-START THRU 3030-PROC-DIV-END.
                                   
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

           PERFORM VARYING WS-IND FROM 1 BY 1 
             UNTIL (WS-IND > WS-NB-FIC)
                PERFORM 3035-TST-STAT-START THRU 3035-TST-STAT-END                        
           END-PERFORM. 
           PERFORM 3000-SQL-STAT-START THRU 3000-SQL-STAT-END.      
       1070-PRG-PERSO-END.
           EXIT.

      * open, read, write, close de tous les fichiers 
      * utilisés dans generprg
           COPY SQFILE-GEST.
           COPY PRG-3000.
       
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

       3030-SQL-WS-START.
           PERFORM 2000-OPEN-SQL-DCL-START 
                    THRU 2000-OPEN-SQL-DCL-END.
           PERFORM 2030-READ-SQL-DCL-START 
                    THRU 2030-READ-SQL-DCL-END.
           PERFORM UNTIL SQL-DCL-STATUS-EOF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-SQL-DCL 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':DBNOM:' 
                       WS-LNGFIN FOR CHARACTERS AFTER  ':DBNOM:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-SQL-DCL(1:WS-LNGDEB)
                          FUNCTION TRIM (WS-SQLNOM)
                          REC-SQL-DCL(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN 
              INSPECT REC-SQL-DCL 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':DBUSER:' 
                       WS-LNGFIN FOR CHARACTERS AFTER  ':DBUSER:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-SQL-DCL(1:WS-LNGDEB)
                          FUNCTION TRIM (WS-SQLUSER)
                          REC-SQL-DCL(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF    
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN 
              INSPECT REC-SQL-DCL 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':DBPSW:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':DBPSW:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 IF FUNCTION LENGTH (FUNCTION TRIM(WS-SQLPSW)) > 0 THEN
                    STRING   REC-SQL-DCL(1:WS-LNGDEB)
                             FUNCTION TRIM (WS-SQLPSW)
                             REC-SQL-DCL(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG
                 ELSE 
                    STRING   REC-SQL-DCL(1:WS-LNGDEB)
                             'SPACE'
                             REC-SQL-DCL(WS-POS-CHAINE:WS-LNGFIN)
                    DELIMITED BY SIZE
                    INTO REC-F-PRG                      
                 END-IF 
              ELSE  
                 INITIALIZE REC-F-PRG
              END-IF           
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-SQL-DCL TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-SQL-DCL-START 
                    THRU 2030-READ-SQL-DCL-END
           END-PERFORM.
           PERFORM 2020-CLOSE-SQL-DCL-START 
                    THRU 2020-CLOSE-SQL-DCL-END.       
       3030-SQL-WS-END.
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
           PERFORM UNTIL TST-STAT-STATUS-EOF 
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-TST-STAT 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':FNAME:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':FNAME:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
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
              PERFORM 2030-READ-TST-STAT-START 
                                      THRU 2030-READ-TST-STAT-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-TST-STAT-START 
                       THRU 2020-CLOSE-TST-STAT-END.       
       3035-TST-STAT-END.
           EXIT.

       3035-GEST-FILI-START.
           PERFORM 2000-OPEN-GEST-FILI-START 
                          THRU 2000-OPEN-GEST-FILI-END.
           PERFORM 2030-READ-GEST-FILI-START 
                          THRU 2030-READ-GEST-FILI-END. 
           PERFORM UNTIL GEST-FILI-STATUS-EOF 
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-GEST-FILI 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':FNAME:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':FNAME:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-GEST-FILI(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-GEST-FILI(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-GEST-FILI TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END 
              PERFORM 2030-READ-GEST-FILI-START 
                          THRU 2030-READ-GEST-FILI-END
           END-PERFORM. 
           PERFORM 2020-CLOSE-GEST-FILI-START 
                       THRU 2020-CLOSE-GEST-FILI-END.
       3035-GEST-FILI-END.
           EXIT.

       3035-GEST-FILO-START.
           PERFORM 2000-OPEN-GEST-FILO-START 
                       THRU 2000-OPEN-GEST-FILO-END.                          
           PERFORM 2030-READ-GEST-FILO-START 
                          THRU 2030-READ-GEST-FILO-END.   
           PERFORM UNTIL GEST-FILO-STATUS-EOF 
              INITIALIZE WS-POS-CHAINE WS-LNGDEB WS-LNGFIN REC-F-PRG
              INSPECT REC-GEST-FILO 
              TALLYING WS-LNGDEB FOR CHARACTERS BEFORE ':FNAME:' 
                       WS-LNGFIN FOR CHARACTERS AFTER ':FNAME:'
              IF (WS-LNGDEB < 80) THEN
                 ADD 8 TO WS-LNGDEB GIVING WS-POS-CHAINE
                 STRING   REC-GEST-FILO(1:WS-LNGDEB)
                          FUNCTION TRIM(WS-FIC-NOM (WS-IND))
                          REC-GEST-FILO(WS-POS-CHAINE:WS-LNGFIN)
                 DELIMITED BY SIZE
                 INTO REC-F-PRG
              ELSE 
                 INITIALIZE REC-F-PRG
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(REC-F-PRG)) = 0 THEN
                 MOVE REC-GEST-FILO TO REC-F-PRG 
              END-IF                 
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END 
              PERFORM 2030-READ-GEST-FILO-START 
                          THRU 2030-READ-GEST-FILO-END 
           END-PERFORM.   
           PERFORM 2020-CLOSE-GEST-FILO-START 
                       THRU 2020-CLOSE-GEST-FILO-END.                    
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
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==SQL-DCL==. 
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==SQL-STAT==. 
