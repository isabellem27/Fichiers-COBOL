      ****************************************************************** 
      * GESTSQL0: Creation des tables SQL 
      *           Lecture du fichier input 
      *           Chargement des tables SQL
      * PROMOSQL: Chargement des tables de travail pour le calcul 
      *           de la moyenne
      *           Appel du sous-programme ecritfo1
      * ECRITFO1: Ecriture des informations dans le fichier output
      *
      * Date de création : le 03/05/2024   
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. promosql.
       AUTHOR. Isabelle Marand.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.         

       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
              05 WS-COURS-COEF  PIC 9V9                .
              05 WS-COURS-MOY   PIC 9(5)V99    VALUE 0 .
              05 WS-COURS-DIV   PIC 9(5)V99    VALUE 0 .   


       01  WS-IND-EL            PIC 99         VALUE 0 .

       01  WS-ELEVE .
           03  WS-NB-ELEVE      PIC 99         VALUE 0 .
           03  WS-ELEVE-TAB  OCCURS  99 TIMES
      *    03  WS-ELEVE-TAB  OCCURS 1 TO 99 
      *                      DEPENDING WS-NB-ELEVE
                             INDEXED BY IDX-ELEVE    .
              05 WS-ELEVE-ID    PIC 99               .
              05 WS-ELEVE-FNAME PIC X(6)             .
              05 WS-ELEVE-LNAME PIC X(7)             .               
              05 WS-ELEVE-MOY   PIC 9(5)V99          .
              05 WS-ELEVE-DIV   PIC 9(5)V99          .
       
       01  WS-IND-CL            PIC 99      VALUE 0  .
       01  WS-ID1               PIC 99      VALUE 0  .
       01  WS-ID2               PIC 99      VALUE 0  .
       
       01  WS-CLASSE.
           03  WS-NB-ENREG          PIC 999 VALUE 0 .
           03  WS-CLASSE-TAB    OCCURS 999 TIMES
      *     03  WS-CLASSE-TAB    OCCURS 1 TO 999 
      *                          DEPENDING   WS-NB-ENREG
                                ASCENDING KEY WS-CLASSE-ID1 
                                            WS-CLASSE-ID2
                                INDEXED BY IDX-CLASSE   .
              05 WS-CLASSE-ID1     PIC 99      .
              05 WS-CLASSE-ID2     PIC 99      .
              05 WS-CLASSE-NOTE    PIC 99V99   .
              05 WS-CLASSE-COEF    PIC 9V9     VALUE 1 .

       01  WS-DIVCLASSE         PIC 99V99      VALUE 0 .
       01  WS-CLASSE-MOY        PIC 999V99     VALUE 0 .

      * Gestion des erreurs
       01  WS-SQL-LIB           PIC X(80)               .
       01  FIN                  PIC S9(9)      VALUE 100.

      * Déclaration des variables correspondant à sql 
       EXEC SQL BEGIN DECLARE SECTION END-EXEC          .
      * paramètres pour connexion à la base 
       01  DBNAME     PIC X(30)   VALUE'promosql'        .
       01  USERNAME   PIC X(30)   VALUE 'cobol'          .
       01  PASSWD     PIC X(10)   VALUE SPACE            .

      * description des enregistrements attendus pour chaque curseur
       01  SQL-CURSOR1.
              05 SQL-CUR1-ST-ID         PIC 99         .
              05 SQL-CUR1-CO-ID         PIC 99         .
              05 SQL-CUR1-ST-LASTNAME   PIC X(7)       .
              05 SQL-CUR1-ST-FIRSTNAME  PIC X(6)       . 
              05 SQL-CUR1-CO-LABEL      PIC X(21)      .       
              05 SQL-CUR1-CO-COEF       PIC 9V9        .
              05 SQL-CUR1-GR-GRADE      PIC 99V99      .
      *       05 SQL-CUR1-GR-COEF       PIC 9V99       .
       
       01  SQL-CURS-COURS.
              05 SQL-CUR2-CO-ID        PIC 99        .      
              05 SQL-CUR2-DIVIDEND     PIC 999V99    .
              05 SQL-CUR2-DIVISEUR     PIC 99V9      .

       01  SQL-CURS-ELEVE.
              05 SQL-CUR3-ST-ID        PIC 99        .
              05 SQL-CUR3-CO-ID        PIC 99        .      
              05 SQL-CUR3-DIVIDEND     PIC 999V99    .
              05 SQL-CUR3-DIVISEUR     PIC 99V9      .

      * variables de travail
       01  SQL_ID1              PIC 99      .
       01  SQL_ID2              PIC 99      .
       01  SQL_NB_ENREG         PIC 99      .
       01  SQL_NB_COURS         PIC 99      .
       01  SQL_NB_STUDENT       PIC 99      .
       01  SQL-DIVIDEND         PIC 9(5)V99 . 
       01  SQL-DIVISEUR         PIC 9(5)V99 .

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

      * Je lie les données dans mes tables et je prépare mes calculs.
      * nb etudiants, nb cours, nb notes
           PERFORM 5030-CHERCHE-NB-INFOS-START 
                 THRU 5030-CHERCHE-NB-INFOS-END.
      * Je récupère le détail dans curseur1 cf description dans WS
           PERFORM 3020-CURSEUR1-START THRU 3020-CURSEUR1-END. 
      * Je calcul les moyennes et complète le chargement des 
      * tables de travail    
      * table ws-cours
           PERFORM 3040-CURSEUR2-START THRU 3040-CURSEUR2-END.
      * table ws-eleve
           PERFORM 3060-CURSEUR3-START THRU 3060-CURSEUR3-END.
      * table ws-classe
           PERFORM 6040-CALC-CLASSE-AVG-START 
                THRU 6040-CALC-CLASSE-AVG-END.

      * J'écris mon fichier   
           CALL 'ecritfo'  USING BY REFERENCE  WS-ELEVE WS-COURS 
                                               WS-CLASSE WS-CLASSE-MOY
           EXCEPTION
              DISPLAY 'PB lors de l''appel de ECRIT-FO' SPACE 
                       'ARRÊT DU PROGRAMME'
              GO TO 0010-STOP-PRG.
          
      *0000-main-end
       0010-STOP-PRG.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DISCONNECTION BASE' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           STOP RUN.

       1000-INITIALIZE-START.
           INITIALIZE WS-NB-COURS     .
           INITIALIZE WS-NB-ELEVE     .
           INITIALIZE WS-ID1          .
           INITIALIZE WS-ID2          .
           INITIALIZE WS-NB-ENREG     .
       1010-INITIALIZE-END.
           EXIT.

       3020-CURSEUR1-START.
      *                , gr.GR_COEF
           EXEC SQL 
              DECLARE CURSEUR1 CURSOR FOR
                 SELECT st.ST_ID, co.CO_ID, st.ST_LASTNAME, 
                       st.ST_FIRSTNAME, co.CO_LABEL, co.CO_COEF,
                       gr.GR_GRADE

                 FROM STUDENT st , GRADE gr , COURSE co 
                 WHERE (st.ST_ID = gr.ST_ID AND co.CO_ID = gr.CO_ID)
                 ORDER BY st.ST_ID,co.CO_ID     
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION CURSEUR1' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              OPEN CURSEUR1
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE CURSEUR1' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           
           PERFORM 3030-CURSEUR1-READ-START THRU 3030-CURSEUR1-READ-END.
           
           EXEC SQL
              CLOSE CURSEUR1
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE CURSEUR1' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
       3020-CURSEUR1-END.
           EXIT.

       3030-CURSEUR1-READ-START.
           INITIALIZE WS-IND-EL.
           INITIALIZE WS-IND-CL.
           INITIALIZE WS-IND-CO.

      *       ,:SQL-CUR1-GR-COEF
           EXEC SQL 
              FETCH CURSEUR1 
              INTO :SQL-CUR1-ST-ID, :SQL-CUR1-CO-ID, 
              :SQL-CUR1-ST-LASTNAME, :SQL-CUR1-ST-FIRSTNAME,
              :SQL-CUR1-CO-LABEL, :SQL-CUR1-CO-COEF,
              :SQL-CUR1-GR-GRADE

           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
              MOVE 'LECTURE CURSEUR1' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
              
           PERFORM UNTIL SQLCODE = FIN 
              PERFORM 5040-CHARGE-WS-TAB-START 
                       THRU 5040-CHARGE-WS-TAB-END

      *          ,:SQL-CUR1-GR-COEF
              EXEC SQL 
                 FETCH CURSEUR1
                 INTO :SQL-CUR1-ST-ID, :SQL-CUR1-CO-ID, 
                 :SQL-CUR1-ST-LASTNAME, :SQL-CUR1-ST-FIRSTNAME,
                 :SQL-CUR1-CO-LABEL, :SQL-CUR1-CO-COEF,
                 :SQL-CUR1-GR-GRADE

              END-EXEC
              IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN
                 MOVE 'LECTURE SUIVANTE CURSEUR1' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF 

  
           END-PERFORM.
       3030-CURSEUR1-READ-END.
           EXIT.
               
       3040-CURSEUR2-START.
           EXEC SQL 
              DECLARE CURSEUR2 CURSOR FOR
                 SELECT gr.CO_ID, 
                    SUM(gr.GR_GRADE * gr.GR_COEF),
                    SUM(gr.GR_COEF)
              FROM GRADE gr 
              GROUP BY gr.CO_ID
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION CURSEUR2' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              OPEN CURSEUR2
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE CURSEUR2' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           
           PERFORM 3050-CURSEUR2-READ-START THRU 3050-CURSEUR2-READ-END.
           
           EXEC SQL
              CLOSE CURSEUR2
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE CURSEUR2' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF. 
       3040-CURSEUR2-END.
           EXIT.

       3050-CURSEUR2-READ-START.
      * Je récupère les sommes qui me permettront de calculer 
      * la moyenne pour chaque cours
           EXEC SQL 
              FETCH CURSEUR2 
              INTO  :SQL-CUR2-CO-ID, :SQL-CUR2-DIVIDEND, 
                    :SQL-CUR2-DIVISEUR
           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) THEN 
              MOVE 'LECTURE CURSEUR2' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           PERFORM UNTIL SQLCODE = FIN  
              PERFORM 6020-CALC-COURS-AVG-START 
                       THRU 6020-CALC-COURS-AVG-END
              EXEC SQL 
                 FETCH  CURSEUR2
                 INTO  :SQL-CUR2-CO-ID, :SQL-CUR2-DIVIDEND, 
                       :SQL-CUR2-DIVISEUR
              END-EXEC
              IF  SQLCODE NOT = ZERO AND SQLCODE NOT = FIN
                 MOVE 'LECTURE SUIVANTE CURSEUR2' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF   


           END-PERFORM.
       3050-CURSEUR2-READ-END.
           EXIT.
       
       3060-CURSEUR3-START.
           EXEC SQL 
              DECLARE CURSEUR3 CURSOR FOR
                 SELECT gr.ST_ID , co.CO_ID, 
                    (SUM(gr.GR_GRADE * gr.GR_COEF)) * co.CO_COEF,
                    SUM(gr.GR_COEF * co.CO_COEF)
              FROM GRADE gr INNER JOIN COURSE co
              ON gr.CO_ID = co.CO_ID
              GROUP BY gr.ST_ID, co.CO_ID
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'DECLARATION CURSEUR3' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           EXEC SQL
              OPEN CURSEUR3
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'OUVERTURE CURSEUR3' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           
           PERFORM 3070-CURSEUR3-READ-START THRU 3070-CURSEUR3-READ-END.
           
           EXEC SQL
              CLOSE CURSEUR3
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE 'FERMETURE CURSEUR3' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
       3060-CURSEUR3-END.
           EXIT.
       
       3070-CURSEUR3-READ-START.
           EXEC SQL 
              FETCH CURSEUR3 
              INTO  :SQL-CUR3-ST-ID, :SQL-CUR3-CO-ID,:SQL-CUR3-DIVIDEND, 
                    :SQL-CUR3-DIVISEUR
           END-EXEC.
           IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) 
              MOVE 'LECTURE CURSEUR3' TO WS-SQL-LIB 
              PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           PERFORM UNTIL SQLCODE = FIN 
              PERFORM 6030-CALC-ELEVE-AVG-START 
                       THRU 6030-CALC-ELEVE-AVG-END
              EXEC SQL 
                 FETCH CURSEUR3
                 INTO :SQL-CUR3-ST-ID, :SQL-CUR3-CO-ID, 
                 :SQL-CUR3-DIVIDEND, :SQL-CUR3-DIVISEUR
              END-EXEC
              IF  (SQLCODE NOT = ZERO) AND (SQLCODE NOT = FIN) 
                 MOVE 'LECTURE SUIVANTE CURSEUR3' TO WS-SQL-LIB 
                 PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
              END-IF   
           END-PERFORM.
       3070-CURSEUR3-READ-END.
           EXIT.

       5030-CHERCHE-NB-INFOS-START.
           EXEC SQL 
              SELECT COUNT(*) INTO :SQL_NB_STUDENT FROM STUDENT;
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               MOVE 'NB STUDENT' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           MOVE SQL_NB_STUDENT TO WS-NB-ELEVE.
           
           EXEC SQL 
              SELECT COUNT(*) INTO :SQL_NB_COURS FROM COURSE;
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               MOVE 'NB COURS' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           MOVE SQL_NB_COURS TO WS-NB-COURS.

           EXEC SQL 
              SELECT COUNT(*) INTO :SQL_NB_ENREG FROM GRADE;
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               MOVE 'NB GRADE' TO WS-SQL-LIB
               PERFORM 9050-ERROR-RTN-START
                   THRU 9050-ERROR-RTN-END
           END-IF.
           MOVE SQL_NB_ENREG TO WS-NB-ENREG.

       5030-CHERCHE-NB-INFOS-END.
           EXIT. 
       
       5040-CHARGE-WS-TAB-START .
      * Je charge les tables ws-eleve et ws-classe.

      * Chargement de WS-ELEVE
           SET IDX-ELEVE TO 1.
           SEARCH WS-ELEVE-TAB
           AT END
              SET WS-IND-EL UP BY 1
              MOVE SQL-CUR1-ST-ID        TO WS-ELEVE-ID(WS-IND-EL)     
              MOVE SQL-CUR1-ST-FIRSTNAME TO WS-ELEVE-FNAME(WS-IND-EL)  
              MOVE SQL-CUR1-ST-LASTNAME  TO WS-ELEVE-LNAME(WS-IND-EL) 
           WHEN (SQL-CUR1-ST-ID = WS-ELEVE-ID(WS-IND-EL))
      * juste pour éviter le plantage à la compile
              MOVE SQL-CUR1-ST-ID        TO WS-ELEVE-ID(WS-IND-EL)
           END-SEARCH.

      
      * Chargement de WS-COURS
           SET IDX-COURS TO 1.
           SEARCH WS-COURS-TAB
           AT END
              SET WS-IND-CO UP BY 1
              MOVE SQL-CUR1-CO-ID        TO WS-COURS-ID(WS-IND-CO)     
              MOVE SQL-CUR1-CO-LABEL     TO WS-COURS-LIB(WS-IND-CO)  
              MOVE SQL-CUR1-CO-COEF      TO WS-COURS-COEF(WS-IND-CO) 
           WHEN (SQL-CUR1-CO-LABEL = WS-COURS-LIB(IDX-COURS))
      * juste pour éviter le plantage à la compile
              MOVE SQL-CUR1-CO-ID        TO WS-COURS-ID(WS-IND-CO)
           END-SEARCH.

      * Chargement de WS-CLASSE
           SET WS-IND-CL UP BY 1.
           MOVE SQL-CUR1-ST-ID        TO WS-CLASSE-ID1(WS-IND-CL)   . 
           MOVE SQL-CUR1-CO-ID        TO WS-CLASSE-ID2(WS-IND-CL)   .
           MOVE SQL-CUR1-GR-GRADE     TO WS-CLASSE-NOTE(WS-IND-CL)  .
      *    MOVE SQL-CUR1-GR-COEF      TO WS-CLASSE-COEF(WS-IND-CL)  .

       5040-CHARGE-WS-TAB-END.
           EXIT.

       6020-CALC-COURS-AVG-START.
      * Je cherche dans ma table le cours à compléter
           SET IDX-COURS TO 1.
           SEARCH  WS-COURS-TAB 
           AT END
              DISPLAY 'COURS NON TROUVE ' SQL-CUR2-CO-ID
      * Quand trouvé, je charge les sommes, et calcule la moyenne     
           WHEN WS-COURS-ID(IDX-COURS) = SQL-CUR2-CO-ID 
               MOVE SQL-CUR2-DIVIDEND TO WS-COURS-MOY(IDX-COURS)
               MOVE SQL-CUR2-DIVISEUR TO WS-COURS-DIV(IDX-COURS)
               IF (WS-COURS-DIV(IDX-COURS) > 0) THEN
                 COMPUTE WS-COURS-MOY(IDX-COURS) ROUNDED  =
                    WS-COURS-MOY(IDX-COURS) / WS-COURS-DIV(IDX-COURS)
               END-IF 
               
           END-SEARCH. 
              
      * Je charge mes variables de la classe avec les informations
      * du cours
           COMPUTE WS-CLASSE-MOY = WS-CLASSE-MOY +
               (WS-COURS-MOY(IDX-COURS) * WS-COURS-COEF(IDX-COURS)).
           SET WS-DIVCLASSE UP BY WS-COURS-COEF(IDX-COURS) .          
           
       6020-CALC-COURS-AVG-END.
           EXIT.

       6030-CALC-ELEVE-AVG-START.
      * Je cherche dans ma table l'étudiant à compléter
           SET IDX-ELEVE TO 1.
           SEARCH  WS-ELEVE-TAB 
           AT END
              DISPLAY 'ELEVE NON TROUVE ' SQL-CUR3-ST-ID
      * Quand trouvé, je charge les sommes, et calcule la moyenne     
           WHEN WS-ELEVE-ID(IDX-ELEVE) = SQL-CUR3-ST-ID 
               MOVE SQL-CUR3-DIVIDEND TO WS-ELEVE-MOY(IDX-ELEVE)
               MOVE SQL-CUR3-DIVISEUR TO WS-ELEVE-DIV(IDX-ELEVE)
               IF (WS-ELEVE-DIV(IDX-ELEVE) > 0) THEN 
                 COMPUTE WS-ELEVE-MOY(IDX-ELEVE) ROUNDED  =
                    WS-ELEVE-MOY(IDX-ELEVE) / WS-ELEVE-DIV(IDX-ELEVE)
               END-IF     
           END-SEARCH.         
       6030-CALC-ELEVE-AVG-END.
           EXIT.

       6040-CALC-CLASSE-AVG-START.
           COMPUTE WS-CLASSE-MOY ROUNDED = WS-CLASSE-MOY / WS-DIVCLASSE.
       6040-CALC-CLASSE-AVG-END.
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
             
                 
   
