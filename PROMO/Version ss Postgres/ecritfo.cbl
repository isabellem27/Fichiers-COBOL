      ****************************************************************** 
      * Ecriture du fichier output contenant le bulletin de la classe. *
      * Date de création : le 28/04/2024                               *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ecritfo.
       AUTHOR. CobolP3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

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

       01  REC-F-OUTPUT        PIC X(105).

       WORKING-STORAGE SECTION.
       01  F-OUTPUT-STATUS      PIC X(02)   VALUE SPACE .
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

      * Création d'une table supplémentaire pour connaitre
      * la position du cours dans le rapport afin de positionner
      * la note dans la bonne colonne si tri par libellé du cours
       01  WS-ID2-T.
           03  WS-IND-ID2          PIC 99   .
           03  WS-ID2-TAB  OCCURS 1 TO 99 
                       DEPENDING WS-NB-COURS
                       INDEXED BY IDX-ID2   .       
              05 WS-ID2-ID         PIC 99   .
              05 WS-ID2-NOTE       PIC Z9,99.

      * Variables internes
       01  WS-IND-CO            PIC 99      VALUE 0     .
       01  WS-IND-EL            PIC 99      VALUE 0     .
       01  WS-IND-CL            PIC 99      VALUE 0     .
       01  WS-ID1               PIC 99      VALUE 0     .
       01  WS-ID2               PIC 99      VALUE 0     .

      *  GESTION DE LA SORTIE
       01  WS-CLASSE-MOY-LIG    PIC Z9,99               .
       01  WS-COURS-MOY-LIG     PIC 99,99               .
       01  WS-COURS-COEF-LIG    PIC 9,9                 .
       01  WS-NB-ELEVE-LIG      PIC Z9                  .
       01  WS-NB-COURS-LIG      PIC Z9                  .
       01  WS-NB-ENREG-LIG      PIC Z9                  .
       01  WS-ELEVE-MOY-LIG     PIC Z9,99               .
       01  WS-IND-CO-LIG        PIC Z9                  .
      * Gestion des libellés

      *    ENtete
       01  WS-LIG-BLANC30    PIC X(30)   VALUE SPACES   . 
       01  WS-LIG-BLANC8     PIC X(8)    VALUE SPACES   .
       01  WS-LIG-BLANC2     PIC X(2)    VALUE SPACES   .
       01  WS-LIG-BLANC3     PIC X(3)    VALUE SPACES   .
       01  WS-LIG-BLANC20    PIC X(20)   VALUE SPACES   .
       01  WS-LIG-TITRE      PIC X(50)   VALUE 
                       'BULLETIN DE LA CLASSE COBOLP3'  .

       01  WS-LIG-ELEVE      PIC X(22)   VALUE 'ELEVE'  .
       01  WS-LIG-MOYE       PIC X(8)    VALUE 'MOYENNE'.                       
       01  WS-LIG-COEF       PIC X(1)    VALUE 'C'      .
       01  WS-LIG-COURS1     PIC X(105)                 .        

      * Bas de page
       01  WS-LIG-CLASSE     PIC X(8)    VALUE ' COBOLP3'      .
       01  WS-LIG-NBELEVE    PIC X(25)   
                          VALUE 'NOMBRE D''ELEVES         :'   .
       01  WS-LIG-NBCOURS    PIC X(25)   
                          VALUE 'NOMBRE DE COURS         :'    .
       01  WS-LIG-NBENREG    PIC X(25)   
                          VALUE 'NOMBRE ENREGISTREMENT   :'    .
       01  WS-LIG-FINFIC     PIC X(50)   VALUE 'FIN DE FICHIER'.

      * LEGENDE DU DOCUMENT
       01  WS-LIG-SLASH      PIC X       VALUE '/'   .
       01  WS-LIG-26         PIC X(26)               .   

       LINKAGE SECTION. 
       01  WS-ELEVE .
           03  WS-NB-ELEVE          PIC 99        .
           03  WS-ELEVE-TAB  OCCURS 1 TO 99 
                             DEPENDING WS-NB-ELEVE
                             INDEXED BY IDX-ELEVE .
              05 WS-ELEVE-ID       PIC 99         .
              05 WS-ELEVE-FNAME    PIC X(6)       .
              05 WS-ELEVE-LNAME    PIC X(7)       .   
              05 WS-ELEVE-AGE      PIC 99         .
              05 WS-ELEVE-MOY      PIC 999V99     .
              05 WS-ELEVE-DIV      PIC 9(5)V99    .
       
       01  WS-COURS .
           03  WS-NB-COURS      PIC 99            . 
           03  WS-COURS-TAB  OCCURS 1 TO 99 
                             DEPENDING WS-NB-COURS
                             INDEXED BY IDX-COURS .
              05 WS-COURS-ID    PIC 99            .
              05 WS-COURS-LIB   PIC X(21)         .
              05 WS-COURS-COEF  PIC 9V9           .
              05 WS-COURS-MOY   PIC 9(5)V99       .
              05 WS-COURS-DIV   PIC 9(5)V99       . 
       
       01  WS-CLASSE.
           03  WS-NB-ENREG          PIC 999    .
           03  WS-CLASSE-TAB    OCCURS 1 TO 999 
                                DEPENDING   WS-NB-ENREG
                                ASCENDING KEY WS-CLASSE-ID1 
                                            WS-CLASSE-ID2
                                INDEXED BY IDX-CLASSE.
              05 WS-CLASSE-ID1     PIC 99      .
              05 WS-CLASSE-ID2     PIC 99      .
              05 WS-CLASSE-NOTE    PIC 99V99   .
              05 WS-CLASSE-COEF    PIC 9V9     VALUE 1 .

       01  WS-CLASSE-MOY        PIC 999V99.

       PROCEDURE DIVISION USING WS-ELEVE WS-COURS WS-CLASSE 
                                WS-CLASSE-MOY .
      *0000-Main-start
      * Je trie les éléves par  ordre alphabétique
           SORT WS-ELEVE-TAB 
                 ASCENDING KEY WS-ELEVE-LNAME WS-ELEVE-FNAME.
      * Je peux trier par libellé de cours
      *    COPY TRIER-COURS.      
           SORT WS-COURS-TAB ASCENDING KEY WS-COURS-LIB.

           PERFORM 3050-WRITE-FO-START   THRU 3060-WRITE-FO-END.
      *0000-main-end
           STOP RUN.


       3000-OPEN-FO-START.
           OPEN OUTPUT F-OUTPUT.
       3010-OPEN-FO-END. 
           EXIT.

       3050-WRITE-FO-START.
           PERFORM 3000-OPEN-FO-START THRU 3010-OPEN-FO-END.

           PERFORM 7000-WRITE-HEADER-START 
                             THRU 7010-WRITE-HEADER-END.
           PERFORM 7020-WRITE-DETAIL-START  THRU 7030-WRITE-DETAIL-END.
           PERFORM 7040-WRITE-FIN-START     THRU 7055-WRITE-FIN-END.
           PERFORM 7056-WRITE-LEG-START     THRU 7057-WRITE-LEG-END.
           PERFORM 7058-WRITE-CPT-START     THRU 7058-WRITE-CPT-END.

           PERFORM 3080-CLOSE-FO-START THRU 3090-CLOSE-FO-END.
       3060-WRITE-FO-END. 
           EXIT.

       3080-CLOSE-FO-START.
           CLOSE F-OUTPUT.
       3090-CLOSE-FO-END.    
           EXIT.

       5090-LOAD-ID2-START. 
      *    Je cherche dans la table ID2 quelle  colonne
      *    correspond à ce cours et charge la note
               SET IDX-ID2 TO 1.
               SEARCH WS-ID2-TAB 
                   WHEN (WS-ID2 =  WS-ID2-ID(IDX-ID2))
                       MOVE WS-CLASSE-NOTE(WS-IND-CL) 
                           TO WS-ID2-NOTE(IDX-ID2)
               END-SEARCH.
       5095-LOAD-ID2-END.
           EXIT.
       
       7000-WRITE-HEADER-START. 
           MOVE ALL '=' TO REC-F-OUTPUT     .
           WRITE REC-F-OUTPUT               . 
           MOVE ALL SPACE TO REC-F-OUTPUT   . 
           STRING 
                 WS-LIG-BLANC30 
                 WS-LIG-TITRE
           DELIMITED BY SIZE 
           INTO REC-F-OUTPUT                .      
           WRITE REC-F-OUTPUT               .
           MOVE ALL '=' TO REC-F-OUTPUT     .
           WRITE REC-F-OUTPUT               . 
           MOVE ALL SPACE TO REC-F-OUTPUT   . 

           PERFORM 7060-PREPA-C-LINES-START 
              THRU 7065-PREPA-C-LINES-END.
                     
           STRING
                 WS-LIG-ELEVE 
                 WS-LIG-BLANC3
                 WS-LIG-MOYE
                 WS-LIG-BLANC3 
                 WS-LIG-COURS1
           DELIMITED BY SIZE 
           INTO REC-F-OUTPUT                .      
           WRITE REC-F-OUTPUT               .

           MOVE ALL '=' TO REC-F-OUTPUT     .
           WRITE REC-F-OUTPUT               . 
           MOVE ALL SPACE TO REC-F-OUTPUT   . 
       7010-WRITE-HEADER-END.
           EXIT.

       7020-WRITE-DETAIL-START.
      *    Pour chaque élève:
           PERFORM VARYING WS-IND-EL FROM 1 BY 1
                 UNTIL (WS-IND-EL > WS-NB-ELEVE)
                 MOVE WS-ELEVE-ID(WS-IND-EL) TO WS-ID1
      *    Je cherche dans la table classe les notes de l'élève
                 SET IDX-CLASSE TO 1
                 SEARCH WS-CLASSE-TAB
                 WHEN WS-CLASSE-ID1(IDX-CLASSE) = WS-ID1 
      * Pour chaque note de l'élève:           
                    PERFORM VARYING WS-IND-CL 
                    FROM IDX-CLASSE BY 1 
                    UNTIL (WS-ID1 NOT EQUAL WS-CLASSE-ID1(WS-IND-CL))

                       MOVE WS-CLASSE-ID2(WS-IND-CL) TO WS-ID2
                       PERFORM 5090-LOAD-ID2-START 
                          THRU 5095-LOAD-ID2-END
                              
                    END-PERFORM
                 END-SEARCH        

                 PERFORM 7070-WRITE-ST-LINE-START 
                    THRU  7075-WRITE-ST-LINE-END
                             
           END-PERFORM.   
       7030-WRITE-DETAIL-END.
           EXIT.

       7040-WRITE-FIN-START.
      * EN bas de page j'affiche les résultats de la classe
           MOVE WS-CLASSE-MOY TO WS-CLASSE-MOY-LIG.
           MOVE ALL SPACE TO REC-F-OUTPUT .
           MOVE ALL SPACE TO WS-LIG-COURS1 .
           STRING
                 WS-LIG-CLASSE
                 WS-LIG-BLANC8  WS-LIG-BLANC8 WS-LIG-BLANC2   
                 WS-CLASSE-MOY-LIG  
                 WS-LIG-BLANC2
                 SPACE
           DELIMITED BY SIZE   
           INTO WS-LIG-COURS1. 
      *    J'ajoute mes moyennes par cours
           PERFORM VARYING WS-IND-ID2 FROM 1 BY 1 
               UNTIL (WS-IND-ID2 > WS-NB-COURS)
                 SET IDX-COURS TO 1
                 SEARCH WS-COURS-TAB 
                    WHEN (WS-ID2-ID(WS-IND-ID2) 
                          = WS-COURS-ID(IDX-COURS))
                       MOVE WS-COURS-MOY(IDX-COURS) 
                             TO WS-COURS-MOY-LIG
                       STRING
                             SPACE
                             FUNCTION TRIM(WS-LIG-COURS1)
                             WS-LIG-BLANC2
                             SPACE  
                             WS-COURS-MOY-LIG
                             SPACE  
                             SPACE 
                       DELIMITED BY SIZE 
                       INTO WS-LIG-COURS1 
                   END-SEARCH           
           END-PERFORM.  

           MOVE WS-LIG-COURS1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT .
           MOVE ALL '=' TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT .
       7055-WRITE-FIN-END.
           EXIT.

       7056-WRITE-LEG-START.           
           PERFORM VARYING WS-IND-CO FROM 1 BY 1 
              UNTIL (WS-IND-CO > WS-NB-COURS)
                 MOVE ALL SPACE TO REC-F-OUTPUT 
                 MOVE WS-IND-CO TO WS-IND-CO-LIG
                 STRING  
                   WS-LIG-COEF
                   FUNCTION TRIM(WS-IND-CO-LIG)
                   WS-LIG-SLASH 
                   WS-COURS-LIB(WS-IND-CO)    
                 DELIMITED BY SIZE 
                 INTO REC-F-OUTPUT            
                 WRITE REC-F-OUTPUT  
           END-PERFORM.   
       7057-WRITE-LEG-END.
           EXIT.

       7058-WRITE-CPT-START.
           MOVE WS-NB-ELEVE TO WS-NB-ELEVE-LIG.
           MOVE WS-NB-COURS TO WS-NB-COURS-LIG.
           MOVE WS-NB-ENREG TO WS-NB-ENREG-LIG.           

           MOVE ALL '=' TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.

           STRING
                 WS-LIG-NBELEVE
                 SPACE  
                 WS-NB-ELEVE-LIG  
                 SPACE
           DELIMITED BY SIZE   
           INTO REC-F-OUTPUT .           
           WRITE REC-F-OUTPUT.
           STRING
                 WS-LIG-NBCOURS
                 SPACE  
                 WS-NB-COURS-LIG  
                 SPACE
           DELIMITED BY SIZE   
           INTO REC-F-OUTPUT .           
           WRITE REC-F-OUTPUT.
                      STRING
                 WS-LIG-NBENREG
                 SPACE  
                 WS-NB-ENREG-LIG  
                 SPACE
           DELIMITED BY SIZE   
           INTO REC-F-OUTPUT .           
           WRITE REC-F-OUTPUT.           
       7058-WRITE-CPT-END.
           MOVE ALL '=' TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           MOVE WS-LIG-FINFIC TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           EXIT.    

       7060-PREPA-C-LINES-START. 
           SET WS-IND-ID2 TO 1.
           PERFORM VARYING WS-IND-CO  FROM 1 BY 1
                 UNTIL (WS-IND-CO > WS-NB-COURS)
              MOVE WS-IND-CO TO WS-IND-CO-LIG
              STRING
                 FUNCTION TRIM(WS-LIG-COURS1)
                 WS-LIG-BLANC3
                 WS-LIG-BLANC3  
                 WS-LIG-COEF 
                 FUNCTION TRIM(WS-IND-CO-LIG)   
              DELIMITED BY SIZE   
              INTO WS-LIG-COURS1    

      *    Chargement des ID des cours dans l'ordre de sortie
      *    Permettra de faire coincider l'ordre des notes
              MOVE WS-COURS-ID(WS-IND-CO) TO WS-ID2-ID(WS-IND-ID2)
              SET WS-IND-ID2 UP BY 1
           END-PERFORM.      
       7065-PREPA-C-LINES-END.   
           EXIT.

       7070-WRITE-ST-LINE-START.
           MOVE ALL SPACE TO WS-LIG-COURS1 .
           MOVE ALL SPACE TO REC-F-OUTPUT  .
           MOVE WS-ELEVE-MOY(WS-IND-EL) TO WS-ELEVE-MOY-LIG.
           STRING
                 
                 SPACE
                 WS-ELEVE-LNAME(WS-IND-EL)
                 WS-ELEVE-FNAME(WS-IND-EL)
                 WS-LIG-BLANC8
                 SPACE
                 SPACE
                 WS-LIG-BLANC2
                 WS-ELEVE-MOY-LIG
                 WS-LIG-BLANC2
                 SPACE
           DELIMITED BY SIZE   
           INTO WS-LIG-COURS1

           PERFORM VARYING WS-IND-ID2 FROM 1 BY 1 
                 UNTIL (WS-IND-ID2 > WS-NB-COURS)
              STRING
                 SPACE
                 FUNCTION TRIM(WS-LIG-COURS1)
                 SPACE 
                 WS-LIG-BLANC2
                 WS-ID2-NOTE(WS-IND-ID2)
                 WS-LIG-BLANC2
                 SPACE
              DELIMITED BY SIZE   
              INTO WS-LIG-COURS1 
           END-PERFORM. 

           INITIALIZE WS-ID2-NOTE(WS-IND-ID2)  .
           MOVE WS-LIG-COURS1 TO REC-F-OUTPUT  .      
           WRITE REC-F-OUTPUT                  .           
       7075-WRITE-ST-LINE-END.
           EXIT.
 
                 
   
