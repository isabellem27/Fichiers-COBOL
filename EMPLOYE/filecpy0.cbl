      *=============================================================*
      *    GESTION DESCRIPTIONS DE FICHIERS VIA COPY                *
      *       LECTURE DES FICHIERS 1 ET 2                           *   
      *       CHARGEMENT D'UNE TABLE DE TRAVAIL                     *
      *       TRI DE LA TABLE SUR LES CHAMPS STATUT ET LIBELLE      *
      *       AFFICHAGE  DES ENREGISTREMENTS                        *
      *    ECRITURE DES ENREGISTREMENTS DANS LE FICHIER DE SORTIE   *
      *    PAR SECTION STATUT ET LIBELLE (ENTETE)                   *
      *                                                             *      
      *    auteur : Isabelle Marand                                 *
      *    Date création 12/04/2024                                 *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. filecpy0.
       AUTHOR . Isabelle Marand.
       
       
       ENVIRONMENT DIVISION.
      * CONFIGURATION SECTION.
                  
      * SPECIAL-NAMES. 
      * pas de point après comma car une seule phrase avec currency
      *     DECIMAL-POINT IS COMMA .
      *     CURRENCY SIGN IS X'80' with Picture Symbol '€'.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT F-EMPLOYE 
           ASSIGN TO 'FichierClient.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FEMP.

           SELECT F-DEPT 
           ASSIGN TO 'FR-LISTE-DEPT.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FDEPT.

           SELECT F-CLISOR 
           ASSIGN TO 'Employe.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FOUT.

       DATA DIVISION.
       FILE SECTION. 
      
      * Récupération du FD et description fichier client
      * dans FCLIENT     
       COPY  'FCLIENT.cpy' REPLACING ==:CLIENT:== BY ==EMPLOYE==.
  
      * Idem pour Département et fichier de sortie      
       COPY 'FDEPT.cpy'.
       
       COPY 'FCLISORTIE.cpy'. 


       WORKING-STORAGE SECTION.
      * gestion des status des fichiers  
       01  WS-STAT-FEMP       PIC XX                    .
       88  WS-STAT-FEMPOK                  VALUE '00'   .
       88  WS-STAT-FEMPFIN                 VALUE '10'   .
       01  WS-STAT-FDEPT      PIC XX                    .
       88  WS-STAT-FDEPTOK                 VALUE '00'   .
       88  WS-STAT-FDEPTFIN                VALUE '10'   .
       01  WS-STAT-FOUT       PIC XX                    .
       88  WS-STAT-FOUTOK                  VALUE '00'   .
       88  WS-STAT-FOUTDBL                 VALUE '06'   .

      * gestion de la boucle de lecture et d'écriture des fichiers
       01  WS-E-REC             PIC X(250)              .
       01  WS-NB-ENREG          PIC 99         VALUE 0      .
       01  WS-NB-ENREG-CHAR     REDEFINES WS-NB-ENREG       PIC ZZ .  
       01  WS-IND-ENREG         PIC 99         VALUE 1      .
       01  WS-AMNT-TOT           PIC 9(6)V99   VALUE 0      .
       01  WS-AMNT-TOT-CHAR      PIC Z(5)9.99               .

       01  WS-IND-TAB           PIC 99         VALUE 0    .  
       01  WS-TAB-ENREG.
           03 ENR-AGA     OCCURS  20 TIMES.   
            05 WS-EMPID      PIC X(10)             . 
      *     05 FILLER        PIC X(3)   VALUE ' | '.        
            05 WS-EMPNOM     PIC X(20)             .
      *      05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-EMPPREN    PIC X(20)             .
      *      05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-EMPPOST    PIC X(20)             .
      *      05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-EMPSAL     PIC X(7)              .
            05 WS-EMPSAL-NUM REDEFINES WS-EMPSAL PIC 9(4)V99   .
      *      05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-EMPAG      PIC X(3)  	         .
      *      05 FILLER        PIC X(3)   VALUE ' | '.     
            05 WS-EMPDEPT    PIC X(26)             .
      *      05 FILLER        PIC X(3)   VALUE ' | '.     
            05 WS-EMPREG     PIC X(26)             .
  
           
      * gestion du rapport
           COPY FRENTETE.
      * 01  R-LIG-ENTETE    REDIFINES R-ENTETE.
      *     05 WS-LIG-ENTETE   PIC X(140)                .   
      * 01  WS-LIG-ETOILE     PIC X(160)    .
       01  WS-LIG-BLC         PIC X(11)  VALUE SPACES            . 
       01  WS-LIG-RAP         PIC X(140) VALUE SPACES            .         
       01  WS-LIG-FIL1        PIC X(21)  VALUE 'Le salaire '     .
       01  WS-LIG-FIL2        PIC X(21)  VALUE 'Cumulé est de '  . 
       01  WS-LIG-FIL3        PIC X(21)  VALUE '************* '  .
       01  WS-LIG-EURO        PIC X(4)   VALUE ' €'              .      

      ***************************************************************
      * Exécution du programme                                      
      ***************************************************************
       
       PROCEDURE DIVISION .

      * 0000-MAIN-START

      * Ouverture des fichiers
           OPEN INPUT F-EMPLOYE .
           IF (NOT WS-STAT-FEMPOK) THEN 
              PERFORM TEST-STATUT
           END-IF.

      * Lecture du fichier employe, chargement dans la table de travail
           INITIALIZE WS-AMNT-TOT.
           PERFORM UNTIL WS-STAT-FEMPFIN 
              PERFORM LECT-FILE
           END-PERFORM.
           PERFORM TRIER-TAB.
      * Plus besoin du fichier employe, on ferme
           CLOSE F-EMPLOYE.

      * Lecture du fichier departement 
      * et chargement du département/région dans la table de travail
           OPEN INPUT F-DEPT .
           IF (NOT WS-STAT-FDEPTOK) THEN 
              PERFORM TEST-STATUT
           END-IF.
      *Boucle de lecture sur le fichier
           MOVE 1 TO WS-IND-TAB.
           PERFORM LECT-FDEPT. 
            
           CLOSE F-DEPT.

           PERFORM ECRIT-SORTIE.

      * Test du status 
       TEST-STATUT.
           IF (NOT WS-STAT-FEMPOK) AND (NOT WS-STAT-FEMPFIN) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE WS-STAT-FEMP 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
           END-IF.           
      * 0000-MAIN-END    
           STOP RUN.

      * Lecture du fichier client et chargement de la table
       LECT-FILE.
           
           READ F-EMPLOYE
              IF (NOT WS-STAT-FEMPOK) AND (NOT WS-STAT-FEMPFIN) THEN 
                 PERFORM TEST-STATUT
              ELSE 
                 IF WS-STAT-FEMPOK THEN
                    ADD 1 TO WS-IND-TAB 
                    PERFORM CHARGE-STRUCT-CLI
                 END-IF   
              END-IF.
             
       TRIER-TAB.
      * On trie sur le code dept pour charger la suite 
           SORT ENR-AGA ASCENDING 
           KEY WS-EMPAG OF WS-TAB-ENREG.

       LECT-FDEPT.
      * On descend la table et le fichier en parallele pour compléter
      * les informations de la table
           MOVE WS-IND-TAB TO WS-IND-ENREG
           READ F-DEPT     
           IF (NOT WS-STAT-FDEPTOK) AND (NOT WS-STAT-FDEPTFIN) THEN 
                 PERFORM TEST-STATUT
           ELSE 
              IF WS-STAT-FDEPTOK THEN 
      * je descend ma table pour trouver le début des employés 
      * liés au dept du fichier
                 PERFORM CHERCH-DANS-TABLE
                 IF (WS-EMPAG OF ENR-AGA(WS-IND-TAB) > RDEPT-ID) THEN
      * Pas trouvé, je recherche dans le fichier l'égalité 
      * sur le code département
                    PERFORM CHERCH-DANS-FIC
                             
                 END-IF
                 IF (WS-EMPAG OF ENR-AGA(WS-IND-TAB) = RDEPT-ID) THEN 
                    PERFORM CHARGE-STRUCT-DEPT
                 END-IF    
           END-IF .              

       CHERCH-DANS-TABLE.
      * Je redemarre au dernier indice sauvegardé dans WS-IND-ENREG 
           PERFORM VARYING WS-IND-TAB FROM WS-IND-ENREG BY 1 
                UNTIL (WS-IND-TAB > 20) 
                   OR (WS-EMPAG OF ENR-AGA(WS-IND-TAB) = RDEPT-ID)
                   OR (WS-EMPAG OF ENR-AGA(WS-IND-TAB) > RDEPT-ID)
      
      * Impossible de faire 1 perform sans 1 instruction dans la boucle             
                   MOVE WS-IND-TAB TO WS-IND-ENREG 
                    
           END-PERFORM.

       CHERCH-DANS-FIC.
           PERFORM 
               UNTIL (WS-EMPAG OF ENR-AGA(WS-IND-TAB)= RDEPT-ID)
                 OR WS-STAT-FDEPTFIN
                    READ F-DEPT
           END-PERFORM.

       CHARGE-STRUCT-CLI.
           
      * Pas de délimitateur donc impossible d'utiliser un unstring
      * Donc on utilise 1 move sur chaque champs 
           MOVE REMPLOYE-ID      TO WS-EMPID   OF ENR-AGA(WS-IND-TAB). 
           MOVE REMPLOYE-NOM     TO WS-EMPNOM  OF ENR-AGA(WS-IND-TAB).
           MOVE REMPLOYE-PRENOM  TO WS-EMPPREN OF ENR-AGA(WS-IND-TAB). 
           MOVE REMPLOYE-POSTE   TO WS-EMPPOST OF ENR-AGA(WS-IND-TAB). 
           MOVE REMPLOYE-SALAIRE TO WS-EMPSAL  OF ENR-AGA(WS-IND-TAB).  
           MOVE REMPLOYE-AGENCE  TO WS-EMPAG   OF ENR-AGA(WS-IND-TAB).
           
           COMPUTE WS-AMNT-TOT = WS-AMNT-TOT 
                           + WS-EMPSAL-NUM OF ENR-AGA(WS-IND-TAB)  .          
      
       CHARGE-STRUCT-DEPT.
      *     MOVE RDEPT TO WS-E-REC. 
           MOVE WS-IND-TAB TO WS-IND-ENREG.
           PERFORM VARYING WS-IND-TAB FROM WS-IND-ENREG BY 1 
              UNTIL (WS-EMPAG OF ENR-AGA(WS-IND-TAB) > RDEPT-ID)
              OR (WS-IND-TAB > 20)

                 MOVE RDEPT-DEP  
                    TO WS-EMPDEPT   OF ENR-AGA(WS-IND-TAB) 
                 MOVE RDEPT-REGION  
                    TO WS-EMPREG   OF ENR-AGA(WS-IND-TAB) 
            
           END-PERFORM.
           IF (WS-IND-TAB <= 20) THEN PERFORM LECT-FDEPT.    


       ECRIT-SORTIE.
           OPEN OUTPUT  F-CLISOR  .
           PERFORM ECRIT-ENTETE THRU ECRIT-FIN.
           CLOSE F-CLISOR.


       ECRIT-ENTETE.    
      *  Ecriture entête du rapport                  
           MOVE R-ENTETE TO WS-LIG-RAP.   
           PERFORM ECRIT-LIGNE.
 
       ECRIT-DETAIL.
           INITIALIZE WS-LIG-RAP.
           MOVE ALL  '-' TO WS-LIG-RAP.
           PERFORM ECRIT-LIGNE. 
      *    Je descends la table pour écrire mes données 
           PERFORM VARYING WS-IND-TAB FROM 1 BY 1 
                    UNTIL (WS-IND-TAB > 20)
      
      * move all pour gérer la mise à blanc des fillers
      * avec initialize, seuls les champs nommés sont réinitialisés
              MOVE ALL SPACE TO R-CLISOR

              MOVE WS-EMPID   OF ENR-AGA(WS-IND-TAB) TO RCLISOR-ID
              MOVE WS-EMPNOM  OF ENR-AGA(WS-IND-TAB) TO RCLISOR-NOM     
              MOVE WS-EMPPREN OF ENR-AGA(WS-IND-TAB) TO RCLISOR-PRENOM
              MOVE WS-EMPPOST OF ENR-AGA(WS-IND-TAB) TO RCLISOR-POSTE
              MOVE WS-EMPSAL  OF ENR-AGA(WS-IND-TAB) 
                                                  TO RCLISOR-SALAIRE-V
              MOVE WS-EMPAG   OF ENR-AGA(WS-IND-TAB) TO RCLISOR-AGENCE
              MOVE WS-EMPDEPT OF ENR-AGA(WS-IND-TAB) TO RCLISOR-DEPART
              MOVE WS-EMPREG  OF ENR-AGA(WS-IND-TAB) TO RCLISOR-REGION
              
              WRITE R-CLISOR 

           END-PERFORM.       

       ECRIT-FIN.
           MOVE ALL  '=' TO WS-LIG-RAP.
           PERFORM ECRIT-LIGNE.              

      * Donner le montant total des salaires
           MOVE WS-AMNT-TOT TO WS-AMNT-TOT-CHAR. 
           INITIALIZE WS-LIG-RAP.
           STRING WS-LIG-BLC 
                  WS-LIG-FIL1
                  WS-LIG-FIL2              
                  WS-LIG-FIL3
                  WS-AMNT-TOT-CHAR 
                  WS-LIG-EURO                                         
           DELIMITED BY SIZE        
           INTO WS-LIG-RAP .
           PERFORM ECRIT-LIGNE.
               
           MOVE ALL  '=' TO WS-LIG-RAP.
           PERFORM ECRIT-LIGNE.   
      
       ECRIT-LIGNE.
                 INITIALIZE R-CLISOR .

                 MOVE WS-LIG-RAP TO R-CLISOR .

                 WRITE R-CLISOR.  


