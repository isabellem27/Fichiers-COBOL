      *=============================================================*
      *    CREATION D'UN RAPPORT à L'ECRAN ET SOUS FORME DE FICHIER *
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
       PROGRAM-ID. filelect.
       AUTHOR . Isabelle Marand.
       
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
                  
       SPECIAL-NAMES. 
      * pas de point après comma car une seule phrase avec currency
           DECIMAL-POINT IS COMMA .
      *     CURRENCY SIGN IS X'80' with Picture Symbol '€'.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT FIC-AGA 
           ASSIGN TO 'assurances-part1.dat' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICAGA.

           SELECT FIC-AGA2 
           ASSIGN TO 'assurances-part2.dat' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICAGA2.

           SELECT FIC-AGA-OUT 
           ASSIGN TO 'rapport-assurances.dat' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICAGA-OUT.

       DATA DIVISION.
       FILE SECTION. 
      * le fichier fait 119 caractères, + 1 déclaré pour retour chariot
      *
      * le fichier contient des caractères spéciaux donc rend la 
      * longueur d'enregistrement variable.

      * Gestion du fichier variable.  
       FD FIC-AGA
           RECORD CONTAINS 120 TO 150 CHARACTERS
           RECORDING MODE IS V.
       01 E-REC-AGA PIC X(150).

       FD FIC-AGA2 
           RECORD CONTAINS 120 TO 150 CHARACTERS
           RECORDING MODE IS V.
       01 E-REC-AGA2 PIC X(150).

       FD FIC-AGA-OUT
           RECORD CONTAINS 150 CHARACTERS
           RECORDING MODE IS F.
       01 E-REC-AGA-OUT PIC X(150).
       
       01 ENR-AGA-OUT REDEFINES E-REC-AGA-OUT.   
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGAID 	    PIC 9(8)         . 
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGAGRP     PIC X(14)        .
            05 FILLER        PIC X(3)   VALUE ' | '.
      * Ajout 2 car. car contient é
            05 WS-AGATYP     PIC X(16)        .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGALIB     PIC X(41)         .
            05 FILLER        PIC X(3)   VALUE ' | '.
      * Ajout 2 car. car contient é
            05 WS-CLACT      PIC X(10)         .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGADDEB                      .
              10 WS-DDYYYY   PIC X(4)          .
              10 WS-DDMM     PIC X(2)          .
              10 WS-DDDD     PIC X(2)          .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGADFIN                      .
              10 WS-DFYYYY   PIC X(4)          .
              10 WS-DFMM     PIC X(2)          .
              10 WS-DFDD     PIC X(2)          . 
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGAMNT 	  PIC 9(6)V99 	     .
            05 FILLER        PIC X(3)   VALUE ' | '.
      * Le caractère € prend 3 positions      
            05 WS-AGCURR 	  PIC X(3) VALUE '€'.
            05 FILLER        PIC X(3)   VALUE ' | '.

       WORKING-STORAGE SECTION.
      * gestion des status des fichiers  
       01  WS-STAT-FICAGA       PIC XX                  .
       88  WS-STAT-FICOK                     VALUE '00'   .
       88  WS-STAT-FICFIN                    VALUE '10'   .
       01  WS-STAT-FICAGA2      PIC XX                  .
       88  WS-STAT-FICOK2                    VALUE '00'   .
       88  WS-STAT-FICFIN2                   VALUE '10'   .
       01  WS-STAT-FICAGA-OUT   PIC XX                  .
       88  WS-STAT-OFICOK                     VALUE '00'  .
       88  WS-STAT-OFICDBL                    VALUE '06'  .

      * gestion de la boucle de lecture et d'écriture des fichiers
       01  WS-E-REC             PIC X(250)              .
       01  WS-NUM-FILE          PIC 9          VALUE 0      . 
       01  WS-NB-ENREG          PIC 99         VALUE 0      .
       01  WS-NB-ENREG-CHAR     REDEFINES WS-NB-ENREG       PIC ZZ .  
       01  WS-NB-ENREG2         PIC 99         VALUE 0      .
       01  WS-NB-ENREG2-CHAR    REDEFINES WS-NB-ENREG2      PIC ZZ . 
       01  WS-NB-ENREGTOT       PIC 99         VALUE 0      .
       01  WS-NB-ENREGTOT-CHAR  REDEFINES WS-NB-ENREGTOT    PIC ZZ . 
       01  WS-IND-ENREG         PIC 99         VALUE 1      .
       01  WS-NB-TYPACT         PIC 99         VALUE 0      .
       01  WS-NB-TYPACT-CHAR    REDEFINES WS-NB-TYPACT      PIC ZZ . 
       01  WS-AMNT-TYPACT        PIC 9(5)V99   VALUE 0      .
      * 01  WS-AMNT-TYPACT-CHAR  PIC Z(4)9,99 .
       01  WS-NB-TYPRESIL        PIC 99        VALUE 0      .
       01  WS-NB-TYPRESIL-CHAR  REDEFINES WS-NB-TYPRESIL    PIC ZZ . 
       01  WS-AMNT-TYPRESIL      PIC 9(5)V99   VALUE 0      .
      * 01  WS-AMNT-TYPRESIL-CHAR 
      *                         REDEFINES WS-AMNT-TYPRESIL PIC Z(4)9,99 .
       01  WS-NB-TYPSUSP         PIC 99        VALUE 0      .
       01  WS-NB-TYPSUSP-CHAR   REDEFINES WS-NB-TYPSUSP     PIC ZZ . 
       01  WS-AMNT-TYPSUSP       PIC 9(5)V99   VALUE 0      .
      * 01  WS-AMNT-TYPSUSP-CHAR REDEFINES WS-AMNT-TYPSUSP PIC Z(4)9,99 . 
       01  WS-AMNT-TYP           PIC 9(5)V99   VALUE 0      .
       01  WS-AMNT-TYP-CHAR      PIC Z(4)9,99 .

       01  WS-IND-TAB           PIC 99         VALUE 0    .  
       01  WS-TAB-ENREG.
      *     03 DEF-TAB        PIC 99     VALUE 85   .
      *     03 ENR-AGA OCCURS  1 TO 99 DEPENDING DEF-TAB. 
           03 ENR-AGA     OCCURS  72 TIMES.   
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGAID 	    PIC 9(8)              . 
            05 FILLER        PIC X(3)   VALUE ' | '.        
            05 WS-AGAGRP     PIC X(14)             .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGATYP     PIC X(16)             .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-AGALIB     PIC X(41)             .
            05 FILLER        PIC X(3)   VALUE ' | '.
            05 WS-CLACT      PIC X(10)             .
            05 FIL1          PIC X(3)   VALUE ' | '.
      *      66 WS-AGA-INFO1 RENAMES WS-AGAID THRU FIL1 .
            05 WS-AGADDEB                          .
              10 WS-DDYYYY   PIC X(4)              .
              10 WS-DDMM     PIC X(2)              .
              10 WS-DDDD     PIC X(2)              .
            05 FIL2          PIC X(3)   VALUE ' | '.
            05 WS-AGADFIN                          .
              10 WS-DFYYYY   PIC X(4)              .
              10 WS-DFMM     PIC X(2)              .
              10 WS-DFDD     PIC X(2)              . 
            05 FIL3          PIC X(3)   VALUE ' | '.
            05 WS-AGAMNT     PIC 9(6)V99 	      .
            05 FILLER        PIC X(3)   VALUE ' | '.
      * Le caractère € prend 3 positions mais décalage avec accents     
            05 WS-AGCURR     PIC X(3)   VALUE '€'  .
            05 FIL4          PIC X(3)   VALUE ' | '.
           
      * gestion du rapport
       01  WS-LIG-ETOILE     PIC X(160)                            .
       01  WS-LIG-BLC        PIC X(25)                             . 
       01  WS-LIG-RAP        PIC X(140)  
           VALUE '                     ETAT DES CONTRATS'          .             
       01  WS-LIG-FIL1        PIC X(40)  
           VALUE 'NB ENREGISTREMENTS DANS LE FICHIER 1 : ' .
       01  WS-LIG-FIL2        PIC X(40)  
           VALUE 'NB ENREGISTREMENTS DANS LE FICHIER 2 : ' .
       01  WS-LIG-FIN1       PIC X(45)  
           VALUE 'THEN END!     NOMBRE TOTAL D''ENREGISTEMRENTS:'.              . 
      * 01  WS-LIG-FIN2       PIC X(17)   VALUE ' ENREGISTREMENTS ' . 
       01  WS-LIG-EURO       PIC X(3)    VALUE '€'                 . 
       01  WS-LIG-DET1       PIC X(12)   VALUE '     ENREG. '      . 
       01  WS-LIG-DET2       PIC X(3)    VALUE ' : '               .
       01  WS-LIG-NBACT      PIC X(13)   VALUE ' NB ACTIFS : '     .
       01  WS-LIG-NBSUSP     PIC X(16)   VALUE ' NB SUSPENDUS : '  .
       01  WS-LIG-NBRESIL    PIC X(15)   VALUE ' NB RESILIES : '   .
       01  WS-LIG-NOUVACT    PIC X(10)   VALUE ' STATUT : '        .
       01  WS-LIG-NOUVAMNT   PIC X(10)   VALUE ' VALEUR : '        .
       01  WS-LIG-NOUVLIB    PIC X(10)   VALUE 'CONTRAT : '        .
       01  WS-LIB-AGALIB     PIC X(41)   VALUE SPACE               .
       01  WS-LIB-CLATC      PIC X(10)   VALUE SPACE               .



      ***************************************************************
      * Exécution du programme                                      
      ***************************************************************
       
       PROCEDURE DIVISION .

      * 0000-MAIN-START

      * Ouverture des fichiers
           OPEN INPUT FIC-AGA .
           IF (NOT WS-STAT-FICOK) THEN 
              PERFORM TEST-STATUT
           END-IF.

           OPEN INPUT FIC-AGA2 .
           IF (NOT WS-STAT-FICOK2) THEN 
              PERFORM TEST-STATUT
           END-IF.



      * Lecture 1 du fichier1
           MOVE 1 TO WS-NUM-FILE.
           PERFORM LECT-FILE.

      *Boucle de lecture sur le fichier1  
           PERFORM VARYING WS-IND-ENREG FROM 1 BY 1 
                   UNTIL WS-STAT-FICFIN 
              PERFORM LECT-FILE
           END-PERFORM. 
          
           IF WS-STAT-FICFIN THEN
      *Le fichier 1 est terminé.
      * Boucle de lecture sur le fichier2  
              MOVE 2 TO WS-NUM-FILE 
              PERFORM LECT-FILE
              PERFORM VARYING WS-IND-ENREG FROM 1 BY 1 
                      UNTIL WS-STAT-FICFIN2 
                 PERFORM LECT-FILE
              END-PERFORM
      * J'ai lu tous les enregs je peux finir de charger la table 
      * avec le nombre d'enregistrements puis trier et écrire le fichier
      *         MOVE WS-NB-ENREG TO DEF-TAB  
              PERFORM TRIER-TAB
           END-IF.

      * Fermeture des fichiers
           CLOSE FIC-AGA.
           CLOSE FIC-AGA2.
           PERFORM ECRIT-SORTIE.

      * Test du status 
       TEST-STATUT.
           IF (NOT WS-STAT-FICOK) AND (NOT WS-STAT-FICFIN) THEN 
              MOVE ALL  '/' TO WS-LIG-ETOILE
              DISPLAY WS-LIG-ETOILE 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE WS-STAT-FICAGA 
              MOVE ALL  '/' TO WS-LIG-ETOILE
              DISPLAY WS-LIG-ETOILE 
           END-IF.           
      * 0000-MAIN-END    
           STOP RUN.

      * Lecture du fichier
       LECT-FILE.
           IF (WS-NUM-FILE = 1) THEN
              READ FIC-AGA
              IF (NOT WS-STAT-FICOK) AND (NOT WS-STAT-FICFIN) THEN 
                 PERFORM TEST-STATUT
              ELSE 
                 IF WS-STAT-FICOK THEN
                    ADD 1 TO WS-NB-ENREG 
                    PERFORM CHARGE-STRUCT
                 END-IF   
              END-IF
           ELSE 
              READ FIC-AGA2 
              IF (NOT WS-STAT-FICOK2) AND (NOT WS-STAT-FICFIN2) THEN 
                 PERFORM TEST-STATUT
              ELSE 
                 IF WS-STAT-FICOK2 THEN
                    ADD 1 TO WS-NB-ENREG2  
                    PERFORM CHARGE-STRUCT
                 END-IF   
              END-IF           
           END-IF.              

      
       CHARGE-STRUCT.

           IF WS-NUM-FILE = 1 THEN
               MOVE WS-NB-ENREG TO WS-IND-TAB
               MOVE E-REC-AGA TO WS-E-REC
           ELSE
               COMPUTE WS-NB-ENREGTOT = WS-NB-ENREG +WS-NB-ENREG2 
               MOVE WS-NB-ENREGTOT TO WS-IND-TAB
               MOVE E-REC-AGA2 TO WS-E-REC
           END-IF.

           UNSTRING WS-E-REC  
           DELIMITED BY '*' 
           INTO  WS-AGAID     OF ENR-AGA(WS-IND-TAB)  
                 WS-AGAGRP    OF ENR-AGA(WS-IND-TAB)  
                 WS-AGATYP    OF ENR-AGA(WS-IND-TAB)   
                 WS-AGALIB    OF ENR-AGA(WS-IND-TAB)   
                 WS-CLACT     OF ENR-AGA(WS-IND-TAB)    
                 WS-AGADDEB   OF ENR-AGA(WS-IND-TAB)  
                 WS-AGADFIN   OF ENR-AGA(WS-IND-TAB)  
                 WS-AGAMNT    OF ENR-AGA(WS-IND-TAB)   
                 WS-AGCURR    OF ENR-AGA(WS-IND-TAB)   .
           
           EVALUATE FUNCTION UPPER-CASE (
                    FUNCTION TRIM(WS-CLACT OF ENR-AGA(WS-IND-TAB) )
                                         )
              WHEN 'ACTIF'
                 ADD 1 TO WS-NB-TYPACT
                 COMPUTE WS-AMNT-TYPACT = WS-AMNT-TYPACT 
                          + WS-AGAMNT OF ENR-AGA(WS-IND-TAB)
              WHEN 'SUSPENDU'
                 ADD 1 TO WS-NB-TYPSUSP  
                 COMPUTE WS-AMNT-TYPSUSP = WS-AMNT-TYPSUSP 
                          + WS-AGAMNT OF ENR-AGA(WS-IND-TAB)
              WHEN OTHER
                 ADD 1 TO WS-NB-TYPRESIL  
                 COMPUTE WS-AMNT-TYPRESIL = WS-AMNT-TYPRESIL 
                          + WS-AGAMNT OF ENR-AGA(WS-IND-TAB)
              END-EVALUATE.

       TRIER-TAB.
           SORT ENR-AGA ASCENDING 
           KEY WS-CLACT OF WS-TAB-ENREG  WS-AGALIB OF WS-TAB-ENREG.

       ECRIT-SORTIE.
           OPEN OUTPUT  FIC-AGA-OUT .
           PERFORM ECRIT-ENTETE THRU ECRIT-FIN.
           CLOSE FIC-AGA-OUT.

       ECRIT-ENTETE.    
      *  Ecriture entête du rapport
           MOVE ALL '=' TO WS-LIG-ETOILE.
           MOVE ALL SPACE TO WS-LIG-BLC .
           DISPLAY WS-LIG-ETOILE.
           MOVE WS-LIG-ETOILE TO E-REC-AGA-OUT.
           WRITE E-REC-AGA-OUT.

           PERFORM ECRIT-LIGNE.
           
           STRING WS-LIG-FIL1 
                  FUNCTION TRIM(WS-NB-ENREG-CHAR) 
           DELIMITED BY SIZE        
           INTO WS-LIG-RAP.               
           PERFORM ECRIT-LIGNE.
           DISPLAY WS-LIG-FIL1 ' ' FUNCTION TRIM(WS-NB-ENREG-CHAR).

           STRING WS-LIG-FIL2 
                  WS-NB-ENREG2-CHAR 
           DELIMITED BY SIZE        
           INTO WS-LIG-RAP.               
           PERFORM ECRIT-LIGNE .

           MOVE WS-LIG-ETOILE TO WS-LIG-RAP.
           PERFORM ECRIT-LIGNE.

       ECRIT-DETAIL.
           INITIALIZE WS-LIB-AGALIB .
           INITIALIZE WS-LIB-CLATC .

      *    Je descends la table pour écrire et afficher mes données 
           PERFORM VARYING WS-IND-TAB FROM 1 BY 1 
                   UNTIL (WS-IND-TAB > WS-NB-ENREGTOT)
      * SI je change de type d'actif, je mets une entete specifique
      * qui contient le nouveau libellé et la valeur totale des contrats
      * associés à ce type.
              INITIALIZE WS-LIG-RAP 
           
              IF (WS-LIB-CLATC NOT = WS-CLACT OF ENR-AGA(WS-IND-TAB)) 
              THEN
                 MOVE WS-CLACT OF ENR-AGA(WS-IND-TAB) TO WS-LIB-CLATC
                 STRING WS-LIG-BLC 
                      WS-LIG-NOUVACT 
                      WS-LIB-CLATC 
                 DELIMITED BY SIZE        
                 INTO WS-LIG-RAP               
                 PERFORM ECRIT-LIGNE  

                 EVALUATE WS-LIG-NOUVACT    
                    WHEN 'ACTIF'
                       MOVE WS-AMNT-TYPACT TO WS-AMNT-TYP-CHAR
                    WHEN 'SUSPENDU'
                       MOVE WS-AMNT-TYPSUSP TO WS-AMNT-TYP-CHAR 
                    WHEN OTHER
                       MOVE WS-AMNT-TYPRESIL TO WS-AMNT-TYP-CHAR
                 END-EVALUATE
                 
                 STRING WS-LIG-BLC 
                      WS-LIG-NOUVAMNT 
                      WS-AMNT-TYP-CHAR
                      WS-LIG-EURO  
                 DELIMITED BY SIZE        
                 INTO WS-LIG-RAP
                 PERFORM ECRIT-LIGNE
              END-IF

      * SI je change de libellé de contrat, je mets une entete specif.             
              IF (WS-LIB-AGALIB NOT = WS-AGALIB OF ENR-AGA(WS-IND-TAB)) 
              THEN
                 MOVE WS-AGALIB OF ENR-AGA(WS-IND-TAB) TO WS-LIB-AGALIB
                 STRING WS-LIG-BLC 
                      WS-LIG-NOUVLIB 
                      WS-LIB-AGALIB 
                 DELIMITED BY SIZE        
                 INTO WS-LIG-RAP

                 PERFORM ECRIT-LIGNE                                    
               END-IF
              
              INITIALIZE WS-LIG-RAP
              INITIALIZE E-REC-AGA-OUT
              MOVE CORR ENR-AGA(WS-IND-TAB) TO ENR-AGA-OUT 
              WRITE E-REC-AGA-OUT  

              DISPLAY '  ENREG' SPACE WS-IND-TAB SPACE ':' 
              SPACE ENR-AGA(WS-IND-TAB)
              MOVE ALL  '-' TO WS-LIG-ETOILE
              DISPLAY WS-LIG-ETOILE 
              MOVE WS-LIG-ETOILE TO WS-LIG-RAP  
              PERFORM ECRIT-LIGNE 
           END-PERFORM.   

       ECRIT-FIN.
      * Je dis à tous que je suis arrivée au bout!!
               MOVE ALL  '=' TO WS-LIG-ETOILE.
               MOVE WS-LIG-ETOILE TO WS-LIG-RAP.
               PERFORM ECRIT-LIGNE.              
               DISPLAY WS-LIG-ETOILE.
      * Donner le nb d'enreg.
      * Delimited by size --> size de la variable pas de la valeur
               INITIALIZE WS-LIG-RAP.
               STRING WS-LIG-BLC 
                      WS-LIG-FIN1 
                      WS-NB-ENREGTOT-CHAR 
                      WS-LIG-NBACT
                      WS-NB-TYPACT-CHAR 
                      WS-LIG-NBSUSP
                      WS-NB-TYPSUSP-CHAR
                      WS-LIG-NBRESIL
                      WS-NB-TYPRESIL-CHAR                                             
               DELIMITED BY SIZE        
               INTO WS-LIG-RAP .

               PERFORM ECRIT-LIGNE.
               DISPLAY WS-LIG-RAP.
               
               MOVE ALL  '=' TO WS-LIG-ETOILE.
               MOVE WS-LIG-ETOILE TO WS-LIG-RAP.
               PERFORM ECRIT-LIGNE . 
               DISPLAY WS-LIG-ETOILE .
      
       ECRIT-LIGNE.
                 INITIALIZE E-REC-AGA-OUT .
                 MOVE WS-LIG-RAP TO E-REC-AGA-OUT .
                 WRITE E-REC-AGA-OUT.  


