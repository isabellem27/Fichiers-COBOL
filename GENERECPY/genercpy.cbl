      *=============================================================*
      *    Creation d'un fichiere cpy à partir de la desccription   *
      *    d'enregistrement contenue dans le fichier input.txt      *
      *                                                             *      
      *    auteur : Isabelle Marand                                 *
      *    Date création 06/05/2024                                 *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. genercpy.
       AUTHOR . Isabelle Marand.
       
       
       ENVIRONMENT DIVISION.
      * CONFIGURATION SECTION.
                  
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT F-INPUT 
           ASSIGN TO 'input.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUT 
           ASSIGN TO 'output.cpy' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-F-OUT.

           SELECT F-CPY 
           ASSIGN TO 'Entlib.cpy' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-F-CPY.

       DATA DIVISION.
       FILE SECTION. 
       FD  F-INPUT
           RECORD CONTAINS 216 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-INPUT        PIC X(220).

       FD  F-CPY
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-CPY        PIC X(80).    

       FD  F-OUT
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUT        PIC X(80).  

       WORKING-STORAGE SECTION.
      * gestion des status des fichiers  
       01  F-INPUT-STATUS       PIC XX                  .
       88  F-INPUT-STATUS-OK               VALUE '00'   .
       88  F-INPUT-STATUS-EOF              VALUE '10'   .

       01  WS-STAT-F-CPY        PIC XX                  .
       88  WS-STAT-F-CPYOK                 VALUE '00'   .
       88  WS-STAT--CPYDBL                 VALUE '06'   .

       01  WS-STAT-F-OUT        PIC XX                  .
       88  WS-STAT-F-OUTOK                 VALUE '00'   .
       88  WS-STAT--OUTDBL                 VALUE '06'   .
       
       01  WS-REC-INPUT         PIC X(250) VALUE SPACES .
       01  WS-POS               PIC 999                 .
       01  WS-LENGTH            PIC 999                 .
       01  WS-LENGTHCP          PIC 999                 . 
       01  WS-CHAMP             PIC X(20)               .
       01  WS-LENGTH-REC        PIC 999                 .
       01  WS-LENGTHFIL         PIC 99                  .

       01  WS-LIG-RAP           PIC X(250)              .

      * gestion de la sortie pour l'entête de fichier
       01  WS-LIG-GRPEnt      PIC X(80)   VALUE
           '       01 REC-ENTETE .'                                  .  
       01  WS-LIG-DEBFIL      PIC X(37)   VALUE 
           '           03 FILLER            PIC ('                   .
       01  WS-LIG-FINFIL      PIC X(18)   VALUE ')     VALUE SPACE.' .       
       01  WS-LIG-DENT03      PIC X(38)   VALUE 
           '           03 FILLER            PIC X('                   .                    . 
       01  WS-LIG-M2ENT03     PIC X(13)   VALUE ')     VALUE '''     .
       01  WS-LIG-FENT03      PIC X(4)    VALUE '''.'                .    
      
      * gestion de la sortie pour la description de fichier
       01  WS-LIG-GROUPE     PIC X(80)   VALUE
           '       01 REC-FILE .'                                   .         
       01  WS-LIG-DEB03      PIC X(16)   VALUE 
           '           03 R-'                                       .
       01  WS-LIG-MID103     PIC X(9)    VALUE '   PIC X('          . 
       01  WS-LIG-FIN03      PIC X(18)   VALUE ')     VALUE SPACE.' .
            

      ***************************************************************
      * Exécution du programme                                      
      ***************************************************************
       
       PROCEDURE DIVISION .

      * 0000-MAIN-START.
           PERFORM 1000-LECT-FILE-START THRU 1000-LECT-FILE-END.
           PERFORM 2000-ECRIT-SORTIE-START THRU 2000-ECRIT-SORTIE-END.

      * 0000-MAIN-END.    
           STOP RUN.

      * Lecture du fichier client et chargement de la table
       1000-LECT-FILE-START.
      * Ouverture du fichier 
           OPEN INPUT F-INPUT .
           IF (NOT F-INPUT-STATUS-OK) THEN 
              PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END 
           END-IF.
           READ F-INPUT.
           IF (NOT F-INPUT-STATUS-OK) AND (NOT F-INPUT-STATUS-EOF) THEN 
              PERFORM 9000-TEST-STATUT-START THRU 9000-TEST-STATUT-END 
           END-IF.
           MOVE REC-F-INPUT TO WS-REC-INPUT.
       1000-LECT-FILE-END.
           CLOSE F-INPUT.
           EXIT.

       2000-ECRIT-SORTIE-START.
           OPEN OUTPUT  F-CPY  .
           OPEN OUTPUT  F-OUT  .
           PERFORM 3000-ECRIT-CPY-START THRU 3000-ECRIT-CPY-END.
       2000-ECRIT-SORTIE-END.
           CLOSE F-CPY.
           CLOSE F-OUT.
           EXIT.
       
       3000-ECRIT-CPY-START.
              SET WS-POS        TO 1.
              SET WS-LENGTH     TO 
              FUNCTION LENGTH(FUNCTION TRIM(WS-REC-INPUT)).
      * Pour gérer le dernier espace du fichier, j'ajoute 1 à longueur
              ADD 1 TO WS-LENGTH GIVING WS-LENGTH.
              SET WS-LENGTH-REC TO WS-LENGTH.
      * J'écrit l'entète du groupe dans mes cpy        
              MOVE WS-LIG-GRPENT TO REC-F-CPY.
              WRITE REC-F-CPY. 
              MOVE WS-LIG-GROUPE TO REC-F-OUT.
              WRITE REC-F-OUT.   

      * Je descend ma ligne mot par mot.
      * Les mots sont séparés par un ou plusieurs blancs 
      * Pour avancer dans ma ligne je récupère la position du blanc
      * Je positionne après cette position pour démarrer mon inspect
      * sur la longueur de caracteres restante (ws-length). 
      * jusqu'à arriver en fin de ligne.       
              PERFORM UNTIL (WS-LENGTH NOT > 0) 
                          OR (WS-POS > WS-LENGTH-REC)
      *                   OR (WS-POS NOT < WS-LENGTH-REC)
      * Je compte le nombre de caractères positionnés devant le 
      * premier espace
              PERFORM UNTIL (WS-LENGTHCP > 0) OR (WS-LENGTH NOT > 0)
                             OR (WS-POS > WS-LENGTH-REC) 
      *                      OR (WS-POS NOT < WS-LENGTH-REC)

      * tant que la chaine de caractère commence par un blanc
      * j'avance dans ma chaine jusqu'à commencer par des caractères
                    INSPECT WS-REC-INPUT(WS-POS: WS-LENGTH)
                    TALLYING WS-LENGTHCP FOR CHARACTERS 
                    BEFORE SPACE
      * la chaine commence par un blanc, je pointe sur le suivant              
                    IF (WS-LENGTHCP = 0) THEN
                       ADD 1 TO WS-LENGTHFIL GIVING WS-LENGTHFIL
                       ADD 1 TO WS-POS GIVING WS-POS 
                       SUBTRACT 1 FROM WS-LENGTH GIVING WS-LENGTH
                    END-IF
                 END-PERFORM 
      *   J'écrit le filler avec ses x spaces            
                 IF (WS-LENGTHFIL > 0) THEN
                     PERFORM 3020-ECRIT-FILLER-START 
                          THRU 3020-ECRIT-FILLER-END                
                 END-IF
      
                 IF (WS-LENGTHCP > 0) THEN
      *          J'écrit mon champ dans le copy          
                    PERFORM 3010-ECRIT-CHAMP-START 
                          THRU 3010-ECRIT-CHAMP-END      
                 END-IF

      *          Je me positionne sur le caractère suivant le mot traité  
                 ADD WS-LENGTHCP TO WS-POS GIVING WS-POS
                 SUBTRACT WS-POS FROM WS-LENGTH
                       GIVING WS-LENGTH 
                 ADD 1 TO WS-LENGTH  
                 INITIALIZE WS-LENGTHCP 
                 INITIALIZE WS-LENGTHFIL               
              END-PERFORM.                
       3000-ECRIT-CPY-END. 
           EXIT.

       3010-ECRIT-CHAMP-START.
           INITIALIZE REC-F-CPY. 
           INITIALIZE REC-F-OUT.           
           INITIALIZE WS-CHAMP.
           
           MOVE WS-REC-INPUT (WS-POS:WS-LENGTHCP) TO WS-CHAMP.
      * Gestion de l'entète .cpy
           IF (WS-CHAMP NOT = SPACE) THEN
              STRING 
                 WS-LIG-DENT03 
                 WS-LENGTHCP 
                 WS-LIG-M2ENT03
                 FUNCTION TRIM(WS-CHAMP) 
                 WS-LIG-FENT03
              DELIMITED BY SIZE
              INTO REC-F-CPY                         
              WRITE REC-F-CPY                                     
           END-IF. 
      * Gestion de la description .cpy
           IF (WS-CHAMP NOT = SPACE) THEN
              STRING 
                 WS-LIG-DEB03
                 FUNCTION TRIM(WS-CHAMP) 
                 WS-LIG-MID103
                 WS-LENGTHCP             
                 WS-LIG-FIN03
              DELIMITED BY SIZE
              INTO REC-F-OUT                         
              WRITE REC-F-OUT                                     
           END-IF. 
       3010-ECRIT-CHAMP-END.
           EXIT.

       3020-ECRIT-FILLER-START .
           INITIALIZE REC-F-OUT.
           INITIALIZE REC-F-CPY.
           STRING 
                 WS-LIG-DEBFIL
                 WS-LENGTHFIL 
                 WS-LIG-FINFIL
           DELIMITED BY SIZE
           INTO REC-F-CPY . 
      * gestion de l'entête                          
           WRITE REC-F-CPY .
      * gestion de la description
           MOVE REC-F-CPY TO REC-F-OUT.                          
           WRITE REC-F-OUT .

       3020-ECRIT-FILLER-END .
           EXIT.

                          
           COPY TST-STATUT REPLACING ==:FNAME:== BY ==F-INPUT==.
 

