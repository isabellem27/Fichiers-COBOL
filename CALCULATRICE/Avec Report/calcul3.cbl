      *=============================================================*
      * CALCULATRICE:                                               *
      *    L'objectif: Concevoir un programme permettant d'imiter le*
      *                fonctionnement d'une calculatrice.           *
      *    Les étapes:                                              *        
      *    Je demande à l'utilisateur de saisir l'opération qu'il   *
      *    veut faire, puis les chiffres qu'il veut mettre en oeuvre*
      *    je teste les données reçues et boucle sur la saisie si ko*
      *    Je calcule le résultat                                   *
      *    J'affiche l'opération et le résultat .                   *
      *                                                             *
      *    MAJv1: Ajout de l'affichage de la date au format français*
      *                                                             *
      *    MAJv2:  Je modifie mon code pour accepter les décimales  *
      *             dans le résultat des calculs et les signer.     *
      *            J'ajoute des tests de saisie supplémentaires     *
      *            J'ajoute une réinitialisation                    *
      *            J'ajoute un fichier de sortie pour récapituler   * 
      *            les opérations successives.                      *      
      *                                                             *   
      *    auteur : Isabelle Marand                                 *
      *    Date création  09/04/2024                                *
      *    Date MAJv1:    10/04/2024                                *
      *    Date MAJv2:    20/04/2024                                *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcul3.
       AUTHOR. Isabelle Marand.
       
      * MAJv2: Gestion du fichieer de sortie 
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT FIC-OUT 
           ASSIGN TO 'Calc-Report.txt' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FIC-OUT.

       DATA DIVISION.
       FILE SECTION.   
           FD FIC-OUT
           RECORD CONTAINS 230 CHARACTERS
           RECORDING MODE IS F.
       01 E-REC-OUT PIC X(230).
       01 ENR-OUT REDEFINES E-REC-OUT. 
           03 ENR-TAB      OCCURS 6.     
              05 FS-DEB         PIC X(3)   VALUE ' ! '      .
              05 FS-SNUM1       PIC +Z(4)9.99               . 
              05 FS-NUM1 REDEFINES FS-SNUM1 PIC Z(4)9.99    .
              05 FILLER         PIC X(1)   VALUE ' '        .
              05 FS-SIGN        PIC X(1)                    .
              05 FILLER         PIC X(1)   VALUE ' '        .
              05 FS-SNUM2 	    PIC +Z(3).99                . 
              05 FS-NUM2 REDEFINES FS-SNUM2 PIC Z(3).99     .
              05 FS-EQUAL       PIC X(3)   VALUE ' = '      .
              05 FS-SRESULT     PIC +Z(4)9.99               . 
              05 FS-RESULT REDEFINES FS-SRESULT PIC Z(4)9.99.
              05 FS-FIN         PIC X(2)   VALUE ' |'       . 


       WORKING-STORAGE SECTION.
      * MAJv2: gestion des status des fichiers  
       
       01  WS-STAT-FIC-OUT   PIC XX               .
       88  WS-STAT-OFICOK             VALUE '00'  .
       88  WS-STAT-OFICDBL            VALUE '06'  .
      * MAJv2: Chaque enregistrement du fichier contient 6 opérations 
      * sauf si réinitialisation ou fin de saisie avant atteinte du max.
       01  WS-NB-CALC        PIC 9    VALUE 0     .
      * nb de caractères max utilisés pour écrire 1 opération 
       01  WS-NB-CAROPE      PIC 99   VALUE 37    .  
      
       01 ENT-LIG PIC X(230).
       01 ENT-DET REDEFINES ENT-LIG.      
              05 FILLER         PIC X(3)   VALUE ' ! '     .
              05 FILDATE        PIC X(35)   
                 VALUE 'Liste des calculs effectués le : ' . 
              05 ENT-DAT        PIC X(29)                  . 
              
      * Variables de la calculette       
       01  WS-SIGN        PIC X(1).
      * Modification de la déclaration des variables numériques
      * pour accepter et afficher les décimales et signe négatif
       01  WS-NUM1        PIC S9(5)V99   VALUE 0.
       01  WS-NUM1-CHAR   REDEFINES WS-NUM1 PIC X(7).
       01  WS-NUM1S-AFF    PIC +Z(5).99.
       01  WS-NUM1-AFF REDEFINES WS-NUM1S-AFF PIC Z(5).99.
       01  WS-CODE        PIC X(7) .
           88  STOPPER    VALUE 'X'.
           88  REINIT     VALUE 'Z'.
       01  WS-DETAIL      PIC X VALUE 'O'.
       01  WS-BON-OPE     PIC 9 VALUE 1.
       01  WS-NUM2        PIC S9(5)V99   VALUE 0.
       01  WS-NUM2-CHAR   REDEFINES WS-NUM2 PIC X(7).
       01  WS-NUM2S-AFF    PIC +Z(5).99.
       01  WS-NUM2-AFF REDEFINES WS-NUM2S-AFF PIC Z(5).99.
      * MAJv2: Ajout test valeur numérique sur la saisie 
       01  WS-NUM         PIC S9(5)V99.
       01  WS-BOOL-NUM    PIC 9.
       01  WS-AFF-RESULTS  PIC +Z(4)9.99  VALUE 0.
       01  WS-AFF-RESULT REDEFINES WS-AFF-RESULTS PIC Z(5).99.
       01  WS-RESULT      PIC S9(5)V99   VALUE 0.
       01  WS-AFF-ERR1    PIC X(50)      VALUE 
            'Merci de répondre par O pour oui ou N pour non'.
       01  WS-AFF-ERR2    PIC X(46)      VALUE 'Valeur non numérique'.
       01  WS-AFF-ERR3 PIC X(30) VALUE 'Impossible de diviser par zero'.
       01  WS-AFF-STOP    PIC X(38)      VALUE 
           'Pour arrêter de jouer saisissez X'.
       01  WS-AFF-REINIT  PIC X(55)      VALUE 
           'Pour réinitialiser la série de calculs saisissez Z'.    
       01  WS-AFF-BONJOUR PIC X(50)      VALUE           
           'Bonjour, saisissez la première série à calculer'.
       01  WS-AFF-DETAIL  PIC X(50)      VALUE 
           'Voulez-vous afficher l''opération, Oui O, Non N'.
       01  WS-AFF-SAI-SIGN PIC X(60)     VALUE           
           'Saisissez le code de l''opération + ou - ou * ou / ou P'.   

      * MAJv1: gestion de la date
       01  WS-DATE-YMD.
           05 WS-DAT-YY   PIC 9(4).
           05 WS-DAT-MM   PIC 99.
           05 WS-DAT-JJ   PIC 99.
           05 WS-DAT-HH   PIC 99.
           05 WS-DAT-MN   PIC 99.
           05 WS-DAT-REST PIC X(9).

       01  WS-DATE-DMY.
           05 WS-DAT-JJ   PIC 99.
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-MM   PIC 99.
           05 FILLER      PIC X VALUE '/'.
           05 WS-DAT-YY   PIC 9(4).
           05 FILLER      PIC X(8) VALUE ' Heure: '.
           05 WS-DAT-HH   PIC 99.
           05 FILLER      PIC X VALUE 'H'.
           05 WS-DAT-MN   PIC 99.
.

      **************************************************************
      * Exécution du programme                                      
      **************************************************************
       PROCEDURE DIVISION.
       
      * 0000-MAIN-START.

      * DIs bonjour et récupère le premier chiffre
      * MAJv1:
           OPEN OUTPUT FIC-OUT.
           PERFORM AFF-DATE.
           PERFORM DIS-BONJOUR THRU SAI-WS-DETAIL.

       SAI-WS-NUM1.    
           INITIALIZE WS-NUM1 .
           DISPLAY 'Saisissez le premier chiffre - 3 positions maxi '
              WITH NO ADVANCING.
           ACCEPT WS-NUM1 .
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir
           MOVE  WS-NUM1-CHAR TO WS-CODE.
           PERFORM TST-CODE.
      * MAJv2: Ajout de tests de saisie
           MOVE WS-NUM1 TO WS-NUM.
           PERFORM TEST-NUM.
           If (WS-BOOL-NUM = 0) THEN
              DISPLAY WS-AFF-ERR2 
              PERFORM SAI-WS-NUM1 
           END-IF. 

           PERFORM UNTIL STOPPER
              PERFORM SAI-WS-SIGN THRU SAI-WS-NUM2

      * Selon le type d'opération demandée, on applique le calcul 
      * correspondant et on affiche le résultat     
              EVALUATE WS-SIGN
                 WHEN '+'
                    PERFORM CALC-ADD                  
                 WHEN '-'
                    PERFORM CALC-SUBTR  
                 WHEN '*'
                    PERFORM CALC-MULTIP
                 WHEN '/'
                    PERFORM CALC-DIVISE
                 WHEN 'P'   
                    PERFORM CALC-P
                 WHEN OTHER
                    DISPLAY 'Cette operation n''est pas prévue'
                    MOVE 0 TO WS-BON-OPE 
              END-EVALUATE
      * On affiche le résultat que si la saisie est OK        
              IF WS-BON-OPE = 1 THEN PERFORM AFF-RESULT
              ELSE MOVE 1 TO WS-BON-OPE 
              END-IF   
                      
           END-PERFORM.
     
           PERFORM ECRIT-FIC.
           CLOSE FIC-OUT .
      * 0000-MAIN-END.    
           STOP RUN.
      ************************************************************** 

      *============================================================*
      *    Les sous-paragraphes
      *============================================================*

      **************************************************************
      * Récupère la saisie et teste les valeurs saisies            *
      **************************************************************
      * MAJv1
       AFF-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE-YMD.
           MOVE CORR WS-DATE-YMD TO WS-DATE-DMY.
           DISPLAY 'NOUS SOMMES LE' SPACE WS-DATE-DMY.
           
      * MAJv2: Gestion du fichier de sortie 
           MOVE 'Liste des calculs effectués le : ' TO FILDATE. 
           MOVE WS-DATE-DMY TO ENT-DAT .
           MOVE ENT-LIG TO E-REC-OUT.
           PERFORM ECRIT-FIC.

       DIS-BONJOUR.
           DISPLAY WS-AFF-BONJOUR. 
           DISPLAY WS-AFF-STOP.
      * MAJv2: gestion de la réinitialisation
           DISPLAY WS-AFF-REINIT. 
       SAI-WS-DETAIL.
           DISPLAY WS-AFF-DETAIL WITH NO ADVANCING.
           ACCEPT WS-DETAIL.
           MOVE FUNCTION TRIM(WS-DETAIL) TO WS-DETAIL.
           MOVE FUNCTION UPPER-CASE(WS-DETAIL) TO WS-DETAIL.
      * MAJv2: Ajout de tests de saisie
           PERFORM TST-CODE.
           IF (WS-DETAIL NOT = 'O') AND (WS-DETAIL NOT = 'N') THEN 
              DISPLAY WS-AFF-ERR1
              PERFORM SAI-WS-DETAIL
              PERFORM SAI-WS-NUM1 
           END-IF.     
         

       SAI-WS-SIGN.
           INITIALIZE WS-SIGN.    
           DISPLAY WS-AFF-SAI-SIGN WITH NO ADVANCING.
           ACCEPT WS-SIGN.   
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir
           MOVE FUNCTION TRIM(WS-SIGN) TO WS-CODE.
           MOVE FUNCTION TRIM(WS-SIGN) TO WS-SIGN.
           MOVE FUNCTION UPPER-CASE(WS-SIGN) TO WS-SIGN.
           PERFORM TST-CODE.
           
       SAI-WS-NUM2.    
           INITIALIZE WS-NUM2.
           DISPLAY 'Saisissez le deuxième chiffre - 3 positions maxi '
            WITH NO ADVANCING.
           ACCEPT WS-NUM2. 
      * récupère la saisie et vérifie si l'utilisateur n'a pas demandé
      * de sortir           
      *     MOVE FUNCTION TRIM(WS-NUM2-CHAR) TO WS-CODE.
           MOVE WS-NUM2-CHAR TO WS-CODE.
           PERFORM TST-CODE. 
      * MAJv2: Ajout de tests de saisie     
           MOVE WS-NUM2 TO WS-NUM.
           PERFORM TEST-NUM.
           If (WS-BOOL-NUM = 0) THEN
              DISPLAY WS-AFF-ERR2 
              PERFORM SAI-WS-NUM2 
           END-IF.

      * Vérifie que le deuxième chiffre est correct vis à vis du premier
           PERFORM TST-VALEUR . 
      *     MOVE WS-NUM2 TO WS-NUM2-CHAR.
      *     INSPECT WS-NUM2-CHAR CONVERTING '0' TO SPACE.       
      

       TST-VALEUR.
      * Si pas OK on demande une nouvelle saisie du Num2
           IF (WS-NUM2 =0) AND (WS-SIGN = '/') THEN
                 DISPLAY WS-AFF-ERR3 
                 PERFORM SAI-WS-NUM2
           END-IF.         

      * MAJv2: Ajout tests de saisie, Ici valeur numérique 
       TEST-NUM.
           MOVE 1 TO WS-BOOL-NUM. 
      *    INSPECT WS-NUM CONVERTING SPACE TO '0'.
      *    display WS-NUM.
           IF (WS-NUM IS NOT NUMERIC ) THEN   
              MOVE 0 TO WS-BOOL-NUM
           END-IF.   

       CALC-ADD.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.

       CALC-SUBTR.
           SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT .          

       CALC-MULTIP.
           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.

       CALC-DIVISE.
           DIVIDE  WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.

       CALC-P.
           COMPUTE WS-RESULT = WS-NUM1 ** WS-NUM2.

      * Affiche a x b = c
       AFF-RESULT.
           MOVE WS-RESULT TO WS-AFF-RESULT 
           
           IF (WS-DETAIL = 'O') THEN
              IF (WS-NUM1 > 0) THEN
                 MOVE WS-NUM1 TO WS-NUM1-AFF
              ELSE
                 MOVE WS-NUM1 TO WS-NUM1S-AFF  
              END-IF 
               IF (WS-NUM2 > 0) THEN
                 MOVE WS-NUM2 TO WS-NUM2-AFF
              ELSE
                 MOVE WS-NUM2 TO WS-NUM2S-AFF 
              END-IF
              IF (WS-RESULT > 0) THEN
                 MOVE WS-RESULT TO WS-AFF-RESULT
              ELSE
                 MOVE WS-RESULT TO WS-AFF-RESULTS
              END-IF
              
              IF (WS-NUM1 > 0) THEN
                 IF (WS-NUM2 > 0) THEN
                    IF (WS-RESULT > 0) THEN
                       DISPLAY  WS-NUM1-AFF  SPACE WS-SIGN 
                       SPACE  WS-NUM2-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULT  
                    ELSE 
                       DISPLAY  WS-NUM1-AFF  SPACE WS-SIGN 
                       SPACE  WS-NUM2-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULTS  
                    END-IF
                 ELSE 
                    IF (WS-RESULT > 0) THEN
                       DISPLAY WS-NUM1-AFF SPACE WS-SIGN 
                       SPACE WS-NUM2S-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULT  
                    ELSE 
                       DISPLAY  WS-NUM1-AFF  SPACE WS-SIGN 
                       SPACE WS-NUM2S-AFF SPACE '=' 
                       SPACE WS-AFF-RESULTS  
                    END-IF
                 END-IF
              ELSE
                 IF (WS-NUM2 > 0) THEN
                    IF (WS-RESULT > 0) THEN
                    DISPLAY  WS-NUM1S-AFF  SPACE WS-SIGN 
                    SPACE WS-NUM2-AFF  SPACE '=' 
                    SPACE WS-AFF-RESULT  
                 ELSE 
                    DISPLAY  WS-NUM1S-AFF  SPACE WS-SIGN 
                       SPACE  WS-NUM2-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULTS  
                    END-IF
                 ELSE 
                    IF (WS-RESULT > 0) THEN
                       DISPLAY  WS-NUM1S-AFF  SPACE WS-SIGN 
                       SPACE  WS-NUM2S-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULT  
                    ELSE 
                       DISPLAY  WS-NUM1S-AFF  SPACE WS-SIGN 
                       SPACE  WS-NUM2S-AFF  SPACE '=' 
                       SPACE WS-AFF-RESULTS  
                    END-IF
                 END-IF
              END-IF  
           ELSE 
               DISPLAY WS-AFF-RESULT  
           END-IF.
      * MAJv2: gestion de l'enregistrement du fichier
           PERFORM CHARG-ENREG.
           
           
      *    CHarge le résultat dans WS-NUM1 pour le prochain calcul 
      *    et laisse la main à la boucle
      *    qui gère l'initialisation et l'affichage pour la saisie
           MOVE WS-RESULT TO WS-NUM1.
           MOVE WS-RESULT TO WS-NUM1-AFF .
           INITIALIZE WS-RESULT.
      * MAJv2: Affiche à chaque calcul le process pour arreter ou réinitialiser 
           DISPLAY WS-AFF-STOP.
           DISPLAY WS-AFF-REINIT. 

      * MAJv2: gestion d'un fichier de sortie 
       CHARG-ENREG.
           ADD 1 TO WS-NB-CALC.
           IF (WS-NB-CALC = 1) THEN
              MOVE '! '         TO FS-DEB       OF ENR-TAB(WS-NB-CALC)
           END-IF. 
      * Pour une meilleure lisibilité, gestion du signe que si négatif         
           IF (WS-NUM1 < 0) THEN  
           MOVE WS-NUM1S-AFF     TO FS-SNUM1     OF ENR-TAB(WS-NB-CALC)
           ELSE 
           MOVE WS-NUM1-AFF     TO FS-NUM1     OF ENR-TAB(WS-NB-CALC)
           END-IF.
           MOVE WS-SIGN         TO FS-SIGN      OF ENR-TAB(WS-NB-CALC).
           IF (WS-NUM2 < 0) THEN 
           MOVE WS-NUM2S-AFF     TO FS-SNUM2     OF ENR-TAB(WS-NB-CALC)  
           ELSE
           MOVE WS-NUM2-AFF     TO FS-NUM2     OF ENR-TAB(WS-NB-CALC)
           END-IF.
           MOVE ' = '           TO FS-EQUAL     OF ENR-TAB(WS-NB-CALC).
           IF (WS-AFF-RESULT < 0) THEN
           MOVE WS-AFF-RESULTS   TO FS-SRESULT   OF ENR-TAB(WS-NB-CALC)
           ELSE
           MOVE WS-AFF-RESULT  TO FS-RESULT  OF ENR-TAB(WS-NB-CALC)
           END-IF.
           MOVE ' |'           TO FS-FIN       OF ENR-TAB(WS-NB-CALC).
      * Le nombre d'opérations est 6, il est temps de l'écrire
           IF (WS-NB-CALC = 6) THEN PERFORM ECRIT-FIC.

       ECRIT-FIC.       
      
           WRITE E-REC-OUT.
           INITIALIZE WS-NB-CALC.
           MOVE ALL '-' TO E-REC-OUT.
           WRITE E-REC-OUT.
           MOVE ALL SPACE TO E-REC-OUT.     
           

      * MAJv2: Gestion de la réinitialisation
       REINIT-VALUE.
           PERFORM ECRIT-FIC.
           DISPLAY 'Réinitialisation des calculs'.
           INITIALIZE WS-RESULT.
           INITIALIZE WS-NUM1.
           INITIALIZE WS-NUM2.
           INITIALIZE WS-SIGN.
           PERFORM SAI-WS-NUM1.


      * Test si l'utilisateur veut sortir, si oui le programme s'arrete
       TST-CODE.
           MOVE FUNCTION UPPER-CASE (WS-CODE) TO WS-CODE.
      * MAJv2: Gestion de la réinitialisation
           IF REINIT THEN
              PERFORM ECRIT-FIC
              PERFORM REINIT-VALUE
           ELSE 
              IF STOPPER THEN
                 PERFORM ECRIT-FIC
                 CLOSE FIC-OUT 
                 STOP RUN
              END-IF 
           END-IF.
