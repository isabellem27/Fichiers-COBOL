      *=============================================================*
      *    Description du tableau pour charger les données          *
      *    auteur : Isabelle Marand                                 *
      *    Date création 10/04/2024                                 *
      *=============================================================*

      ***************************************************************
      *    identification et déclarations
      ***************************************************************     
       IDENTIFICATION DIVISION.
       PROGRAM-ID. desctab1.
       AUTHOR . Isabelle Marand.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IND-TAB PIC 99 VALUE 1.
       01  IND-COD PIC 9  VALUE 1.
       01  WS-AFF-CLNUM PIC X(8).
       01  TEST-DATA.                   
           03 FILL       PIC X(33) 
           VALUE "0001HOKKAI       TARO        0400".
           03 FILL       PIC X(33) 
           VALUE "0002AOMORI       JIRO        0350".
           03 FILL       PIC X(33) 
           VALUE "0003AKITA        SABURO      0300".
           03 FILL       PIC X(33) 
           VALUE "0004IWATE        SHIRO       0900".
           03 FILL       PIC X(33) 
           VALUE "0005MIYAGI       GORO        0200".
           03 FILL       PIC X(33) 
           VALUE "0006FUKUSHIMA    RIKURO      0150".
           03 FILL       PIC X(33) 
           VALUE "0007TOCHIGI      SHICHIRO    0100".
           03 FILL       PIC X(33) 
           VALUE "0008IBARAKI      HACHIRO     1050".
           03 FILL       PIC X(33) 
           VALUE "0009GUMMA        KURO        0200".
           03 FILL       PIC X(33) 
           VALUE "0010SAITAMA      JURO        0350".

        01 WS-TAB-DATA REDEFINES TEST-DATA. 
            03 WS-FILLER    OCCURS 10 TIMES	. 
                05 WS-CLID 	    PIC 9(4) 			   . 
                05 WS-CLNOM 	PIC X(12)			   . 
                05 WS-CLPREN    PIC X(13)			   . 
                05 WS-CLNUM 	PIC 9(4) 	.
        
      ***************************************************************
      * Exécution du programme                                      
      ***************************************************************
       
       PROCEDURE DIVISION .

      * 0000-MAIN-START

           PERFORM VARYING IND-TAB FROM 1 BY 1 UNTIL IND-TAB > 10
              PERFORM AFF-ENREG
           END-PERFORM.

      * 0000-MAIN-END    
           STOP RUN.




       AFF-ENREG.
           DISPLAY 'ENREG' SPACE IND-TAB SPACE ':'.
           DISPLAY SPACE SPACE SPACE SPACE 'ID' 
            SPACE WS-CLID OF WS-TAB-DATA (IND-TAB) .
           DISPLAY SPACE SPACE SPACE SPACE 'NOM' 
            SPACE WS-CLNOM OF WS-TAB-DATA (IND-TAB) .
           DISPLAY SPACE SPACE SPACE SPACE 'PRENOM' 
            SPACE WS-CLPREN OF WS-TAB-DATA (IND-TAB) .
            
 

           DISPLAY SPACE SPACE SPACE SPACE 'CODES' 
            SPACE WS-CLNUM (IND-TAB) .

