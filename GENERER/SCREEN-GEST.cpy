      * Gestion champ par champ pour permettre l'utilisation 
      * de la touche Entrée champ par champ après la saisie
       4000-DISPLAY-IDENT-START.
           DISPLAY IDENT-SCREEN.
           DISPLAY " " LINE 6 COL 46 .
           ACCEPT WS-PRG-NOM AT LINE 6 COL 46.
           DISPLAY " " LINE 7 COL 46 .
           ACCEPT WS-PRG-AUT AT LINE 7 COL 46.
           DISPLAY " " LINE 9 COL 46 .
           ACCEPT WS-PERSO AT LINE 9 COL 46.
       4000-DISPLAY-IDENT-END.
           EXIT.

       4000-DISPLAY-PERSO-START.
           DISPLAY PERSO-SCREEN.
           DISPLAY " " LINE 2 COL 25.
           DISPLAY " " LINE 3 COL 25.           
           DISPLAY " " LINE 6 COL 46 .
           ACCEPT WS-INPUT AT LINE 6 COL 46.
           DISPLAY " " LINE 7 COL 46 .
           ACCEPT WS-OUTPUT AT LINE 7 COL 46.
           DISPLAY " " LINE 8 COL 47 .
           ACCEPT WS-NBSSPRG AT LINE 8 COL 47.
           DISPLAY " " LINE 9 COL 67 .
           ACCEPT WS-SQL AT LINE 9 COL 67.
           IF (FUNCTION UPPER-CASE( WS-SQL) = 'O') THEN
              DISPLAY " " LINE 10 COL 46 
              ACCEPT WS-SQLNOM AT LINE 10 COL 46 
              DISPLAY " " LINE 11 COL 60 
              ACCEPT WS-SQLUSER AT LINE 11 COL 60 
              DISPLAY " " LINE 12 COL 9
              DISPLAY " " LINE 13 COL 66 
              ACCEPT WS-SQLPSW AT LINE 13 COL 66 
           END-IF.                               
       4000-DISPLAY-PERSO-END.
           EXIT.

       4000-DISPLAY-FIC-START.
           DISPLAY FIC-SCREEN.
           DISPLAY " " LINE 2 COL 25.
           DISPLAY " " LINE 3 COL 25.
           DISPLAY " " LINE 6 COL 34.
           ACCEPT WS-FICNOM AT LINE 6 COL 34.
           DISPLAY " " LINE 7 COL 62 .
           ACCEPT WS-FICTYP AT LINE 7 COL 62.
           DISPLAY " " LINE 8 COL 45 .
           ACCEPT WS-LGENREG AT LINE 8 COL 45.          
       4000-DISPLAY-FIC-END.
           EXIT.

       4000-DISPLAY-SSPRG-START.
           DISPLAY SSPRG-SCREEN.
           DISPLAY " " LINE 2 COL 25.
           DISPLAY " " LINE 3 COL 25.
           DISPLAY " " LINE 6 COL 36.
           ACCEPT WS-SSPRGNOM AT LINE 6 COL 36.
           DISPLAY " " LINE 7 COL 45 .
           ACCEPT WS-RET AT LINE 7 COL 45.
           IF (FUNCTION UPPER-CASE(WS-RET) ='O') THEN 
              DISPLAY " " LINE 8 COL 30 
              ACCEPT WS-PARAMNOM AT LINE 8 COL 30 
           END-IF. 
           DISPLAY " " LINE 9 COL 60 .
           ACCEPT WS-NBPARAM AT LINE 9 COL 60.
           DISPLAY " " LINE 10 COL 10 .
              
       4000-DISPLAY-SSPRG-END.
           EXIT.

       4000-DISPLAY-PARAM-START.
           DISPLAY PARAM-SCREEN.
           DISPLAY " " LINE 2 COL 1.
           DISPLAY " " LINE 3 COL 1.           
           DISPLAY " " LINE 6 COL 30 .
           ACCEPT WS-PARAMNOM AT LINE 6 COL 30.
           DISPLAY " " LINE 7 COL 66 .
           ACCEPT WS-PARAMTYP AT LINE 7 COL 66.             
       4000-DISPLAY-PARAM-END.
           EXIT.
