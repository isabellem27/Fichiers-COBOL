       01  SCR-ROMAIN FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'EXERCICE 05'.
                  
           05 FILLER   PIC X(46) LINE 20 COL 40
              VALUE 'Saisissez un nombre entier compris entre 1 et '. 
           05 FILLER   PIC X(7)  COL 87
              VALUE '3999 : '.   
           05 PIC X(4) USING  WS-CHIFFRE COL 94.  
                
           05 PIC X(250) FROM  SC-ROMAIN 
              LINE 22 COL 40.       

           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'RETOUR AU MENU'. 
           05 PIC X(1) USING  SC-RETURN 
           COL 178 
           FOREGROUND-COLOR IS 10.           

                