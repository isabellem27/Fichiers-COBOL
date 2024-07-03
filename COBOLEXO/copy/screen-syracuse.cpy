       01  SCR-SYRACUSE FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'EXERCICE 02'.
                  
           05 FILLER   PIC X(50) LINE 20 COL 40
              VALUE 'Saisissez un nombre entier inferieur a 100000 : '. 
           05 PIC X(5) USING  WS-CHIFFRE 
              COL 91.    
           05 PIC X(250) FROM  SC-SYRACUSE 
              LINE 22 COL 40. 
           05 PIC X(250) FROM  SC-OPESYRACUSE1 
              LINE 23 COL 40.  
           05 PIC X(250) FROM  SC-OPESYRACUSE2 
              LINE 24 COL 40.  
           05 PIC X(250) FROM  SC-OPESYRACUSE3 
              LINE 25 COL 40.          
           05 PIC X(250) FROM  SC-OPESYRACUSE4 
              LINE 26 COL 40.

           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'RETOUR AU MENU'.
           05 PIC X(1) USING  SC-RETURN 
           COL 178 
           FOREGROUND-COLOR IS 10.           
 
                