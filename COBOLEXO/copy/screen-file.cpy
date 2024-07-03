       01  SCR-FILE FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'EXERCICE 03'.
                  
           05 FILLER   PIC X(40) LINE 20 COL 40
              VALUE 'Le plus gros salaire trouvé est: '.    
           05 PIC X(6) FROM  WS-MAXZ 
              COL 81.  
           05 FILLER   PIC X(40) LINE 22 COL 40
              VALUE 'Le plus petit salaire est le suivant: '.    
           05 PIC X(6) FROM  WS-MINZ 
               COL 81.         

           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'RETOUR AU MENU'.           
           05 PIC X(1) USING  SC-RETURN 
           COL 178 
           FOREGROUND-COLOR IS 10.           
   
              