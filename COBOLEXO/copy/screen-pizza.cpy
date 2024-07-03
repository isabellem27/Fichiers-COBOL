       01  SCR-PIZZAS FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'EXERCICE 01'.
                  
           05 FILLER   PIC X(50) LINE 20 COL 40
              VALUE 'Bonjour, Veuillez saisir le nombre de convives:'. 
           05 PIC X(3) USING  WS-NBGOURMAND 
              COL 91.       
           05 FILLER   PIC X(43) LINE 21 COL 40
              FOREGROUND-COLOR IS 9
              VALUE 'Sachant que chaque convive mange 1,1 part, '.  
           05 FILLER   PIC X(20) LINE 21 COL 84
              FOREGROUND-COLOR IS 9
              VALUE 'vous avez besoin de '. 
           05 PIC X(4) FROM  WS-NBPIZZAZ 
              COL 105.    
           05 FILLER   PIC X(10) LINE 21 COL 110
              FOREGROUND-COLOR IS 9
              VALUE ' pizzas'. 
                 
           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'RETOUR AU MENU'.
           05 PIC X(1) USING  SC-RETURN  
           COL 178 
           FOREGROUND-COLOR IS 10.           
   
              