       01  SCR-ECHEC FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'EXERCICE 04'.
                  
           05 FILLER   PIC X(48) LINE 20 COL 40
              VALUE 'Donnez la position de votre reine blanche et je '.
           05 FILLER   PIC X(51) LINE 20 COL 89
             VALUE 'vous dirai si vous pouvez attaquer la reine noire:'.
           05 FILLER   PIC X(20) LINE 20 COL 140
              VALUE '1 lettre 1 chiffre'.             
           05 FILLER   PIC X(60) LINE 21 COL 40 VALUE 
            'Choisissez 1 chiffre entre 1 et 8, 1 lettre entre A et H'.
           05 PIC X(2) USING WS-POS-DB COL 102. 

           05 PIC X(15) FROM  SC-LIG-ECHEC
              LINE 24 COL 40.  
           05 PIC X(40) FROM  SC-LIG-DN
              LINE 25 COL 40.     

           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'RETOUR AU MENU'.  
           05 PIC X(1) USING  SC-RETURN 
           COL 178 
           FOREGROUND-COLOR IS 10.           
 
              