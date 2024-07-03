       01  SCR-CHOICEEXO FOREGROUND-COLOR IS 10 BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 FILLER   PIC X(40) LINE 6 COL 75
              VALUE 'LISTE DES EXERCICES DISPONIBLES'.
                  
           05 FILLER   PIC X(15) LINE 20 COL 40
              VALUE 'EXERCICE 01 :'.     
           05 FILLER   PIC X(38) LINE 20 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'A partir d''un nombre de gourmands que '.  
           05 FILLER   PIC X(41) LINE 20 COL 93
              FOREGROUND-COLOR IS 9
              VALUE 'vous renseignez, le programme calcule le '. 
           05 FILLER   PIC X(26) LINE 20 COL 134
              FOREGROUND-COLOR IS 9
              VALUE 'nombre de pizzas a acheter'. 
                 
           05 FILLER   PIC X(15) LINE 22 COL 40
              VALUE 'EXERCICE 02 :'.     
           05 FILLER   PIC X(42) LINE 22 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'Application de la conjecture de Syracuse '.  
           05 FILLER   PIC X(31) LINE 22 COL 97
              FOREGROUND-COLOR IS 9
              VALUE 'sur un nombre que vous saisirez'. 

           05 FILLER   PIC X(15) LINE 24 COL 40
              VALUE 'EXERCICE 03 :'. 
           05 FILLER   PIC X(50) LINE 24 COL 55
              FOREGROUND-COLOR IS 9
             VALUE 'Recherche des salaires min et max dans un fichier.'.          
            
           05 FILLER   PIC X(15) LINE 26 COL 40
              VALUE 'EXERCICE 04 :'.     
           05 FILLER   PIC X(42) LINE 26 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'L''attaque de la reine.'.  
           05 FILLER   PIC X(48) LINE 26 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'Vous saisissez la position de la reine blanche, '. 
           05 FILLER   PIC X(48) LINE 26 COL 103
              FOREGROUND-COLOR IS 9
              VALUE 'l''application calcule aleatoirement la position '.
           05 FILLER   PIC X(18) LINE 26 COL 151
              FOREGROUND-COLOR IS 9
              VALUE 'de la reine noire.'.
           05 FILLER   PIC X(43) LINE 27 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'L''application vous dit si la reine blanche '.
           05 FILLER   PIC X(30) LINE 27 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'peut attaquer la reine noire. '.           
       
           05 FILLER   PIC X(15) LINE 29 COL 40
              VALUE 'EXERCICE 05 :'.     
           05 FILLER   PIC X(47) LINE 29 COL 55
              FOREGROUND-COLOR IS 9
              VALUE 'Saisissez un nombre entier compris entre 1 et '.  
           05 FILLER   PIC X(50) LINE 29 COL 55
              FOREGROUND-COLOR IS 9
              VALUE '3999. Nous vous le traduisons en chiffres romains'.     

           05 FILLER   PIC X(50) LINE 34 COL 40
              FOREGROUND-COLOR IS 10
              VALUE 'Pour sortir tapez X au niveau du bouton Arret'.                

           05 FILLER   PIC X(50) LINE 35 COL 40
              FOREGROUND-COLOR IS 10
              VALUE 'Choisissez l''exercice que vous voulez faire :'.     
           05 PIC X(1) USING  WS-CHOICE  
           COL 90 
           FOREGROUND-COLOR IS 10.           

           05 FILLER   PIC X(15) LINE 37 COL 180
              FOREGROUND-COLOR IS 10
              VALUE 'ARRET'.             
           05 PIC X(1) USING  WS-STOP  COL 178           
           FOREGROUND-COLOR IS 10.           
  
              