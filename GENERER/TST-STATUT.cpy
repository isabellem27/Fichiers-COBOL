       9000-TEST-:FNAME:-STATUT-START.
           IF (NOT :FNAME:-STATUS-OK) 
              AND (NOT :FNAME:-STATUS-EOF) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE :FNAME:-STATUS 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP
              STOP RUN 
           END-IF. 
       9000-TEST-:FNAME:-STATUT-END. 
           EXIT.

           