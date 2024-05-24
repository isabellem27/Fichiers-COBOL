
       8000-OPEN-:FNAME:-START.
           OPEN OUTPUT :FNAME:.
           PERFORM 9000-TEST-:FNAME:-STATUT-START 
                    THRU 9000-TEST-:FNAME:-STATUT-END.
       8000-OPEN-:FNAME:-END.
           EXIT.

       8010-WRITE-:FNAME:-START.
           WRITE REC-:FNAME:  .
           PERFORM 9000-TEST-:FNAME:-STATUT-START 
                    THRU 9000-TEST-:FNAME:-STATUT-END.
       8010-WRITE-:FNAME:-END.
           EXIT.

       8020-CLOSE-:FNAME:-START.
           CLOSE :FNAME:.
           PERFORM 9000-TEST-:FNAME:-STATUT-START 
                    THRU 9000-TEST-:FNAME:-STATUT-END.          
       8020-CLOSE-:FNAME:-END.
           EXIT.

           