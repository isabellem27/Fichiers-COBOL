      
       3000-DATA-DIV-START.
           PERFORM 2000-OPEN-DATA-DIV-START THRU 2000-OPEN-DATA-DIV-END.
           PERFORM 2030-READ-DATA-DIV-START THRU 2030-READ-DATA-DIV-END.
           PERFORM UNTIL DATA-DIV-STATUS-EOF
              MOVE REC-DATA-DIV TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-DATA-DIV-START 
                          THRU 2030-READ-DATA-DIV-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-DATA-DIV-START 
                       THRU 2020-CLOSE-DATA-DIV-END.
       3000-DATA-DIV-END.
           EXIT.
       
       3000-FILE-SECT-START. 
           PERFORM 2000-OPEN-FILE-SECTION-START 
                       THRU 2000-OPEN-FILE-SECTION-END.
           PERFORM 2030-READ-FILE-SECTION-START 
                       THRU 2030-READ-FILE-SECTION-END.
           PERFORM UNTIL FILE-SECTION-STATUS-EOF
              MOVE REC-FILE-SECTION TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-SECTION-START 
                          THRU 2030-READ-FILE-SECTION-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-SECTION-START 
                       THRU 2020-CLOSE-FILE-SECTION-END.       
       3000-FILE-SECT-END.
           EXIT.
       3000-FILE-CONT-START.
           PERFORM 2000-OPEN-FILE-CONT-START 
                       THRU 2000-OPEN-FILE-CONT-END.
           PERFORM 2030-READ-FILE-CONT-START 
                       THRU 2030-READ-FILE-CONT-END.
           PERFORM UNTIL FILE-CONT-STATUS-EOF
              MOVE REC-FILE-CONT TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-FILE-CONT-START 
                          THRU 2030-READ-FILE-CONT-END              
           END-PERFORM.
           PERFORM 2020-CLOSE-FILE-CONT-START 
                       THRU 2020-CLOSE-FILE-CONT-END.       
       3000-FILE-CONT-END.
           EXIT.

       3000-IO-FILE-START. 
           PERFORM 2000-OPEN-IO-SECTION-START 
                    THRU 2000-OPEN-IO-SECTION-END.
           PERFORM 2030-READ-IO-SECTION-START 
                    THRU 2030-READ-IO-SECTION-END.
           PERFORM UNTIL IO-SECTION-STATUS-EOF
              MOVE REC-IO-SECTION TO REC-F-PRG                            
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-IO-SECTION-START 
                    THRU 2030-READ-IO-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-IO-SECTION-START 
                    THRU 2020-CLOSE-IO-SECTION-END.
       3000-IO-FILE-END.
           EXIT.   

       3000-WS-SECT-START.
           PERFORM 2000-OPEN-WS-SECTION-START 
                          THRU 2000-OPEN-WS-SECTION-END.
           PERFORM 2030-READ-WS-SECTION-START 
                               THRU 2030-READ-WS-SECTION-END.
           PERFORM UNTIL WS-SECTION-STATUS-EOF
              MOVE REC-WS-SECTION TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-WS-SECTION-START 
                               THRU 2030-READ-WS-SECTION-END
           END-PERFORM.
           PERFORM 2020-CLOSE-WS-SECTION-START 
                             THRU 2020-CLOSE-WS-SECTION-END.
       3000-WS-SECT-END.
           EXIT.

       3000-SQL-STAT-START.
           PERFORM 2000-OPEN-SQL-STAT-START 
                          THRU 2000-OPEN-SQL-STAT-END.
           PERFORM 2030-READ-SQL-STAT-START 
                               THRU 2030-READ-SQL-STAT-END.
           PERFORM UNTIL SQL-STAT-STATUS-EOF
              MOVE REC-SQL-STAT TO REC-F-PRG
              PERFORM 2010-WRITE-PRG-START THRU 2010-WRITE-PRG-END
              PERFORM 2030-READ-SQL-STAT-START 
                               THRU 2030-READ-SQL-STAT-END
           END-PERFORM.
           PERFORM 2020-CLOSE-SQL-STAT-START 
                             THRU 2020-CLOSE-SQL-STAT-END.
       3000-SQL-STAT-END.
           EXIT.     
