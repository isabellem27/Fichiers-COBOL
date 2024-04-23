       FD  FO-TRAIN
           RECORD CONTAINS 35 TO 50 CHARACTERS
           RECORDING MODE IS V.
       01  RECO-TRAIN PIC X(50).
       01  FSO-TRAIN-DET REDEFINES RECO-TRAIN.   
           05 FS-TYP         PIC X(3).
           05 FS-DEST        PIC X(18). 
           05 FS-HHMN        PIC 9(4).
           05 FS-HHMN-DEST   PIC 9(4).
           05 FS-NBH         PIC 99.
           05 FS-NB-ARRET    PIC 99.  
           05 FSO-ARR-TAB PIC X OCCURS 10 .
           