       FD  F-TRAIN
           RECORD IS VARYING IN SIZE FROM 27 TO 50 CHARACTERS 
           DEPENDING ON WS-FS-ENREG
           RECORDING MODE IS V.
       01  REC-TRAIN PIC X(50).
       01  FS-TRAIN-DET REDEFINES REC-TRAIN.   
           05 FS-TYP   PIC X(3).
           05 FS-DEST  PIC X(18). 
           05 FS-HHMN  PIC 9(4).
           05 FS-NBH   PIC 99.
           05 FS-ARR-TAB PIC X OCCURS 10 .
           88 TRAIN-STOP  VALUE 'H'.

