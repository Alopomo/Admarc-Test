002700 IDENTIFICATION DIVISION.
002800 PROGRAM-ID.                     AMFNTEMAIL.
002900 AUTHOR.                         NOEL FUENTES.
003000 INSTALLATION.                   NEW YORK TIMES.
003100 DATE-WRITTEN.                   MARCH 25, 2015.
003200 DATE-COMPILED.
      ***********************************************************
037000*** CHANGE LOG:        VERSION# 7.0-XX                    *        
003800***    DATE     - XX - # - SHORT DESCRIPTION              *        
013400***    08/24/16 - AL-01    add new print ctl value        *
      ***********************************************************
003400 ENVIRONMENT DIVISION.
003500 CONFIGURATION SECTION.
003600 SOURCE-COMPUTER.                AMZMCMPS.
003700 OBJECT-COMPUTER.                AMZMCMPO.
003800 INPUT-OUTPUT SECTION.
003900 FILE-CONTROL.
004001     SELECT OUTPUT-FILE     ASSIGN TO SYS002
                      ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-FILE      ASSIGN TO SYS007
                      ORGANIZATION IS SEQUENTIAL.

004200/
004200/
004300 DATA DIVISION.
004400 FILE SECTION.
004701 FD  OUTPUT-FILE            COPY AMZFSEL.
004702 01  OUTPUT-REC                  PIC X(380).

       FD  INPUT-FILE             COPY AMZFSEL.
       01  INPUT-REC                   PIC X(284).
004703
004710 
004712

008100/
008200 WORKING-STORAGE SECTION.
       

013902 01  WK-OUTPUT-REC.        
           05  OUT-ACCT-NBR                PIC 9(10). 
           05  OUT-NAME                    PIC X(40).   
           05  OUT-EMAIL-1                 PIC X(80).
           05  OUT-EMAIL-2                 PIC X(50).
           05  OUT-EMAIL-3                 PIC X(50).
           05  OUT-EMAIL-4                 PIC X(50).
           05  OUT-EMAIL-5                 PIC X(50).
           05  OUT-EMAIL-6                 PIC X(50).

       01  WK-INPUT-REC.               COPY AMZRSEL.



018316 01  NAD-REC.                    COPY AMZRNAD.
018317/
0
.18330 01  DB-FILES.                   COPY AMZWFILE.
018331 01  IO-PKT.                     COPY AMZWIOP.
018332/
018336 01  WK-SEQ                      PIC 9(12) VALUE 0.
018336 01  WK-FOUND                    PIC X(01).
018336 01  WK-TEMP                     PIC S9(11)V99 COMP  .
018336 01  WK-X                        PIC 9(02) VALUE 0.
018337 01  WK-REM-TEXT                 PIC X(70).                       00003810
018338 01  WK-REMARK-MAX               PIC 9(02) VALUE 70.              00003810
018339 01  WK-REMARK.                                                   00003810
018340     05  WK-REMARK-X             OCCURS 70                        00003820
018341                                 INDEXED WK-RX                    00003830
018342                                 PIC X(01).                       00003840
018343 01  UTL-WORK.                   COPY AMZWUTL.

018344 01  WK-NBR-APP                  PIC 9(02).

018345 01  WK-JOB-NBR                  PIC 9(13).
018346 01  FILLER REDEFINES WK-JOB-NBR.
018347     05  WK-JOB-NBR-7         PIC 9(10).
018348     05  WK-JOB-NBR-2         PIC 9(03).
018349
018350 01  SAVE-PUB                    PIC X(04).
018350 01  SAVE-JOB-NBR                PIC 9(13).
018360 01  FILLER REDEFINES SAVE-JOB-NBR.
018370     05  SAVE-JOB-NBR-7       PIC 9(10).
018380     05  SAVE-JOB-NBR-2       PIC 9(03).
018381
018382 01  WK-CNT-SEQ                  PIC 9(09) VALUE 0.
018383 01  WK-CNT-LEVEL                PIC 9(08).
018384 01  WK-CNT-LVL                  REDEFINES WK-CNT-LEVEL
018385                                 PIC X(08).
018386 01  WK-CNT-LVL-RE               REDEFINES WK-CNT-LEVEL.
018387     05  WK-CNT-LVL-1            PIC X(04).
018388     05  WK-CNT-LVL-2            PIC X(04).
018389 01  WK-XRF-NAME                 PIC X(12).
018389 01  WK-XRF-NAME-2               PIC X(12).
NYTMSB 01  WK-XRF-ACCT-NBR          PIC 9(09).                           NYTMSB
018390 01  WK-ADV-JXRF                 PIC X(10).
018391 01  WK-ADV-TRN-JXRF             PIC X(10).
018392 01  WK-AGY-JXRF                 PIC X(10).
018393 01  WK-PAR-JXRF                 PIC X(06).
018394 01  WK-APARJXRF                 PIC X(06).
018395 01  WK-SORT-EOF                 PIC X(01) VALUE 'N'.
018396 01  WK-FIRST                    PIC X(01) VALUE 'N'.
018397 01  WK-JXRF-AREA.
018398     05  FILLER                  PIC X(01).
018399     05  WK-JXRF.
018400         10  WK-JXRF-1           PIC X(01).
018410         10  WK-JXRF-5           PIC X(05).
018500     05  FILLER                  PIC X(04).
018600 01  WK-STOP                     PIC X(01) VALUE 'N'.
018700 01  WK-ADV-STOP                 PIC X(01) VALUE 'N'.
018700 01  WK-AGY-STOP                 PIC X(01) VALUE 'N'.
018600 01  WK-STOP-COUNT               PIC 9(09) VALUE 999.
018700 01  WK-AGY-TRN-STOP             PIC X(01) VALUE 'N'.
018700 01  WK-ADV-COUNT                PIC 9(09) VALUE 0.
018701 01  WK-ADV-TRN-COUNT            PIC 9(09) VALUE 0.
018702 01  WK-AGY-COUNT                PIC 9(09) VALUE 0.
018703 01  WK-CNT-COUNT                PIC 9(09) VALUE 0.
018704 01  WK-CDT-COUNT                PIC 9(09) VALUE 0.
018705 01  WK-WO-COUNT                 PIC 9(09) VALUE 0.
018800 01  WK-DISPLAY                  PIC ZZZ,ZZZ,ZZ9.
018900 01  WK-NAD-NAM1                 PIC X(01)      VALUE SPACE.
019000 01  WK-COUNT                    PIC 9(09) COMP VALUE 0.
019100 01  WK-TEST-COUNT               PIC 9(09) COMP VALUE 0.
019400 01  WK-DATE                     PIC 9(08) COMP VALUE 0.
019400 01  WK-TODAY-DATE               PIC 9(08) COMP VALUE 0.
019400 01  SAVE-TODAY-DATE             PIC 9(08) COMP VALUE 0.
022100 01  WK-COMPILE                  PIC X(40).
022200
022300 01  WK-COMMENTS              PIC X(24).
022400 01  FILLER REDEFINES WK-COMMENTS.
022500     05  WK-POSN-RQST         PIC X(14).
022600     05  FILLER               PIC X(10).
022700
022701 01  WK-GROSS-AMT             PIC S9(07)V99.
       01  WORK-FIELDS.
           05  ARE-THERE-MORE-RECORDS      PIC X(2).
           05  INPUT-RECORD-COUNTER        PIC 9(09)  COMP-3  VALUE 0.
       01  ACCT-SUBSCRIPT                  PIC 9(04).
       01  SEARCH-SUBSCRIPT                PIC 9(04).
           
       01  ACCOUNT-TABLE.
           05  ACCT-ENTRIES OCCURS 10000 TIMES INDEXED BY SUB.
               10  TABLE-ACCT              PIC 9(09)  COMP.
022702/
022703 PROCEDURE  DIVISION.
022704 0000-MAIN-PROCESSING.
 
           OPEN INPUT  INPUT-FILE.
022800     OPEN OUTPUT OUTPUT-FILE.

023202     CALL 'JSTART'               USING IO-PKT IO-PKT IO-PKT.
           MOVE SPACES TO ARE-THERE-MORE-RECORDS.
           MOVE ZERO   TO INPUT-RECORD-COUNTER.
           MOVE ZERO   TO ACCT-SUBSCRIPT.
           INITIALIZE ACCOUNT-TABLE.

           PERFORM READ-INPUT-FILE.
           PERFORM PROCESS-INPUT-REC
              THRU PROCESS-INPUT-REC-EXIT
               UNTIL ARE-THERE-MORE-RECORDS = 'NO'.
  
           
380703     CALL 'JEND'                 USING IO1-PKT IO4-PKT IO4-PKT.

           DISPLAY 'TOTAL INPUT RECORDS: ' INPUT-RECORD-COUNTER.

           CLOSE       INPUT-FILE
                       OUTPUT-FILE.
380908
           GOBACK.
   
       PROCESS-INPUT-REC.
           IF SEL-AGY-ACCT-NBR > 0
               MOVE SEL-AGY-ACCT-NBR TO NAD-ACCT-NBR
           ELSE
               MOVE SEL-ACCT-NBR     TO NAD-ACCT-NBR
           END-IF.
           
           SET SUB TO 1.
            SEARCH ACCT-ENTRIES
               AT END CONTINUE
               WHEN NAD-ACCT-NBR = TABLE-ACCT (SUB)
                   GO TO PROCESS-INPUT-REC-EXIT
           END-SEARCH.
         
           CALL 'GIT'                 USING NAD-FILE NAD-REC IO-PKT.
           IF STATUS-CODE NOT = 0
                DISPLAY 'SEL-AGY-ACCT-NBR = ' SEL-AGY-ACCT-NBR
                DISPLAY 'SEL-ACCT-NBR =  ' SEL-ACCT-NBR
                
                DISPLAY 'NAD-ACCT-NBR =  ' NAD-ACCT-NBR
                DISPLAY 'NOT FOUND IN NAD FILE'
                MOVE 0 TO STATUS-CODE
               GO TO PROCESS-INPUT-REC-EXIT
           END-IF.
           
           IF  (NAD-EMAIL-1 = SPACES
           AND NAD-EMAIL-2 = SPACES
           AND NAD-EMAIL-3 = SPACES
           AND NAD-EMAIL-4 = SPACES
           AND NAD-EMAIL-5 = SPACES)
             or (nad-route(1:1) not = 'E' or
                 NAD-PRINT-CTL not = 'E')
              GO TO PROCESS-INPUT-REC-EXIT
           END-IF.
   
           MOVE SPACE                  TO WK-OUTPUT-REC.
           MOVE NAD-ACCT-NBR           TO OUT-ACCT-NBR.
           MOVE NAD-NAM1 (1)           TO OUT-NAME.
           MOVE NAD-EMAIL-1            TO OUT-EMAIL-1.
           MOVE NAD-EMAIL-2            TO OUT-EMAIL-2.
           MOVE NAD-EMAIL-3            TO OUT-EMAIL-3.
           MOVE NAD-EMAIL-4            TO OUT-EMAIL-4.
           MOVE NAD-EMAIL-5            TO OUT-EMAIL-5.
           MOVE SPACES                 TO OUT-EMAIL-6.

           WRITE OUTPUT-REC FROM WK-OUTPUT-REC.
           ADD 1 TO ACCT-SUBSCRIPT.
           MOVE NAD-ACCT-NBR TO TABLE-ACCT(ACCT-SUBSCRIPT).
   
       PROCESS-INPUT-REC-EXIT.
           PERFORM READ-INPUT-FILE.

       READ-INPUT-FILE.
           READ INPUT-FILE
               AT END 
                 MOVE 'NO' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                 MOVE INPUT-REC TO WK-INPUT-REC
                 ADD 1 TO INPUT-RECORD-COUNTER
           END-READ.
