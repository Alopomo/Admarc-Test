003300*** 
003300*** 
003300*** dwww  ACR include/exclude logic for amcr1-tev
003300*** 
003300*** 
003300*** NEW YORK TIMES' CHANGES ARE MARKED WITH "NYTXX#" IN COLUMN 73 
003400*** WHERE "XX" IS PROGRAMMER'S INITIALS AND "#" IS THE SEQUENTIAL 
003500*** CHANGE NUMBER UNIQUE TO EACH PROGRAMMER WITHIN THIS PROGRAM.  
003600***                                                               wt-nyt
003700*** CHANGE LOG:        VERSION# 7.0-XX                            
003800***    DATE     - XX - # - SHORT DESCRIPTION                      
013400***                                                               
013500**********       RELEASE  4.1  JAN. 1985        ******************
013600***-------------------------------------------------------------- 
013700***  IMPLEMENT:  UNPAID WORK ORDERS (PAID FLAG="U") IN SEL FILE   
013800***              (SEL-PD-FLAG="U") SHOULD BE BYPASSED JUST LIKE   
013900***              PENDING ORDERS (SEL-STATUS="P") OR THEIR INVC-AMT
014000***              SHOULD BE IGNORED FROM PRINTING AND TOTALING !!!!
014100***-------------------------------------------------------------- 
014200**********       RELEASE  4.1  JAN. 1985        ******************
014300******************************************************************
014400*                                   (R)                          *
014500*                        A D M A R C                             *
014600*                                                                *
014700*     ADVERTISING MANAGEMENT AND ACCOUNTS RECEIVABLE CONTROL     *
014800*              SYSTEM DEVELOPED BY NEASI-WEBER INC.              *
014900*                                                                *
015000*    UNAUTHORIZED USE, COPYING,           COPYRIGHT 1980         *
015100*    OR REPRODUCTION OF ANY               NEASI-WEBER INC.       *
015200*    NEASI-WEBER INC. PROGRAM             LOS ANGELES, CA.       *
015300*    IS PROHIBITED.                       AN UNPUBLISHED WORK.   *
015400*                         ----------                             *
015500*                         --NOTICE--                             *
015600*                         ----------                             *
015700*                                                                *
015800*    THE IDEAS CONTAINED IN THIS PROGRAM ARE CONSIDERED PRO-     *
015900*    PRIETARY AND ARE TRADE SECRETS OF NEASI-WEBER INC.  THEY    *
016000*    MAY NOT BE REVEALED OR RELEASED WITHOUT THE EXPRESSED       *
016100*    WRITTEN CONSENT OF NEASI-WEBER INC.                         *
016200*                                                                *
016300******************************************************************
016400/                                                                 
016500 IDENTIFICATION DIVISION.                                         
016600                                                                  
016700 PROGRAM-ID.                     AMOFLPRG.                        
016800 AUTHOR.                         NEASI-WEBER INC.                 
016900*REMARKS.                        AMFBNYT.                         
017000****************************************************************  
017100*                                                              *  
017200*          *** RELEASE     3.1.0   02/21/84     ***            *  
017300*                                                              *  
017400****************************************************************  
017500*          *** RELEASE     3.0.1   09/28/83     ***            *  
017600****************************************************************  
017700*               --------------                                    
017800*               PRINT INVOICES                                    
017900*               --------------                                    
018000*        BILINV READS THE SELECT FILE CONTAINING THE WORK         
018100*        ORDER RECORDS SELECTED BY BILSEL AND PRODUCES THE        
018200*        CORRESPONDING INVOICES.  OPEN ITEMS FROM THE SELECT      
018300*        FILE ARE GROUPED ONTO AN INVOICE IF THEY HAVE THE        
018400*        SAME FIRST SEVEN DIGITS.                                 
018500*                                                                 
018600/                                                                 
018700 ENVIRONMENT DIVISION.                                            
018900 CONFIGURATION SECTION.                                           
019000                                                                  
019100 SOURCE-COMPUTER.                COPY AMZMCOMP.                   
019200 OBJECT-COMPUTER.                COPY AMZMCOMP.                   
019300 SPECIAL-NAMES.                  COPY AMZMSPEC.                   
019400                                                                  
019500 INPUT-OUTPUT SECTION.                                            
019700 FILE-CONTROL.                                                    
AL01A      SELECT optional CONTROL-FILE    
                          ASSIGN TO CTLFILE
                             ORGANIZATION IS LINE SEQUENTIAL.
000074     SELECT FTP-PARM             ASSIGN TO FTPPARM      
                             ORGANIZATION IS LINE SEQUENTIAL.
000074     SELECT acr-PARM             ASSIGN TO ACRPARM      
                             ORGANIZATION IS LINE SEQUENTIAL.
                    
019800     SELECT SEL-FILE             COPY AMZSSEL.
al-12
xxdw       SELECT PRM-FILE ASSIGN TO  PRM-FILE-NAME              
                      FILE STATUS IS PRM-FILE-STATUS.
      
019900     SELECT SSL-FILE             COPY AMZSSORT.
020000     SELECT IAR-FILE   assiGn to SYS056       
                             ORGANIZATION IS LINE SEQUENTIAL.
020100     SELECT MAG-TAPE-FILE        ASSIGN TO SYS066
                             ORGANIZATION IS LINE SEQUENTIAL.
020200     SELECT INVOICE-FILE         ASSIGN TO SYS067
                             ORGANIZATION IS LINE SEQUENTIAL.
-2001-     SELECT LOCKBOX-FILE         ASSIGN TO SYS065
                             ORGANIZATION IS LINE SEQUENTIAL.
DW-08      SELECT LOCKBOXN-FILE        ASSIGN TO SYS068
                             ORGANIZATION IS LINE SEQUENTIAL.
      *CTS - 04/12/07 CHANGE BEGINS
002501     SELECT LOCKBOXI-FILE        ASSIGN TO SYS069
                             ORGANIZATION IS LINE SEQUENTIAL.
002501     SELECT LOCKBOXA-FILE        ASSIGN TO SYS062
                             ORGANIZATION IS LINE SEQUENTIAL.
000074     SELECT FTP-FILE             ASSIGN TO SYS009
                             ORGANIZATION IS LINE SEQUENTIAL.
      *CTS - 04/12/07 CHANGE ENDS
09/07-     SELECT SETTLMT-FILE         ASSIGN TO SYS064
                             ORGANIZATION IS LINE SEQUENTIAL.
09/07-     SELECT SETTLMTI-FILE        ASSIGN TO SYS070
                             ORGANIZATION IS LINE SEQUENTIAL.
09/07-     SELECT SETTLMTA-FILE        ASSIGN TO SYS071
                             ORGANIZATION IS LINE SEQUENTIAL.
09/07-     SELECT CCWORK-FILE          ASSIGN TO SYS063
                             ORGANIZATION IS LINE SEQUENTIAL.
FXC---     SELECT CC-REPORT            ASSIGN TO SYS099
                             ORGANIZATION IS LINE SEQUENTIAL.
FXC---     SELECT CC-REPORTI           ASSIGN TO SYS098
                             ORGANIZATION IS LINE SEQUENTIAL.
FXC---     SELECT CC-REPORTA           ASSIGN TO SYS097
                             ORGANIZATION IS LINE SEQUENTIAL.

AL-03      Select Cc-Det-Bin-File        Assign to AMDBIN
AL-03             Organization is Indexed
AL-03             Access Mode  is Sequential
AL-03             Record Key   is Cc-Det-Bin-KEY
AL-03             File Status  is Cc-Det-Status-Code
AL-03                             Cc-Det-Ext-Status-Code.
002501     SELECT EUROR-INP            ASSIGN TO EURI
                             ORGANIZATION IS LINE SEQUENTIAL.
002501     SELECT optional NEWT-OUT      ASSIGN TO NEWO
                             ORGANIZATION IS LINE SEQUENTIAL.

xxdw       SELECT optional DIR-FILE    ASSIGN TO DIRFILE
                                       FILE STATUS IS DIR-FILE-STATUS 
                                       ORGANIZATION IS LINE SEQUENTIAL. 

020300/                                                                 
020400 DATA DIVISION.                                                   
020600 FILE SECTION.   

       FD  PRM-FILE                    COPY AMZFSTD.
009400 01  PRM-FILE-REC.
           03  sel3-rec.                copy amzrsel3.      

       FD  DIR-FILE                    COPY AMZFSTD. 
       01  DIR-REC                     PIC X(100). 

AL-01A FD  CONTROL-FILE.
AL-01A 01  CONTROL-REC.
AL-01A      05 CTL-REC-CNT            PIC 9(08).
AL-01A      05 CTL-REC-NET PIC s9(09)V99 SIGN IS LEADING SEPARATE.
AL-01A      05 CTL-REC-AGING          PIC 9(07).
AL-01A      05 CTL-REC-GROSS PIC s9(09)V99 SIGN IS LEADING SEPARATE.
AL-01A      05 CTL-REC-FROM           PIC 9(08).
AL-01A      05 CTL-REC-TO             PIC 9(08).
AL-01A      05 CTL-REC-TYPE           PIC X(01).
AL-01A      05 CTL-REC-ORG            PIC X(03).
AL-01A      05 CTL-REC-RUN            PIC X(21)
AL-01A      05 CTL-REC-cc-amt PIC s9(09)V99 SIGN IS LEADING SEPARATE.
                                                                        
AL-03  FD  Cc-Det-Bin-File.                                             
AL-03  01  Cc-Det-Bin-Rec.            Copy AMZRBINR.                    
020700                                                                  
DW-08- FD  LOCKBOXN-FILE               COPY AMZFSTD.                    
DW-08- 01  LOCKBOXN-REC                PIC X(80).                       
DWW-08                                                                  
-2001- FD  LOCKBOX-FILE                COPY AMZFSTD.                    
09/07- 01  LOCKBOX-REC                 PIC X(80).                       
      *CTS - 04/12/07 CHANGE BEGINS                                     
-2001- FD  LOCKBOXI-FILE               COPY AMZFSTD.                    
09/07- 01  LOCKBOXI-REC                PIC X(80).                       
-2001- FD  LOCKBOXA-FILE               COPY AMZFSTD.                    
09/07- 01  LOCKBOXA-REC                PIC X(80).                       
000082 FD  FTP-FILE                    COPY AMZFSTD.                    
000083 01  FTP-REC                     PIC  X(100).                     
      *CTS - 04/12/07 CHANGE ENDS        
000082 FD  FTP-parm                    COPY AMZFSTD.           
000083 01  FTP-parm-rec                PIC  X(80).             

000082 FD  acr-parm                    COPY AMZFSTD.           
000083 01  acr-parm-rec                PIC  X(80).             
09/07-                                                                  
09/07- FD  SETTLMT-FILE                COPY AMZFSTD.                    
09/07- 01  SETTLMT-REC                 PIC X(80).                       
09/07-                                                                  
09/07- FD  SETTLMTI-FILE               COPY AMZFSTD.                    
09/07- 01  SETTLMTI-REC                PIC X(80).                       
09/07-                                                                  
09/07- FD  SETTLMTA-FILE               COPY AMZFSTD.                    
09/07- 01  SETTLMTA-REC                PIC X(80).                       
09/07-                                                                  
09/07- FD  CCWORK-FILE                 COPY AMZFSTD.                    
09/07- 01  CCW-REC                     PIC X(53).                       
09/07-                                                                  
09/07- FD  CC-REPORT                   COPY AMZFSTD.                    
09/07- 01  CC-REPORT-REC               PIC X(133).                      
FXC---                                                                  
09/07- FD  CC-REPORTI                  COPY AMZFSTD.                    
09/07- 01  CC-REPORTI-REC              PIC X(133).                      
FXC---                                                                  
09/07- FD  CC-REPORTA                  COPY AMZFSTD.                    
09/07- 01  CC-REPORTA-REC              PIC X(133).                      
FXC---                                                                  
020800 FD  MAG-TAPE-FILE               COPY AMZFSTD.                    
020900 01  MAG-TAPE-REC                PIC X(175).                      
021000                                                                  
021100 FD  INVOICE-FILE                COPY AMZFSTD.                    
021200 01  INVOICE-REC                 PIC X(200).                      
021300                                                                  
021400 FD  SEL-FILE                    COPY AMZFSTD.                    
021500 01  SEL-REC.                    COPY AMZRSEL.                    
021600                                                                  
021700 FD  IAR-FILE                    COPY AMZFSTD.                    
021800 01  IAR-REC                     PIC X(96).                       
      *CTS - 02/23/07 CHANGE BEGINS                                     
003401 FD  EUROR-INP                   COPY AMZFSEL.                    
003402 01  EUROR-REC                   PIC X(80).                       
019401                                                                  
003401 FD  NEWT-OUT                    COPY AMZFSEL.                    
003402*01  NEWT-REC                    PIC X(960).                      
003402 01  NEWT-REC                    PIC X(1287).                     
      *CTS - 02/23/07 CHANGE ENDS                                       
021900/                                                                 
022000 SD  SSL-FILE.                                                    
022100 01  SSL-REC.                    COPY AMZRSEL2.                   
022200     03  SSL-REC-NOT-COPIED.                                      
022300         05  SSL-AGY-TYPE        PIC X(02).                       
022400         05  SSL-ADV-TYPE        PIC X(02).                       
                                                                        
022500         05  SSL-ADV-PAR-INFO.                                    
022600             10 SSL-ADV-PAR-TYPE PIC X(02).                       
022700             10 SSL-ADV-PAR-KEY  PIC X(15).                       
022800             10 SSL-ADV-PAR-NBR  PIC 9(09) comp.                  
                                                                        
022900         05  SSL-WO-INFO.   
023000             10  SSL-ISSUE-WO    PIC 9(8) comp.                   
023100             10  SSL-ISSUE-WO-X  REDEFINES SSL-ISSUE-WO           
023200                                 PIC X(04).                       
023300             10  SSL-EDITION-WO  PIC X(04).                       
023400             10  SSL-PAGE-WO     PIC X(06).                       
                                                                        
023500         05  SSL-NAT-AGY         PIC X(01).                       
023600         05  SSL-REF-NBR-2       PIC 9(08) comp.                  
                                                                        
023700     03  SSL-SORT-AGY.                                            
023800         05  SSL-S-AGY-TYPE      PIC X(02).                       
023900         05  SSL-S-AGY-KEY       PIC X(15).                       
024000         05  SSL-S-AGY-ACCT-NBR  PIC 9(09) comp.                  
                                                                        
024100     03  SSL-SORT-ADV-PAR.                                        
024200         05  SSL-S-CATEGORY      PIC X(01).                       
024300         05  SSL-S-ADV-PAR-KEY   PIC X(15).                       
024400         05  SSL-S-ADV-PAR-NBR   PIC 9(09) comp.                  
                                                                        
024500     03  SSL-SORT-ADV.                                            
024600         05  SSL-S-ADV-KEY       PIC X(15).                       
024700         05  SSL-S-ACCT-NBR      PIC 9(09) comp.                  
                                                                        
024800     03  SSL-SORT-PUB-CNT.                                        03610000
*     *CTS - 02/23/07 CHANGE BEGINS                                     03610100
024900         05  SSL-S-PUB           PIC X(04).                       03611100
AL-20          05  SSL-S-INV           PIC 9(09).                       03610301
               05  filler redefines SSL-S-INV.
                   10  SSL-S-INV-6     piC 9(06).                       03610301
                   10  SSL-S-INV-3     piC 9(03).                       03610301
                   
      *CTS - 02/23/07 CHANGE BEGINS                                     
      
           03  ssl-s-not-used.
               05  SSL-S-GROUP-CODE    PIC X(06).                       03611200
               05  SSL-S-AD-TYPE       PIC X(04).                       
               05  SSL-S-REF-NBR-X     PIC X(04).                       
                                                                        
025200     03  SSL-SORT-WO.                                             
025300         05  SSL-S-PROD-KEY      PIC X(12). 
               05  ssl-job-nbr-7       pic 9(10).
************   inside per-cnt do per-job-nbr first
025400         05  SSL-S-ISSUE-WO-X    PIC X(04).                       
025500         05  SSL-S-EDITION-WO    PIC X(04).                       
025600         05  SSL-S-PAGE-WO       PIC X(06).                       
025700         05  SSL-S-SEQ-KEY       PIC 9(04) comp.                  
025800         05  SSL-S-ADJ-KEY       PIC 9(04) comp.                  
-2001-          
           03  ssl-parm-dates.
043300         05  ssl-PARM-INVC-DATE-P PIC X(08).     
043300         05  ssl-PARM-INVC-DATE-F PIC X(08).     
043300         05  ssl-PARM-INVC-DATE-T PIC X(08).     

09/07- 01  SSL-CC-REC.                                                  
09/07-     05  SSL-CC-TYPE.                                             
09/07-         10  SSL-CC-TYPE1        PIC X(01).                       
09/07-         10  FILLER              PIC X(03).                       
09/07-     05  SSL-CC-NBR              PIC 9(16).                       
09/07-     05  SSL-CC-AUTH-CODE        PIC X(06).                       
09/07-     05  SSL-CC-MULTI-APP        PIC X(01).                       
09/07-     05  SSL-CC-EXP-DATE         PIC 9(04)    COMP.               
09/07-     05  SSL-CC-INVC-DATE        PIC 9(08)    comp.               
09/07-     05  SSL-CC-INVC-NBR         PIC 9(09)    comp.               
09/07-     05  SSL-CC-JOB-NBR          PIC 9(09)    comp.               
09/07-     05  SSL-CC-ACCT-NBR         PIC 9(09)    comp.               
09/07-     05  SSL-CC-CHRGE-AMT        PIC S9(7)V99 comp.               
FXC---     05  SSL-CC-INVC-AMT         PIC S9(7)V99 comp.               
025900/                                                                 
026000 WORKING-STORAGE SECTION.                                         
026100                                                                  
026200 01  FILLER                      PIC X(28) VALUE                  
026300                                'WORKING STORAGE STARTS HERE.'.
       01  selo-sort-key.      
               10  selo-adv-acct-nbr    pic 9(09).
               10  selo-agy-acct-nbr    pic 9(09).
               10  selo-bill-start      pic 9(08).
               10  selo-bill-end        pic 9(08).              


       01  wk-prm-name.
           05  filler                  pic x(40).
           05  wk-acct-nbr             pic x(08).  
           05  wk-prm-start            pic x(08).  
           05  wk-prm-end              pic x(08).  

DW-01  01  WK-group-code               PIC 9(06) VALUE 0.      
       01  DIR-FILE-STATUS              PIC X(02) VALUE SPACES. 
       01  EOF-SW                      PIC X(01) VALUE SPACES. 
       01  wk-multi-bsel               pic x(01) value space.
       01  RESULT                      PIC 9 COMP.
       01  FUNC                        PIC 9(02) COMP VALUE 35.
       01  COMMAND-LIN.
         05  COMMAND-LIN-LENGTH        PIC 9 COMP   VALUE  0.
       01  COMMAND-1.
           05  CMD-LINE                PIC X(128).
           05  FILLER                  PIC X VALUE X"00".
012500 01  PRM-FILE-STATUS             PIC 9(02) VALUE ZERO.
012500 01  PRM-FILE-NAME               PIC X(80).
       01  bsel-x                      pic 9(03).
       01  ENV-NAME                    PIC X(255).                      NWI001
       01  ENV-VALUE                   PIC X(255).                      NWI001 
       01  ENV-VALUE-MAX               PIC 9(09) COMP.                  NWI001 
       01  ENV-VALUE-LEN               PIC 9(09) COMP.                  NWI001 
       01  bsel-entry                  pic 9(03) value 0.
       01  bsel-entry-max              pic 9(03) value 500.
       01  bsel-file-names.
           05  bsel-name occurs 500    pic x(80).

*     *    DATA DIVISION DECLARATIONS FOR FUNCTION CURRENT-DATE
       01  Y2K-DATE-HOLD PICTURE X(21) DISPLAY 
*     *         YYYYMMDD     &     HHMMSS
       01  WS-Y2K-NUMERIC-DATE-R REDEFINES Y2K-DATE-HOLD.
           03 WS-NUMERIC-DATE             PICTURE 9(08).
           03 WS-NUMERIC-DATE-R REDEFINES WS-NUMERIC-DATE.
              05  WS-NUMERIC-YEAR         PICTURE 9(04).
              05  WS-NUMERIC-MONTH        PICTURE 9(02).
              05  WS-NUMERIC-DAY          PICTURE 9(02).
          03 WS-NUMERIC-HHMMSS           PICTURE 9(06).
          03 WS-NUMERIC-HHMMSS-R REDEFINES WS-NUMERIC-HHMMSS.
             05  WS-NUMERIC-HOUR         PICTURE 9(02).
             05  WS-NUMERIC-MINUTE       PICTURE 9(02).
             05  WS-NUMERIC-SECOND       PICTURE 9(02).
*     *              MMDDYYYY
       01  WS-NUMERIC-DATE-2               PICTURE 9(08).
       01  WS-NUMERIC-DATE-2-R REDEFINES WS-NUMERIC-DATE-2.
            05  WS-NUMERIC-MONTH-2      PICTURE 9(02).
            05  WS-NUMERIC-DAY-2        PICTURE 9(02).
            05  WS-NUMERIC-YEAR-2       PICTURE 9(04).

043800 01  wk-ftp-parm-eof             PIC X(01) VALUE space.  
       01  wk-ftp-parm-rec.                                
           05  parm-ftp-tag            pic x(06).          
           05  filler                  pic x(03).          
           05  parm-ftp-info           pic x(71).          

dw-44		   
	   01  wk-parm-acr-march           pic x(01).
	   01  wk-parm-x                   pic 9(02).
	   01  wk-x                        pic 9(02).	   
	   01  wk-parm-acr-max             pic 9(02) value 10.
	   01  wk-parm-acr-count           pic 9(02) value 0.
           88  no-acr-parm             value 0.	   
       01  wk-acr-parm-eof             PIC X(01) VALUE space.  
       01  wk-acr-parm-rec.                                
           05  parm-acr-ind            pic x(01).          
           05  filler                  pic x(01).
           05  parm-acr-in   occurs 10 pic x(05). 
		   
       01  parm-acr-1nfo.  		   
           05  parm-acr-table occurs 10
               10  parm-acr-code       pic x(04).
dw-44			   
AL-20  01 WK-BARTER-AD                 PIC X(01) VALUE SPACES.          
AL-20  01 WK-REV-CNTRY                 PIC X(04) VALUE SPACES.          
AL-20     88 VALID-REV-CNTRY  VALUE 'AUS ',                             
AL-20                               'NZ  ',                             
AL-20                               'HK  ',                             
AL-20                               'RI  ',                             
AL-20                               'J   ',                             
AL-20                               'ROK ',                             
AL-20                               'MAL ',                             
AL-20                               'IND ',                             
AL-20                               'RP  ',                             
AL-20                               'SGP ',                             
AL-20                               'RC  ',                             
AL-20                               'TW  ',                             
AL-20                               'MAC ',                             
AL-20                               'PAK ',                             
AL-20                               'BD  ',                             
AL-20                               'T   ',                             
AL-20                               'CL  '.                             
       01  WK-NAD-VAT-ID               PIC X(14) VALUE SPACES.          
       01  WK-NAD-VAT-BILL             PIC X(04) VALUE SPACES.          
026400                                                                  
AL-07  01  PPD-CC-AMT                  PIC S9(8)V99 VALUE ZERO.         
AL-07  01  PPD-PPD-AMT                 PIC S9(8)V99 VALUE ZERO.         
AL-06A 01  DETAIL-DATA                 PIC X(01)    VALUE SPACE.        
AL-03  01  CC-DONE-READING             Pic  X(01)   Value SPACE.        
AL-03  01  CC-DET-STATUS-CODE          Pic  9(02)   Value Zeros.        
AL-03  01  CC-DET-EXT-STATUS-CODE.                                      
AL-03      05 CC-DET-RETURN            PIC S9(4) COMP VALUE 0.          
AL-03      05 CC-DET-FUNCTION          PIC S9(4) COMP VALUE 0.          
AL-03      05 CC-DET-FEEDBACK          PIC S9(4) COMP VALUE 0.          
AL-03  01  CC-NBR                      PIC 9(16).                       
AL-03  01  CC-NBR-RD REDEFINES CC-NBR.                                  
AL-03      05 CC-NBR-9                 PIC 9(9).                        
AL-03      05 CC-NBR-7                 PIC 9(7).                        
AL-03                                                                   
AL-03  01  Ws-Date-yymmdd              Pic x(06) Value Space.           
AL-03  01  Ws-Usa                      Pic X(02) Value 'US'.            
AL-03  01  Ws-Card                     Pic x(01) Value Spaces.          
AL-03      88 Ws-M                     Value 'M'.                       
AL-03      88 Ws-V                     Value 'V'.                       
AL-03  01  Ws-Card-Type                Pic x(01) Value Spaces.          
AL-03      88 Ws-B                     Value 'B'.                       
AL-03      88 Ws-P                     Value 'P'.                       
AL-03  01  ws-M-Da2-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-V-Da2-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-V-Da1-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-M-Da1-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-V-Ar3-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-V-Ar2-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-M-Ar2-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-V-Ar1-Count              Pic 9(06) Value Zeros.           
AL-03  01  ws-M-Ar1-Count              Pic 9(06) Value Zeros.           
AL-03  01  Ws-Edit-Amt                 Pic ------9.99.                  
AL-03  01  WS-CC-DOL                   PIC 9(08)V99 VALUE 0.            
AL-06B 01  WK-GROUP-BUY                PIC X(1) VALUE SPACES.           
AL-06  01  WK-TEMP-GROUP-RATE          PIC S9(06)V99 VALUE +0.          
AL-06  01  WS-DECIMAL                  PIC Z(10).                       
AL-06  01  TTL-GB-NBR                  PIC 9(8) VALUE 0.                
AL-06  01  SWAP-DONE                   PIC X(01).                       
AL-06  01  WS-IDX                      PIC 9(04) comp.                  
AL-06  01  WS-GCT-X                PIC 9(06).                           
AL-06  01  GCT-MAX-ENTRY-1         PIC 9(06).                           
AL-06  01  WK-SAVE-GROUP-CODE      PIC X(06) VALUE SPACES.              
AL-06  01  PROCESS-GROUP-CODE      PIC X(01) VALUE SPACES.              
AL-06  01  WK-GRP-INV-AMT          PIC S9(09)V99.                       
AL-06  01  WK-GRP-GRS-AMT          PIC S9(09)V99.                       
AL-06  01  GCT-HOLD-ENTRY          PIC 9(04) comp VALUE ZERO.           
AL-06                                                                   
AL-06**01  TEMP-GCT-ENTRY          PIC X(381).                          
AL-06  01  TEMP-GCT-ENTRY          PIC X(387).                          
AL-06**01  HOLD-SSL-REC            PIC X(376).                          
AL-06  01  HOLD-SSL-REC            PIC X(382).                          
AL-06   01 GROUP-CONTROL-TABLE.                                       
AL-06      05  GCT-MAX-ENTRY           PIC 9(04) comp VALUE ZERO.     
AL-06      05  GCT-ENTRY               OCCURS 20  TIMES               
AL-06                                  INDEXED BY GCT-X GCT-X2.       
AL-06          10  GCT-USED            PIC X(01).                     
AL-06          10  GCT-ADTP            PIC X(04).                     
AL-06          10  GCT-SEL-REC.    



002500         15    GCT-ACCT-NBR      PIC 9(09) COMP.
002600         15    GCT-PUB           PIC X(04).
002700         15  GCT-REF-NBR         PIC 9(08) COMP.
002800       15  GCT-ISSUE             PIC 9(08) COMP.
002900       15  GCT-SEQ-KEY           PIC 9(04) COMP.
003000       15  GCT-ADJ-KEY           PIC 9(04) COMP.
003200     15  GCT-JOB-NBR             PIC 9(18) COMP.                
003300     15  GCT-AGY-ACCT-NBR        PIC 9(09) COMP.
003400     15  GCT-SLS-TEAM.
A22005       20  GCT-SLSMAN            OCCURS 4  INDEXED BY GCT-SX.
003600         25  GCT-SLS-NBR         PIC 9(04) COMP.
003700         25  GCT-SLS-PCT         PIC 9(03) COMP.
003800     15  GCT-EDITION             PIC X(04).
003900     15  GCT-INVC-DATE           PIC 9(08) COMP.
004000     15  GCT-ADV-KEY             PIC X(15).
004100     15  GCT-CATEGORY            PIC X(01).
004200     15  GCT-STATUS              PIC X(01).
004300     15  GCT-AGY-KEY             PIC X(15).
004400     15  GCT-PD-FLAG             PIC X(01).
004500     15  GCT-ACR-FLAG            PIC X(01).
004600     15  GCT-ACR-INVC-CODE       PIC X(04).
005000***  15  GCT-ISS-JOB             PIC 9(08) COMP.                
005100     15  GCT-ISS-JOB             PIC 9(18) COMP.                
005200*    ... ACR-CODE  -  ZERO
005300     15  GCT-CNT-NBR             PIC 9(08) COMP.
005400     15  GCT-LINE-RATE           PIC S9(06)V99 COMP.
005500     15  GCT-NAD-TYPE            PIC X(02).
005600     15  GCT-CMB-ADTP            PIC X(04).
005700     15  GCT-CMB-EDTN            PIC X(04).
005800***  15  GCT-CRD-NBR             PIC 9(16).                 
005900     15  GCT-CRD-NBR             PIC X(16).                     
006000     15  GCT-CRD-SEQ             PIC 9(04).                     
006000     15  GCT-MPUB-FLAG           PIC X(01).                     
006000     15  GCT-BBUY-INVC-NBR       PIC 9(18) COMP.                
      ****added fields from nyt
           15 GCT-SPC-HAND             PIC X(01).                     
	  15 GCT-WO-ACR-CODE          PIC X(04).                      
		15 GCT-WO-INVC-CODE         PIC X(04).                
		15 GCT-WO-PAGE              PIC X(06).                
		15 GCT-WO-INVC-AMT          PIC S9(07)V99 COMP  .     
		15 GCT-WO-LINE-COUNT        PIC 9(04) COMP  .         
		15 GCT-WO-PROD-KEY     PIC X(12).                     
           15 GCT-NAD-ZIP              PIC X(09).                     
	    15 GCT-NAD-TEL-NBR          PIC X(10).                    
	    15 GCT-BTA-JXRF.                                          
             20 GCT-BTA-JXRF-PRE      PIC X(01).                      
  	     20 GCT-BTA-JXRF-NBR      PIC X(05).                      
           15 GCT-WO-CLASS             PIC X(04).                     
	    15 GCT-NAD-ADDR-CODE        PIC X(01).                    
	    15 GCT-WO-SP-CHG-TOT        PIC  9(07)V99 COMP  .         
	    15 GCT-AD-TYPE              PIC X(04).                    
	    15 GCT-CLS-SPC-HAND         PIC X(01).                    
	    15 GCT-ADV-JXRF.                                          
              20 GCT-ADV-JXRF-PRE      PIC X(01).                     
	      20 GCT-ADV-JXRF-NBR      PIC X(05).                       
           15 GCT-WO-AD-SIZE           PIC X(04).                     
		   15 GCT-WO-AD-QUANTITY       PIC 9(16)V99  COMP  .  
		   15 GCT-WO-AD-TYPE           PIC X(04).             
		   15 GCT-WO-AGY-COMM          PIC S9(07)V99 COMP  .  
		   15 GCT-WO-GROSS-AMT         PIC S9(07)V99 COMP  .  
      ***  15 FILLER                   PIC X(20).                     
           15 GCT-WO-JOB-NBR           PIC 9(13).                     
           15 GCT-WO-JOB-NBR-RE REDEFINES GCT-WO-JOB-NBR.             
              20 GCT-WO-JOB-NBR-7      PIC 9(10).                     
	      20 GCT-WO-JOB-NBR-2      PIC 9(03).                     
AL-01 ***  15 FILLER                   PIC X(11).                     
AL-01      15 GCT-WO-GROUP-CODE        PIC X(06).                     
AL-01      15 FILLER                   PIC X(14).                     



AL-06              15 GCT-REC-NOT-COPIED.                               
AL-06                  25 GCT-AGY-TYPE PIC X(02).                       
AL-06                  25 GCT-ADV-TYPE PIC X(02).                       
AL-06                                                                   
AL-06                  25 GCT-ADV-PAR-INFO.                             
AL-06                      30 GCT-ADV-PAR-TYPE PIC X(02).               
AL-06                      30 GCT-ADV-PAR-KEY PIC X(15).                
AL-06                      30 GCT-ADV-PAR-NBR PIC 9(09) comp.           
AL-06                                                                   
AL-06                  25 GCT-WO-INFO.                                  
AL-06                      30 GCT-ISSUE-WO PIC 9(08) comp.              
AL-06                      30 GCT-ISSUE-WO-X REDEFINES GCT-ISSUE-WO     
AL-06                                          PIC X(04).               
AL-06                      30 GCT-EDITION-WO PIC X(04).                 
AL-06                      30 GCT-PAGE-WO PIC X(06).                    
AL-06                                                                   
AL-06                  25 GCT-NAT-AGY  PIC X(01).                       
AL-06                  25 GCT-REF-NBR-2 PIC 9(08) comp.                 
AL-06                                                                   
AL-06              15 GCT-SORT-AGY.                                     
AL-06                  25 GCT-S-AGY-TYPE PIC X(02).                     
AL-06                  25 GCT-S-AGY-KEY PIC X(15).                      
AL-06                  25 GCT-S-AGY-ACCT-NBR PIC 9(09) comp.            
AL-06                                                                   
AL-06              15 GCT-SORT-ADV-PAR.                                 
AL-06                  25 GCT-S-CATEGORY PIC X(01).                     
AL-06                  25 GCT-S-ADV-PAR-KEY PIC X(15).                  
AL-06                  25 GCT-S-ADV-PAR-NBR PIC 9(09) comp.             
AL-06                                                                   
AL-06              15 GCT-SORT-ADV.                                     
AL-06                  25 GCT-S-ADV-KEY PIC X(15).                      
AL-06                  25 GCT-S-ACCT-NBR PIC 9(09) comp.                
AL-06                                                                   
AL-06              15 GCT-SORT-PUB-CNT.                                 
AL-06                  25 GCT-S-GROUP  PIC X(06).                       
AL-06                  25 GCT-S-PUB    PIC X(04).                       
AL-06                  25 GCT-S-AD-TYPE PIC X(04).                      
AL-06                  25 GCT-S-REF-NBR-X PIC X(04).                    
AL-06                                                                   
AL-06              15 GCT-SORT-WO.                                      
AL-06                  25 GCT-S-PROD-KEY PIC X(12).                     
AL-06                  25 GCT-S-ISSUE-WO-X PIC X(04).                   
AL-06                  25 GCT-S-EDITION-WO PIC X(04).                   
AL-06                  25 GCT-S-PAGE-WO PIC X(06).                      
AL-06                  25 GCT-S-SEQ-KEY PIC 9(04) comp.                 
AL-06                  25 GCT-S-ADJ-KEY PIC 9(04) comp.                 
AL-06                                                                   
AL-06  01  WS-MISC-GROUP-BUY           PIC X(01).                       
026500 01  WK-MAG-TAPE-REC.            COPY AMZRMAG.                    
026600                                                                  
-2005- 01  Ws-Invc-Code                Pic X(01) Value Space.           
08/30-     88  CrCard Value 'A', 'D', 'M', 'V', 'S'.                    
fxc---     88  NytdCc Value 'N'.                                        
026600                                                                  
-2001- 01  FIRST-DATA-REC.             COPY AMZRFDMS.                   
09/07-                                                                  
09/07- 01  N                           PIC S9(9) COMP VALUE ZEROS.      
09/07- 01  WS-CC-NUMBER.                                                
09/07-     05 WS-CC-NBR                PIC ZZZZZZZZZZZZZZZ9.            
09/07-                                                                  
09/07- 01  WK-CURRENT-DATE             PIC X(08)    VALUE SPACE.        
09/07- 01  WK-LAST-CC-TYPE             PIC X(04)    VALUE SPACE.        
09/07- 01  WK-TTL-INVC-COUNT           PIC 9(09)    VALUE 0 comp.       
09/07- 01  WK-TTL-CC-AMT               PIC 9(08)V99 VALUE 0.            
09/07- 01  WK-TTL-CCI-AMT              PIC 9(08)V99 VALUE 0.            
09/07- 01  WK-TTL-CCA-AMT              PIC 9(08)V99 VALUE 0.            
09/07- 01  WK-TTL-CC-CHRG-AMT          PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TTL-CC-CHRGI-AMT         PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TTL-CC-CHRGA-AMT         PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CC-AMT               PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CCI-AMT              PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CCA-AMT              PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CC-CHRG-AMT          PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CC-CHRGI-AMT         PIC 9(07)V99 VALUE 0.            
09/07- 01  WK-TOT-CC-CHRGA-AMT         PIC 9(07)V99 VALUE 0.            
DW-08  01  WK-TTL-CC-AMT-N             PIC 9(08)V99       VALUE 0.      
09/07- 01  WK-TOT-CHRG-AMT             PIC S9(7)V99 VALUE 0.            
09/07- 01  WK-TOT-CHRGI-AMT            PIC S9(7)V99 VALUE 0.            
09/07- 01  WK-TOT-CHRGA-AMT            PIC S9(7)V99 VALUE 0.            
09/07- 01  WK-TOT-CC-COUNT             PIC 9(06)    VALUE 0.            
09/07- 01  WK-TOT-CCI-COUNT            PIC 9(06)    VALUE 0.            
09/07- 01  WK-TOT-CCA-COUNT            PIC 9(06)    VALUE 0.            
09/07- 01  WK-TTL-CC-COUNT             PIC 9(06)    VALUE 0.            
09/07- 01  WK-TTL-CCI-COUNT            PIC 9(06)    VALUE 0.            
09/07- 01  WK-TTL-CCA-COUNT            PIC 9(06)    VALUE 0.            
DW-08  01  WK-TTL-CC-COUNT-N           PIC 9(06)          VALUE 0.      
      *CTS - 07/02/07 CHANGE BEGINS                                     
CTS-02 01  WK-TTL-CC-COUNT-I           PIC 9(06)          VALUE 0.      
CTS-02 01  WK-TTL-CC-COUNT-A           PIC 9(06)          VALUE 0.      
CTS-02 01  WK-LASTCC-AUTH              PIC X(06)    VALUE SPACES.       
      *CTS - 07/02/07 CHANGE ENDS                                       
09/07- 01  WK-CC-LINE-COUNT            PIC 9(02)    VALUE 99.           
09/07- 01  WK-CC-LINEI-COUNT           PIC 9(02)    VALUE 99.           
09/07- 01  WK-CC-LINEA-COUNT           PIC 9(02)    VALUE 99.           
09/07- 01  WK-CC-PAGE-COUNT            PIC 9(06)    VALUE 0.            
09/07- 01  WK-CC-PAGEI-COUNT           PIC 9(06)    VALUE 0.            
09/07- 01  WK-CC-PAGEA-COUNT           PIC 9(06)    VALUE 0.            
DW-08  01  WK-TOT-N-COUNT              PIC 9(06)          VALUE 0.      
FXC---                                                                  
AL-28  01  WK-NET-DTL-AMT              PIC S9(08)V99 VALUE 0.           
AL-05                                                                   
AL-05                                                                   
AL-05  01 WK-CC-DATA.                                                   
AL-05      05  WK-CC-TYPE              PIC X(01).                       
AL-05      05  WK-CC-EXP-DATE.                                          
AL-05          10  WK-CC-EXP-MM        PIC X(02).                       
AL-05          10  WK-CC-EXP-YY.                                        
AL-05              15  WK-CC-EXP-YY1   PIC X(01).                       
AL-05              15  WK-CC-EXP-YY2   PIC X(01).                       
AL-05      05  WK-CC-COMMENT.                                           
AL-05          10  WK-CC-NBR           PIC X(16).                       
AL-05          10  FILLER              PIC X(02).                       
AL-05          10  WK-CC-AUTH          PIC X(06).                       
AL-05  01 WK-CC-EXP-DATE-NUM           PIC 9(04) VALUE 0.               
AL-05                                                                   
AL-05                                                                   
026700 01  TOT-TABLE-MAX               PIC 9(02) VALUE 50.              
026800 01  TOT-TABLE-COUNT             PIC 9(02) VALUE 0.               
                                                                        
026900 01  TOT-TABLE.                                                   
027000     05  TOT-LINE OCCURS 50 INDEXED BY TOT-X.                     
027200         10  TOT-DATA.                                            
027300             15  TOT-ROUTE       PIC X(01).                       
027400             15  TOT-COUNT       PIC 9(09) comp. 
       01  wk-edtn-x                   pic 9(02).
       01  edtn-table-count            pic 9(03).
       01  edtn-table-MAX              pic 9(02) value 90.
026900 01  edtn-table.                                                
027000     05  edtn-LINE OCCURS 90 INDEXED BY edtn-X.                     
027200         10  edtn-DATA.                                            
027300             15  edtn-job-nbr    PIC 9(18).                       
027400             15  edtn-pub        PIC x(04).                  
027400             15  edtn-edtn       PIC x(04).                  
027400             15  edtn-section    PIC x(04).                  
027400             15  edtn-ad-posn    PIC x(04).                  

027600 01  PAGE-TABLE-MAX              PIC 9(02) VALUE 90.              
027700 01  PAGE-TABLE-COUNT            PIC 9(02) VALUE 0.               
                                                                        
027800 01  PAGE-TABLE.                                                  
027900     05  PAGE-LINE OCCURS 90 INDEXED BY PAGE-X.                   
028100         10 PAGE-DATA.                                            
028200            15 PAGE-CB           PIC X(01).                       
028300            15 PAGE-FONT         PIC X(01).                       
028400            15 PAGE-ROUTE        PIC X(01).                       
028500            15 PAGE-PAGE         PIC X(01).                       
028600            15 FILLER            PIC X(150).                      
028700         10 PAGE-KEY.                                             
028800            15 PAGE-ACTT         PIC X(02).                       
028900            15 PAGE-INVC-COUNT   PIC 9(08).                       
029000            15 PAGE-PAGE-NBR     PIC 9(04).                       
029100            15 PAGE-LINE-NBR     PIC 9(04).                       
NYTMS3*********   15 PAGE-ACCT-NBR     PIC X(20).                       
NYTMS3            15 PAGE-ACCT-NBR     PIC X(10).                       
NYTMS3            15 PAGE-STMT-BREAK   PIC X(01).                       
NYTMS3*           15 FILLER            PIC X(09).                       
AL-06B            15 PAGE-GROUP-BUY    PIC X(01).                       
AL-06B            15 FILLER            PIC X(08).                       
029300                                                                  
054800 01  WK-SORT-NAME                PIC X(20).                       
054800 01  WK-MAT-COMMENT.                                              
054900     05  WK-MAT-NAME             PIC X(16).                       
055000     05  FILLER                  PIC X(02).                       
055100     05  WK-MAT-CC-AUTH          PIC X(06).                       
                                                                        
       01  WK-REF-NBR.                                                  
           05  FILLER                  PIC X(01).                       
           05  WK-REF-NBR-9            PIC X(09).                       

A22003 01  MFLD-PKT.                   COPY AMZWMFLD. 
A22003 01  RATE-PKT.                   COPY AMZWRATE.

		   
029400 01  WK-CDT-AREA.                                                 
029500     05  WK-CDT-ACCT-NBR         PIC 9(09)     COMP VALUE 0.      
029600     05  WK-CDT-AGY-ACCT-NBR     PIC 9(09)     COMP VALUE 0.      
029700     05  WK-CDT-INVC-AMT         PIC S9(07)V99 COMP VALUE 0.      
029800                                                                  
029900* 01  INPUT-CHARS.                                                
030000*     05  INPUT-CHAR-1            PIC X(01).                      
030100*     05  INPUT-CHAR-2            PIC X(01).                      
030200*     05  INPUT-CHAR-3            PIC X(01).                      
030300*     05  INPUT-CHAR-4            PIC X(01).                      
030400*                                                                 
030500* 01  TARGET-CHARS.                                               
030600*     05  TARGET-CHAR-1           PIC X(01).                      
030700*     05  TARGET-CHAR-2           PIC X(01).                      
030800*     05  TARGET-CHAR-3           PIC X(01).                      
030900*     05  TARGET-CHAR-4           PIC X(01).                      
031000                                                                  
031100 01  WK-ABORT-CODES.                                              
031300*>   --- Invalid admarc publication code                          
031400     05  IN01                    PIC X(04) VALUE 'IN01'.          
031600*>   --- "NINB" Type code record does not exist                   
031700     05  IN02                    PIC X(04) VALUE 'IN02'.          
031900*>   --- Read for update failed on "NINB" type code record        
032000     05  IN03                    PIC X(04) VALUE 'IN03'.          
032200*>   --- Write failed on "NINB" type code record                  
032300     05  IN04                    PIC X(04) VALUE 'IN04'.          
032500*>   --- No advertiser record found for selected work order       
032600     05  IN05                    PIC X(04) VALUE 'IN05'.          
032800*>   --- Read failed on "NATP" type code record                   
032900     05  IN06                    PIC X(04) VALUE 'IN06'.          
033100*>   --- No advertiser found for "NATP" type code record          
033200     05  IN07                    PIC X(04) VALUE 'IN07'.          
033400*>   --- Read failed on "ITRM" type code record                   
033500     05  IN08                    PIC X(04) VALUE 'IN08'.          
033700*>   --- Read failed on Message file for Msg# from "ITRM" code    
033800     05  IN09                    PIC X(04) VALUE 'IN09'.          
034000*>   --- Read failed on Agency file for selected work order       
034100     05  IN10                    PIC X(04) VALUE 'IN10'.          
034300*>   --- Read failed on Parent of Adv or Agy for selected W.O.    
034400     05  IN11                    PIC X(04) VALUE 'IN11'.          
034600*>   --- Read failed on work order which was selected             
034700     05  IN12                    PIC X(04) VALUE 'IN12'.          
034900*>   --- Read failed on code record for "PBSZ"                    
035000     05  IN13                    PIC X(04) VALUE 'IN13'.          
035200*>   --- No effective issue in code record                        
035300     05  IN14                    PIC X(04) VALUE 'IN14'.          
035500*>   --- End of effective issues in code record                   
035600     05  IN15                    PIC X(04) VALUE 'IN15'.          
035800*>   --- Read failed on Adv/Agy Xrf file                          
035900     05  IN16                    PIC X(04) VALUE 'IN16'.          
036100*>   --- Read failed on Reg/Mct contract - from "SAVE" area       
036200     05  IN17                    PIC X(04) VALUE 'IN17'.          
036400*>   --- Parm card - Account type code not ' ' and 'S' (Short)    
036500     05  IN18                    PIC X(04) VALUE 'IN18'.          
036700*>   --- Cannot open interface to A/R output file                 
036800     05  IN19                    PIC X(04) VALUE 'IN19'.          
037000*>   --- Cannot close interface to A/R output file                
037100     05  IN20                    PIC X(04) VALUE 'IN20'.          
037300*>   --- Cannot write interface to A/R output file                
037400     05  IN21                    PIC X(04) VALUE 'IN21'.          
037600*>   --- Cannot open interface to old invoices file -Detail-      
037700     05  IN22                    PIC X(04) VALUE 'IN22'.          
037900*>   --- Cannot close interface to old invoices file -Detail-     
038000     05  IN23                    PIC X(04) VALUE 'IN23'.          
038200*>   --- Cannot write interface to old invoices file -Detail-     
038300     05  IN24                    PIC X(04) VALUE 'IN24'.          
038500*>   --- Cannot open interface to old invoices file -Header-      
038600     05  IN25                    PIC X(04) VALUE 'IN25'.          
038800*>   --- Cannot close interface to old invoices file -Header-     
038900     05  IN26                    PIC X(04) VALUE 'IN26'.          
039100*>   --- Cannot write interface to old invoices file -Header-     
039200     05  IN27                    PIC X(04) VALUE 'IN27'.          
039400*>   --- Cannot open interface to old invoices file -Agency-      
039500     05  IN28                    PIC X(04) VALUE 'IN28'.          
039700*>   --- Cannot close interface to old invoices file -Agency-     
039800     05  IN29                    PIC X(04) VALUE 'IN29'.          
040000*>   --- Cannot write interface to old invoices file -Agency-HR   
040100     05  IN30                    PIC X(04) VALUE 'IN30'.          
040300*>   --- Cannot write interface to old invoices file -Agency-DR   
040400     05  IN31                    PIC X(04) VALUE 'IN31'.          
040600*>   --- Read failed on Reg/Mct contract - from "SSL" area        
040700     05  IN32                    PIC X(04) VALUE 'IN32'.          
040900*>   --- Cannot close interface to old invoices file -Detail-CHGDD
041000     05  IN33                    PIC X(04) VALUE 'IN33'.          
041200*>   --- Cannot close interface to old invoices file -Header-CHGDD
041300     05  IN34                    PIC X(04) VALUE 'IN34'.          
041500*>   --- Cannot execute "CHGDDN" function -Detail-                
041600     05  IN35                    PIC X(04) VALUE 'IN35'.          
041800*>   --- Cannot execute "CHGDDN" function -Header-                
041900     05  IN36                    PIC X(04) VALUE 'IN36'.          
042100*>   --- Cannot open interface to old invoices file -Detail-CHGDDN
042200     05  IN37                    PIC X(04) VALUE 'IN37'.          
042400*>   --- Cannot open interface to old invoices file -Header-CHGDDN
042500     05  IN38                    PIC X(04) VALUE 'IN38'.          
AL-08 *>   --- Gold table needs expanding                               
AL-08      05  IN39                    PIC X(04) VALUE 'IN39'.          
AL-08 *>   --- CANT FIND GOLD WEB ORDER                                 
AL-08      05  IN40                    PIC X(04) VALUE 'IN40'.          
AL-15 *>   --- PLATINUM table needs expanding                           
AL-15      05  IN41                    PIC X(04) VALUE 'IN41'.          
AL-15 *>   --- CANT FIND PLATINUM WEB ORDER                             
AL-15      05  IN42                    PIC X(04) VALUE 'IN42'.          
      *CTS - 02/23/07 CHANGE BEGINS                                     
      *>   --- EURO-REACH LOOK-UP TABLE NEEDS EXPANDING                 
           05  IN43                    PIC X(04) VALUE 'IN43'.          
      *>   --- EURO-REACH AMOUNTS TABLE NEEDS EXPANDING                 
           05  IN44                    PIC X(04) VALUE 'IN44'.          
      *CTS - 02/23/07 CHANGE ENDS                                       
042600                                                                  
042700 01  PARM-CARD.                                                   
042800     05  PARM-PUB                PIC X(04) VALUE SPACES.          
042900     05  PARM-PFM-DATE           PIC X(06) VALUE SPACES.          
043000     05  PARM-ACTT-CODE          PIC X(01) VALUE SPACES.          
043100     05  PARM-IAR-FLAG           PIC X(01) VALUE SPACES.          
043200     05  PARM-IOI-FLAG           PIC X(01) VALUE SPACES.          
043300     05  PARM-INVC-DATE-P        PIC X(06) VALUE SPACES.          
043400     05  FILLER                  PIC X(06) VALUE SPACES.          
043300     05  PARM-INVC-DATE-F        PIC X(06) VALUE SPACES.          
043300     05  PARM-INVC-DATE-T        PIC X(06) VALUE SPACES.          
043400     05  FILLER                  PIC X(01) VALUE SPACES.          
043300     05  PARM-INVC-TYPE          PIC X(02) VALUE SPACES.          
      *CTS - 07/05/07 CHANGE BEGINS                                     
043400***  05  FILLER                  PIC X(40) VALUE SPACES.          
043400     05  PARM-UPD-FLAG           PIC X(01) VALUE SPACES.          
043400     05  FILLER                  PIC X(01) VALUE SPACES.          
043400     05  PARM-TYPE               PIC X(01) VALUE SPACES.          
043400     05  FILLER                  PIC X(01) VALUE SPACES.          
043400     05  PARM-ORG                PIC X(03) VALUE SPACES.          
043400     05  FILLER                  PIC X(01) VALUE SPACES.          
xxdw       05  PARM-code-include       PIC X(04) VALUE SPACES.          
xxdw       05  FILLER                  PIC X(28) VALUE SPACES.          
      *CTS - 07/05/07 CHANGE ENDS                                       
043500                                                                  
043600*>   ---  File status indicators - OPN, CLS, EOF                  
043700 01  SSL-FILE-STATUS             PIC X(03) VALUE 'CLS'.           
043800 01  SEL-FILE-STATUS             PIC X(03) VALUE 'CLS'.           
043900 01  WK-IAR-STATUS               PIC X(02) VALUE '00'.            
044000 01  WK-IOID-STATUS              PIC X(02) VALUE '00'.            
044100 01  WK-IOIH-STATUS              PIC X(02) VALUE '00'.            
044200 01  WK-IOIA-STATUS              PIC X(02) VALUE '00'.            
044300                                                                  
044400 01  WK-FIRST-TIME               PIC X(01) VALUE SPACE.           
044500 01  WK-FIRST-LINE               PIC X(01) VALUE SPACE.           
044600 01  WK-READY-TOTAL              PIC X(01) VALUE SPACE.           
044700 01  WK-MORE-CNT                 PIC X(01) VALUE SPACE.           
044800 01  WK-NON-ZERO                 PIC X(01) VALUE SPACE.           
044900 01  WK-CLS-PUB                  PIC X(04) VALUE 'CLAS'.          
045000 01  WK-MAGC-PUB                 PIC X(04) VALUE 'MAGC'.          
      *CTS - 02/23/07 CHANGE BEGINS                                     
045000 01  WK-IHTC-PUB                 PIC X(04) VALUE 'IHTC'.          
      *CTS - 02/23/07 CHANGE ENDS                                       
045000 01  WK-NCOM-PUB                 PIC X(04) VALUE 'NCOM'.          
045100 01  WK-FSI-PUB                  PIC X(04) VALUE 'FSIN'.          
045200 01  WK-NYT-PUB                  PIC X(04) VALUE 'NYT '.          
045200 01  WK-NYTL-PUB                 PIC X(04) VALUE 'NYTL'.          
045300                                                                  
045400 01  WK-INVCFORM-DD              PIC X(08) VALUE 'INVCFORM'.      
045500 01  WK-SYS031-DD                PIC X(08) VALUE 'SYS031'.        
045600 01  WK-SYS060-DD                PIC X(08) VALUE 'SYS060'.        
045700 01  WK-SYS061-DD                PIC X(08) VALUE 'SYS061'.        
045800                                                                  
045900 01  WK-NAD-TYPE                 PIC X(01) VALUE SPACE.           
046000 01  WK-WO-AD-TYPE               PIC X(03) VALUE SPACE.           
046100 01  WK-CHR-1                    PIC X(01) VALUE SPACE.           
046200 01  WK-CHR-2                    PIC X(01) VALUE SPACE.           
046300 01  WK-IOI-SWITCH               PIC X(01) VALUE SPACE.           
046400 01  WK-TOTAL-TYPE               PIC X(01) VALUE SPACE.           
046500 01  WK-AR-REC-COUNT             PIC 9(02) VALUE ZEROS.           
046600 01  WK-BLANK-FOUND              PIC X(01) VALUE SPACE.           
046700 01  TEMP-IX                     PIC 9(02) VALUE ZEROS.           
046800                                                                  
046900 01  WK-TERMS                    PIC X(03).                       
047000 01  WK-GL-CODES.                                                 
047100     05 WK-GL-SUBLED             PIC X(02).                       
047200     05 WK-GL-LED                PIC X(04).                       
047300                                                                  
       01  WK-INVC-TMP                 PIC 9(12).                       
       01  WK-INVC-TMP-X               REDEFINES WK-INVC-TMP.           
AL-06 *    05 WK-INVC-TMP-6            PIC 9(06).                       
AL-06      05 WK-INVC-TMP-6            PIC Z(09).                       
           05 WK-INVC-TMP-3            PIC 9(03).                       
                                                                        
AL-08  01  WK-mega-GROSS-AMT           PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-mega-BLEED-AMT           PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-mega-INVC-AMT            PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-mega-adj-AMT            PIC S9(07)V99 VALUE +0.          

AL-08  01  WK-mega-override            PIC X(01) VALUE SPACES.          
AL-08  01  WK-mega-IND                 PIC X(01) VALUE SPACES.          
       01  wk-mega-job-nbr             pic 9(18).
       01  wk-mega-job-sum             pic 9(12)v99.
       
AL-08  01  mega-TABLE-COUNT            PIC 9(03) VALUE 0.               
AL-08  01  WK-JOB-mega                 PIC 9(13).                       
AL-08  01  WK-JOB-mega-RD REDEFINES WK-JOB-mega.                        
AL-08      05 WK-JOB-mega-7            PIC 9(10).                       
AL-08      05 WK-JOB-mega-2            PIC 9(03).                       
AL-08  01  mega-TABLE-MAX              PIC 9(02) VALUE 90.              
AL-08  01  mega-TABLE.                                                  
AL-08      05  mega-LINE OCCURS 99 INDEXED BY mega-X.                   
AL-08          10  mega-DATA.                                           
AL-08              15  mega-NBR         PIC 9(13).                      
AL-08              15  mega-NBR-RD REDEFINES mega-NBR.                  
AL-08                25  mega-NBR-7     PIC 9(10).                      
AL-08                25  mega-NBR-2     PIC 9(03).                      
AL-08              15  mega-GROSS-AMT   PIC S9(07)V99.                  
AL-08              15  mega-BLEED-AMT   PIC S9(07)V99.                  
AL-08              15  mega-INVC-AMT    PIC S9(07)V99.                  
AL-08              15  mega-adj-AMT     PIC S9(07)V99.                  
AL-08              15  mega-edtn        PIC x(04).                      
AL-08              15  mega-pub         PIC x(04).                      

AL-08                                                                   
AL-08  01  WK-GOLD-GROSS-AMT           PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-GOLD-BLEED-AMT           PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-GOLD-INVC-AMT            PIC S9(07)V99 VALUE +0.          
AL-08  01  WK-GOLD-adj-AMT            PIC S9(07)V99 VALUE +0.          

AL-08  01  WK-GOLD-IND                 PIC X(01) VALUE SPACES.          
AL-08  01  GOLD-TABLE-COUNT            PIC 9(03) VALUE 0.               
AL-08  01  WK-JOB-GOLD                 PIC 9(13).                       
AL-08  01  WK-JOB-GOLD-RD REDEFINES WK-JOB-GOLD.                        
AL-08      05 WK-JOB-GOLD-7            PIC 9(10).                       
AL-08      05 WK-JOB-GOLD-2            PIC 9(03).                       
AL-08  01  GOLD-TABLE-MAX              PIC 9(02) VALUE 90.              
AL-08  01  GOLD-TABLE.                                                  
AL-08      05  GOLD-LINE OCCURS 99 INDEXED BY GOLD-X.                   
AL-08          10  GOLD-DATA.                                           
AL-08              15  GOLD-NBR         PIC 9(13).                      
AL-08              15  GOLD-NBR-RD REDEFINES GOLD-NBR.                  
AL-08                25  GOLD-NBR-7     PIC 9(10).                      
AL-08                25  GOLD-NBR-2     PIC 9(03).                      
AL-08              15  GOLD-GROSS-AMT   PIC S9(07)V99.                  
AL-08              15  GOLD-BLEED-AMT   PIC S9(07)V99.                  
AL-08              15  GOLD-INVC-AMT    PIC S9(07)V99.                  
AL-08              15  GOLD-adj-AMT     PIC S9(07)V99.                  

AL-08                                                                   
AL-15  01  WK-PACK-IND                 PIC X(01) VALUE SPACES.          
AL-15                                                                   
AL-15  01  WK-PLAT-GROSS-AMT           PIC S9(07)V99 VALUE +0.          
AL-15  01  WK-PLAT-BLEED-AMT           PIC S9(07)V99 VALUE +0.          
AL-15  01  WK-PLAT-INVC-AMT            PIC S9(07)V99 VALUE +0.          
AL-15  01  WK-PLAT-adj-AMT            PIC S9(07)V99 VALUE +0.          

AL-15  01  WK-PLAT-IND                 PIC X(01) VALUE SPACES.          
AL-15  01  PLAT-TABLE-COUNT            PIC 9(03) VALUE 0.               
AL-15  01  WK-JOB-PLAT                 PIC 9(13).                       
AL-15  01  WK-JOB-PLAT-RD REDEFINES WK-JOB-PLAT.                        
AL-15      05 WK-JOB-PLAT-7            PIC 9(10).                       
AL-15      05 WK-JOB-PLAT-2            PIC 9(03).                       
AL-15  01  PLAT-TABLE-MAX              PIC 9(02) VALUE 90.              
AL-15  01  PLAT-TABLE.                                                  
AL-15      05  PLAT-LINE OCCURS 99 INDEXED BY PLAT-X.                   
AL-15          10  PLAT-DATA.                                           
AL-15              15  PLAT-NBR         PIC 9(13).                      
AL-15              15  PLAT-NBR-RD REDEFINES PLAT-NBR.                  
AL-15                25  PLAT-NBR-7     PIC 9(10).                      
AL-15                25  PLAT-NBR-2     PIC 9(03).                      
AL-15              15  PLAT-GROSS-AMT   PIC S9(07)V99.                  
AL-15              15  PLAT-BLEED-AMT   PIC S9(07)V99.                  
AL-15              15  PLAT-INVC-AMT    PIC S9(07)V99.                  
AL-15              15  PLAT-adj-aMT    PIC S9(07)V99.                  
                                                                        
047400 01  WK-CREDIT-REPS.                                              
047500     05 WK-REP-1                 PIC X(02).                       
047600     05 WK-REP-2                 PIC X(02).                       
047700                                                                  
047800 01  WK-PAGE.                                                     
047900     05 WK-PAGE-PAGE.                                             
048000        10 WK-PAGE-PAGE-1        PIC X(01).                       
048100        10 FILLER                PIC X(02).                       
048200     05 FILLER                   PIC X(02).                       
048300     05 WK-PAGE-BOOK             PIC X(01).                       
048400  
       01  wk-pub-totals.
           05  wk-CLAS-amt              pic s9(08)v99.
           05  wk-CST-amt              pic s9(08)v99.
           05  wk-FSIN-amt              pic s9(08)v99.
           05  wk-MG-amt              pic s9(08)v99.
           05  wk-MGL-amt              pic s9(08)v99.
           05  wk-NCOM-amt              pic s9(08)v99.
           05  wk-NYT-amt              pic s9(08)v99.
           05  wk-NYTL-amt              pic s9(08)v99.
           05  wk-TA48-amt              pic s9(08)v99.
           05  wk-TMAG-amt              pic s9(08)v99.
           05  wk-TTR-amt              pic s9(08)v99.

           
048500*>   ---  Recap Totals                                            
048600 01  TTL-TTL-GROSS-AMT           PIC S9(12)V99 comp VALUE +0.     
048600 01  ttl-digital-gross-amt       PIC S9(12)V99 comp VALUE +0.     
048600 01  ttl-digital-net-amt         PIC S9(12)V99 comp VALUE +0.     

048700 01  TTL-TTL-GROSS-AMT-ADJ       PIC S9(12)V99 comp VALUE +0.     
048800 01  TTL-AGY-GROSS-AMT           PIC S9(12)V99 comp VALUE +0.     
048800 01  TTL-pub-GROSS-AMT           PIC S9(12)V99 comp VALUE +0.     
048800 01  TTL-pub-invc-amt            PIC S9(12)V99 comp VALUE +0.     

AL-20  01  TTL-AGY-NET-AMT-NT          PIC S9(12)V99 comp VALUE +0.     
048900 01  TTL-AGY-GROSS-AMT-ADJ       PIC S9(12)V99 comp VALUE +0.     
AL-20  01  TTL-AGY-NET-AMT-ADJ-NT      PIC S9(12)V99 comp VALUE +0.     
AL-20  01  WK-AGY-ADV-NET-NOTAX        PIC S9(12)V99 comp VALUE +0.     
AL-20  01  WK-AGY-ADV-GROSS-TOT        PIC S9(12)V99 comp VALUE +0.     
049000 01  TTL-TTL-DISC-AMT            PIC S9(12)V99 comp VALUE +0.     
049100 01  TTL-TTL-DISC-AMT-ADJ        PIC S9(12)V99 comp VALUE +0.     
049200 01  TTL-TTL-SPCH-AMT            PIC S9(12)V99 comp VALUE +0.     
049300 01  TTL-TTL-SPCH-AMT-ADJ        PIC S9(12)V99 comp VALUE +0.     
049400 01  TTL-TTL-NET-AMT             PIC S9(12)V99 comp VALUE +0.     
049500 01  TTL-TTL-NET-AMT-ADJ         PIC S9(12)V99 comp VALUE +0.     
049600 01  TTL-AGY-NET-AMT             PIC S9(12)V99 comp VALUE +0.     
049700 01  TTL-AGY-NET-AMT-ADJ         PIC S9(12)V99 comp VALUE +0.     
049800 01  TTL-IAR-10-AMT              PIC S9(12)V99 comp VALUE +0.     
049900 01  TTL-IAR-11-AMT              PIC S9(12)V99 comp VALUE +0.     
050000 01  TTL-PAGE-CTR                PIC 9(09)     comp VALUE  0.     
050100 01  TTL-INVC-NBR                PIC 9(09)     comp VALUE  0.     
050200 01  TTL-WO-NBR                  PIC 9(18)     comp VALUE  0.     
050300 01  TTL-ACCT-NBR                PIC 9(09)     comp VALUE  0.     
050400 01  TTL-UNIT-AMT                PIC S9(12)V99 comp VALUE +0.     
050500 01  TTL-UNIT-AMT-LINE           PIC S9(12)V99 comp VALUE +0.     
050600 01  TTL-UNIT-AMT-INCH           PIC S9(12)V99 comp VALUE +0.     
050700 01  TTL-IAR-DETAIL              PIC 9(09)     comp VALUE  0.     
050800 01  TTL-IAR-TOTAL               PIC 9(09)     comp VALUE  0.     
050900 01  TTL-OLD-TOTAL               PIC 9(09)     comp VALUE  0.     
051000                                                                  
051100*>   ---  Invoics Totals                                          
051200 01  TTL-GROSS-AMT               PIC S9(09)V99 comp VALUE +0.     
051300 01  TTL-GROSS-AMT-ADJ           PIC S9(09)V99 comp VALUE +0.     
AL-20  01  TTL-NET-AMT-ADJ-NT          PIC S9(09)V99 comp VALUE +0.     
AL-20  01  TTL-NET-AMT-NT              PIC S9(09)V99 comp VALUE +0.     
051400 01  TTL-DISC-AMT                PIC S9(09)V99 comp VALUE +0.     
051500 01  TTL-DISC-AMT-ADJ            PIC S9(09)V99 comp VALUE +0.     
051600 01  TTL-SPCH-AMT                PIC S9(09)V99 comp VALUE +0.     
051700 01  TTL-SPCH-AMT-ADJ            PIC S9(09)V99 comp VALUE +0.     
051800 01  TTL-NET-AMT                 PIC S9(09)V99 comp VALUE +0.     
051900 01  TTL-NET-AMT-ADJ             PIC S9(09)V99 comp VALUE +0.     
052000                                                                  
052100*>   ---  Detail Totals                                           
052200 01  DTL-GROSS-AMT               PIC S9(09)V99 comp VALUE +0.     
052300 01  DTL-NET-AMT                 PIC S9(09)V99 comp VALUE +0.     
AL-20  01  DTL-NET-SPACE               PIC S9(09)V99 comp VALUE +0.     
AL-20  01  DTL-NET-NOTAX               PIC S9(09)V99 comp VALUE +0.     
DW-20  01  WK-MON-ISSUE                PIC 9(08) VALUE 20070611.                                                                      
052500 01  WK-DISCOUNTS                PIC S9(16)V99 comp VALUE +0.     
052600                                                                  
AL-17  01  WK-LEVEL-DISC               PIC X(01) VALUE SPACES.          
AL-17                                                                   
052700 01  WK-AD-SIZE                  PIC X(04).                       
052800 01  WK-AD-SIZE-RE               REDEFINES WK-AD-SIZE.            
052900     05 WK-AD-SIZE-X        OCCURS 4 TIMES                        
053100            INDEXED BY WK-SIZE-X PIC X(01).                       
053200                                                                  
053300 01  WK-AD-QUANTITY              PIC 9(16)V99.                    
053400 01  WK-AD-QUANTITY-RE           REDEFINES WK-AD-QUANTITY.        
053500     05  FILLER                  PIC 9(10).                       
053600     05  WK-AD-QUANTITY-2        PIC 9(06).                       
053500     05  FILLER                  PIC 9(02).                       
053900*>   ---  Temporary Fields                                        
054000 01  WK-TEMPS.                                                    
054100     05  WK-ADDRESS.                                              
054200         10  WK-CITY             PIC X(20).                       
054300         10  WK-CNTRY.                                            
054400             15 WK-STATE         PIC X(03).                       
054500             15 FILLER           PIC X(02).                       
054600             15 WK-ZIP.                                           
054700                20 WK-ZIP-5      PIC X(05).                       
054800                20 WK-ZIP-4      PIC X(04).                       
054900             15 FILLER           PIC X(01).                       
055000     05  WK-ZIP-2.                                                
055100         10 WK-ZIP-2-5           PIC X(05).                       
055200         10 WK-ZIP-2-4           PIC X(04).                       
055300     05  WK-SOURCE.                                               
055400         10  WK-SOURCE-1         PIC X(01).                       
055500         10  WK-SOURCE-2         PIC X(02).                       
055600         10  FILLER              PIC X(01).                       
056000     05  WK-DIMENSIONS-RE.                                        
056100         10 WK-DIMENSIONS-1      PIC Z(03)9.                      
056200         10 WK-DIMENSIONS-2      PIC X(03).                       
056300         10 WK-DIMENSIONS-3      PIC X(04).                       
056300         10 WK-DIMENSIONS-5      PIC X(07).                       
055700     05  WK-DIMENSIONS.                                           
055800         10  WK-DIM-1            PIC X(08).                       
055900         10  WK-DIM-2            PIC X(08).                       
056400     05  WK-DIMENSIONS-RE-2      REDEFINES WK-DIMENSIONS.         
056500         10 WK-AD-DIM-1          PIC X(04).                       
056600         10 WK-AD-DIM-X          PIC X(03).                       
056700         10 WK-AD-DIM-2          PIC X(03).                       
056800     05  WK-DIMENSIONS-RE-3      REDEFINES WK-DIMENSIONS.         
056900         10 WK-FSI-DIM-1         PIC Z(06).                       
057000         10 WK-FSI-DIM-X         PIC X(03).                       
057100         10 WK-FSI-DIM-2         PIC X(04).                       
057200     05  WK-DIMENSIONS-RE-4      REDEFINES WK-DIMENSIONS.         
057300         10 FILLER               PIC X(04).                       
057400         10 WK-DIMENSIONS-4      PIC X(12).                       
057500     05  WK-CLS-DIMENSIONS       REDEFINES WK-DIMENSIONS.         
057600         10 WK-CLS-DIM-1         PIC Z(02)9.                      
057700         10 WK-CLS-DIM-TIMES     PIC X(03).                       
057800         10 WK-CLS-DIM-2-X       OCCURS 3                         
057900                                 INDEXED WK-DIM-X                 
058000                                 PIC X(01).                       
058100     05  WK-TEMP-9               PIC 9(04).                       
058200     05  WK-TEMP-X               REDEFINES WK-TEMP-9              
058300                                 PIC X(04).         
           05  test-nbr pic 9(18) value 105067001.                      
058400     05  WK-TEMP-LINEAGE-I       PIC 9(07)    VALUE 0.            
058500     05  WK-TEMP-LINEAGE-D       PIC 9(05)V99 VALUE 0.            
058600     05  WK-TEMP-TEL-NBR.                                         
058700         10  WK-TEMP-TEL-1       PIC X(03).                       
058800         10  WK-TEMP-TEL-2       PIC X(03).                       
058900         10  WK-TEMP-TEL-3       PIC X(04).                       
059000     05  WK-TEMP-RATE            PIC S9(06)V99 VALUE +0.          
059100     05  WK-ACTT-CODE            PIC X(01)     VALUE SPACE.       
059200     05  WK-MT-CNT-END           PIC 9(08).                       
059300     05  WK-MT-CNT-END-RE        REDEFINES WK-MT-CNT-END.         
059400         10 FILLER               PIC X(02).                       
059500         10 WK-MT-CNT-END-6.                                      
059600            15 WK-MT-CNT-END-YY  PIC X(02).                       
059700            15 WK-MT-CNT-END-MM  PIC X(02).                       
059800            15 WK-MT-CNT-END-DD  PIC X(02).                       
059900     05  WK-PRODUCE-TAPE         PIC X(01) VALUE 'N'.             
089700* CTS-02/23/07 CHANGES BEGINS                                     
059200     05  WK-JXRF-KEY             PIC X(12).                       
059300     05  WK-ADV-KEY.                                              
059400         10  WK-ADV-FILLER       PIC X(01).                       
059500         10  WK-ADV-KEY-1        PIC X(12).                       
059600         10  FILLER              PIC X(07).                       
059700     05  WK-AGY-KEY.                                              
059200         10  WK-AGY-FILLER       PIC X(01).                       
059300         10  WK-AGY-KEY-1        PIC X(12).                       
059400         10  FILLER              PIC X(07).                       
059500     05  WK-HOLD-ADV-NBR         PIC X(12).                       
059600     05  WK-HOLD-AGY-NBR         PIC X(12).                       
089700* CTS-02/23/07 CHANGES ENDS                                       
060000     05  WK-JXRF-AREA            PIC X(20).                       
060100     05  WK-ADV-JXRF-AREA.                                        
060200         10  FILLER              PIC X(01).                       
060300         10  WK-ADV-JXRF.                                         
060400             15  FILLER          PIC X(01).                       
060500             15  WK-ADV-JXRF-2   PIC X(05).                       
060600         10  WK-ADV-JXRF-RE      REDEFINES WK-ADV-JXRF.           
060700             15  WK-ADV-JXRF-4-1 PIC X(04).                       
060800             15  WK-ADV-JXRF-2-2 PIC X(02).                       
060900         10  FILLER              PIC X(10).                       
061000     05  WK-AGY-JXRF-AREA.                                        
061100         10  FILLER              PIC X(01).                       
061200         10  WK-AGY-JXRF.                                         
061300             15  FILLER          PIC X(01).                       
061400             15  WK-AGY-JXRF-2   PIC X(05).                       
061500         10  WK-AGY-JXRF-RE      REDEFINES WK-AGY-JXRF.           
061600             15  WK-AGY-JXRF-4-1 PIC X(04).                       
061700             15  WK-AGY-JXRF-2-2 PIC X(02).                       
061800         10  FILLER              PIC X(10).                       
061900***  05  WK-J-XREF-AREA.                                          
062000***      10  FILLER              PIC X(01).                       
062100***      10  WK-J-XREF.                                           
062200***          15  FILLER          PIC X(01).                       
062300***          15  WK-J-XREF-2     PIC X(05).                       
062400     05  WK-ADV-NBR              PIC 9(09)    comp VALUE 0.       
062500     05  WK-DISPLAY              PIC Z(09).       

062600     05  WK-PFM-IND              PIC X(01).                       
062700     05  WK-INDX                 PIC 9(04)    comp.               
062800     05  WK-PFM-TMP              PIC 9(09)V99 comp.               
062900     05  WK-PFM-TMP-2            PIC 9(09)    comp.               
063000     05  WK-PFM-CUT-OFF          PIC 9(08)    comp.               
063100     05  WK-PFM-WO-ISSUE         PIC 9(08)    comp VALUE 0.       
063200     05  WK-PFM-AREA             OCCURS 2 TIMES.                  
063300         10  WK-PFM-ISS-SUN      PIC 9(09)    comp.               
063400         10  WK-PFM-ISS-WEK      PIC 9(09)    comp.               
063500         10  WK-PFM-TOT-SUN      PIC 9(09)    comp.               
063600         10  WK-PFM-TOT-WEK      PIC 9(09)    comp.               
063700         10  WK-PFM-LIN-SUN      PIC 9(09)    comp.               
063800         10  WK-PFM-LIN-WEK      PIC 9(09)    comp.               
063900         10  WK-PFM-INC-SUN      PIC 9(09)V99 comp.               
064000         10  WK-PFM-INC-WEK      PIC 9(09)V99 comp.               
064100     05  WK-MONTH-DATES.                                          
064200         10  WK-MO-FROM          PIC 9(08) VALUE 0.               
064300         10  WK-MO-TO            PIC 9(08) VALUE 0.               
064400         10  WK-MO-TO-RE         REDEFINES WK-MO-TO.              
064500             15  FILLER          PIC X(06).                       
064600             15  WK-MO-TO-DD     PIC 9(02).                       
064700     05  WK-PFM-DATE             PIC 9(08) comp VALUE 0.          
064800     05  WK-CNT-LEVEL            PIC 9(08).                       
064900     05  WK-CNT-LVL              REDEFINES WK-CNT-LEVEL           
065000                                 PIC X(08).                       
065100     05  WK-CNT-LVL-RE           REDEFINES WK-CNT-LEVEL.          
065200         10  WK-CNT-LVL-1        PIC X(04).                       
065300         10  WK-CNT-LVL-2        PIC X(04).                       
065400     05  WK-DATE.                                                 
065500         10  WK-MM1               PIC X(02).                      
065600         10  WK-DD1               PIC X(02).                      
065700         10  WK-YY1               PIC X(02).                      
065800     05  WK-BATCH-NBR.                                            
065900         10  WK-BATCH-NBR-9      PIC 9(03) VALUE 1.               
066000     05  WK-INVC-DATE-P          PIC 9(08) comp VALUE 0.          
066000     05  WK-INVC-DATE-F          PIC X(08)        VALUE SPACE.    
066000     05  WK-INVC-DATE-T          PIC X(08)        VALUE SPACE.
AL-01A     05  WK-INVC-RUN             PIC X(01)        VALUE SPACES.  
AL-01A     05  WK-INVC-ORG             PIC X(03)        VALUE SPACES.  
AL-01A     05  WK-NT-REC-CNT           PIC 9(08) VALUE 0.  
AL-01A     05  WK-NT-inv-CNT           PIC 9(08) VALUE 0.  

AL-01A     05 wk-invc-nbr-18           pic 9(18).
AL-01A     05 wk-invc-nbr-18-rd redefines wk-invc-nbr-18.
AL-01A        08 wk-invc-nbr-filler    pic 9(6).
AL-01A        08 wk-invc-nbr-7-18      pic 9(12).        
066100     05  WK-COMP-DATE.                                            
066200         10  WK-COMP-HOUR        PIC X(08).                       
066300         10  WK-COMP-DAY         PIC X(12).                       
066400     05  WK-SEP-TEXT.                                             
066500         10  FILLER              PIC X(04) VALUE SPACES.          
066600         10  WK-SEP-LIT          PIC X(15) VALUE SPACES.          
066700     05  WK-DOCUMENT             PIC X(09) VALUE SPACES.          
066800     05  WK-CB                   PIC X(01) VALUE SPACES.          
066900     05  WK-FONT                 PIC X(01) VALUE SPACES.          
067000     05  WK-SAVE-MAJOR           PIC X(02) VALUE SPACES.          
067100     05  WK-SAVE-MINOR           PIC X(04) VALUE SPACES.          
067200     05  WK-SAVE-FLAG            PIC X(01) VALUE 'Y'.             
067300     05  WK-ROUTE                PIC X(01) VALUE SPACE.           
067400     05  WK-REF-FIELD.                                            
067500         10  WK-REF-NAME         PIC X(10) VALUE SPACES.          
067600         10  FILLER              PIC X(01) VALUE SPACES.          
067700         10  WK-REF-ACCT         PIC X(05) VALUE SPACES.          
067800     05  WK-CNT-COUNT            PIC S9(9) comp VALUE +0.         
067900                                                                  
068000*>   ---  Account currently being processed                       
068100 01  SAVE-INFO.                                                   
068200     05  SAVE-TYPE               PIC X(02) VALUE SPACES.          
068200     05  SAVE-pub                PIC X(04) VALUE SPACES.          

068300     05  SAVE-AGY-ACCT-NBR       PIC 9(09) comp VALUE 0.          
068400     05  SAVE-ADV-PAR-NBR        PIC 9(09) comp VALUE 0.          
068500     05  SAVE-ACCT-NBR           PIC 9(09) comp VALUE 0.          
068600     05  SAVE-CNT-KEY.                                            
068700         10  SAVE-CNT-ACCT-NBR   PIC 9(09) comp.                  
068800         10  SAVE-CNT-PUB        PIC X(04).                       
068900         10  SAVE-CNT-REF-NBR    PIC 9(08) comp.                  
069000     05  SAVE-CATEGORY           PIC X(01) VALUE SPACE.           
069100     05  SAVE-NAT-AGY            PIC X(01) VALUE SPACE.           
AL-06      05  SAVE-GROUP-CODE         PIC X(06) VALUE SPACE.           09981000
AL-20      05  SAVE-INVNBR             PIC 9(09).                       09982001
AL-20      05  SAVE-INVNBR-RD REDEFINES SAVE-INVNBR.                    09983001
AL-20          08 SAVE-INVNBR-6        PIC 9(06).                       09984001
AL-20          08 SAVE-INVNBR-3        PIC 9(03).                       09985001
AL-20  01 WK-INV-NBR                     PIC 9(09).                     03930101
AL-20  01 WK-INV-NBR-RD REDEFINES WK-INV-NBR.                           03930201
AL-20     05 WK-INV-NBR-6                PIC X(06).                     03930301
AL-20     05 WK-INV-NBR-3                PIC X(03).                     03930401
069200                                                                  
069300*>   ---  Owners of Advertiser being billed                       
069400 01  PARENT-TABLE.                                                
069500     05 PARENT-ENTRY             OCCURS 2 INDEXED BY PR-X.        
069600        10 PARENT-NAME           PIC X(28).                       
069700        10 PARENT-TYPE           PIC X(02).                       
069800        10 PARENT-ACCT-KEY       PIC X(15).                       
069900        10 PARENT-ACCT-NBR       PIC 9(09) comp.                  
070000                                                                  
070100*>   ---  Free form invoice messages                              
070200 01  WK-INVC-MSGS.                                                
070300     05 WK-INVC-MSG              OCCURS 5 INDEXED BY WK-M         
070400                                 PIC X(75).                       
070500 01  WK-MSG-CTR                  PIC 9(04) comp VALUE 0.          
070600 01  WK-MSG-KEY.                                                  
070700     05 WK-MSG-PRIM.                                              
070800        10 WK-MSG-KEY1           PIC X(01).                       
070900        10 WK-MSG-KEY2.                                           
071000           15 FILLER             PIC X(01).                       
071100           15 WK-MSG-KEY2-2      PIC X(01).                       
071200     05 WK-MSG-KEY3              PIC X(01).                       
071300                                                                  
071400*>   ---  Adv/Agy group type (NAD-TYPE)                           
071500 01  WK-INVC-TYPE                PIC 9(01) VALUE 0.               
071600                                                                  
071700*>   ---  Invoice Number                                          
071800 01  WK-INVC-NBR                 PIC 9(13) comp VALUE 0.          
071900 01  WK-INVC-COUNT               PIC 9(09) comp VALUE 0.          
072000                                                                  
072100*>   ---  Temp area for CNT-PFM footer.                           
072200 01  WK-TEMP-CTR                 PIC 9(04) comp VALUE 0.          
072300                                                                  
072400*>   ---  Lines printed on current invoice page.                  
072500 01  WK-LINE-CTR                 PIC 9(04) comp VALUE 0.          
072600                                                                  
072700*>   ---  Page number within current invoice (not overall).       
072800 01  WK-PAGE-CTR                 PIC 9(04) comp VALUE 0.          
072900                                                                  
073000*>   ---  Maximum number of details in the body of Invc.          
073100 01  WK-PAGE-DETAILS             PIC 9(04) comp VALUE 39.         
073200                                                                  
073300*>   ---  Maximum number of lines per page (including header)     
073400 01  WK-PAGE-MAX                 PIC 9(04) comp VALUE 60.         
073600/                                                                 
073700 01  HDR-LINES.                                                   
073800                                                                  
073900     05 HDR-LINE-1               PIC X(150).                      
074000     05 HDR-LINE-1-RE            REDEFINES HDR-LINE-1.            
074100        10 HDR-1-CB              PIC X(01).                       
074200        10 HDR-1-FONT            PIC X(01).                       
074300        10 HDR-1-ROUTE           PIC X(01).                       
074400        10 HDR-1-PAGE            PIC X(01).                       
074500        10 FILLER                PIC X(40).                       
074600*...(1) Billing Period                                            
074700        10 HDR-ITEM-1            PIC X(24).                       
074800        10 HDR-ITEM-1-X          REDEFINES HDR-ITEM-1.            
074900*...(1) Billing Period                                            
075000           15 FILLER             PIC X(02).                       
075100           15 HDR-BILLING-FROM   PIC X(08).                       
075200           15 HDR-THRU           PIC X(03).                       
075300           15 HDR-BILLING-TO     PIC X(08).                       
075400           15 FILLER             PIC X(02).                       
075500*...(2) Advertiser Name                                           
075600        10 HDR-ITEM-2            PIC X(30).                       
075700        10 HDR-ITEM-2-X          REDEFINES HDR-ITEM-2.            
075800*...(2) Advertiser Name                                           
075900           15 FILLER             PIC X(01).                       
076000           15 HDR-ADV-NAME       PIC X(28).                       
076100           15 FILLER             PIC X(01).                       
076200                                                                  
076300     05 HDR-LINE-2               PIC X(150).                      
076400     05 HDR-LINE-2-RE            REDEFINES HDR-LINE-2.            
076500        10 HDR-2-CB              PIC X(01).                       
076600        10 HDR-2-FONT            PIC X(01).                       
076700        10 HDR-2-ROUTE           PIC X(01).                       
076800        10 HDR-2-PAGE            PIC X(01).                       
076900        10 FILLER                PIC X(42).                       
077000*...(3) Terms                                                     
077100        10 HDR-ITEM-3            PIC X(24).                       
077200        10 HDR-ITEM-3-X          REDEFINES HDR-ITEM-3.            
077300           15 HDR-TERMS-LIN1     PIC X(24).                       
077400*...(4) Page Number                                               
077500        10 HDR-ITEM-4            PIC X(08).                       
077600        10 HDR-ITEM-4-X          REDEFINES HDR-ITEM-4.            
077700           15 FILLER             PIC X(03).                       
077800           15 HDR-PAGE-NBR       PIC Z(03)9.                      
077900           15 FILLER             PIC X(01).                       
078000*...(6) Billed Account Nbr                                        
078100        10 HDR-ITEM-6            PIC X(24).                       
078200        10 HDR-ITEM-6-X          REDEFINES HDR-ITEM-6.            
078300           15 FILLER             PIC X(05).                       
078400*...(6) Billed Account Nbr                                        
078900           15 HDR-AGY-NBR-X.                                      
079000              20 FILLER          PIC X(01).                       
079100              20 HDR-AGY-NBR-8   PIC X(08).                       
079200              20 FILLER          PIC X(04).                       
079300                                                                  
079400     05 HDR-LINE-3               PIC X(150).                      
079500     05 HDR-LINE-3-RE            REDEFINES HDR-LINE-3.            
079600        10 HDR-3-CB              PIC X(01).                       
079700        10 HDR-3-FONT            PIC X(01).                       
079800        10 HDR-3-ROUTE           PIC X(01).                       
079900        10 HDR-3-PAGE            PIC X(01).                       
080000*...(7) Advertiser Number                                         
080100        10 FILLER                PIC X(46).                       
080200        10 HDR-ITEM-7            PIC X(19).                       
080300        10 HDR-ITEM-7-X          REDEFINES HDR-ITEM-7.            
080400*...(7) Advertiser Number                                         
080500           15 FILLER             PIC X(01).                       
080600           15 HDR-ADV-NBR-X      PIC X(09).                       
080700*...(5) Billing Date                                              
080800        10 HDR-ITEM-5            PIC X(12).                       
080900        10 HDR-ITEM-5-X          REDEFINES HDR-ITEM-5.            
081000           15 FILLER             PIC X(02).                       
081100*...(5) Billing Date                                              
081200           15 HDR-BILLING-DATE.                                   
081300              20 HDR-BILL-MM     PIC X(02).                       
081400              20 HDR-BILL-SL1    PIC X(01).                       
081500              20 HDR-BILL-DD     PIC X(02).                       
081600              20 HDR-BILL-SL2    PIC X(01).                       
081700              20 HDR-BILL-YY     PIC X(02).                       
081800           15 FILLER             PIC X(02).                       
081900*...(23) Total Amount Due                                         
082000        10 HDR-ITEM-23           PIC X(20).                       
082100        10 HDR-ITEM-23-X         REDEFINES HDR-ITEM-23.           
082200*...(23) Total Amount Due                                         
082300           15 HDR-AMOUNT         PIC ZZZ,ZZZ,ZZ9.99CR.            
082400           15 HDR-AMOUNT-X       REDEFINES HDR-AMOUNT             
082500                                 PIC X(16).                       
082600           15 FILLER             PIC X(04).                       
082700                                                                  
082800     05 HDR-LINE-4               PIC X(150).                      
082900     05 HDR-LINE-4-RE            REDEFINES HDR-LINE-4.            
083000        10 HDR-4-CB              PIC X(01).                       
083100        10 HDR-4-FONT            PIC X(01).                       
083200        10 HDR-4-ROUTE           PIC X(01).                       
083300        10 HDR-4-PAGE            PIC X(01).                       
083400        10 FILLER                PIC X(17).                       
083500        10 HDR-ITEM-8-1          PIC X(43).                       
083600        10 HDR-ITEM-8-1-X        REDEFINES HDR-ITEM-8-1.          
083700*...(8) Billed Account Address                                    
083800           15 HDR-AGY-NAM1       PIC X(30).                       
083900*...(9) Remittance Address                                        
084000        10 HDR-ITEM-9-1          PIC X(34).                       
084100        10 HDR-ITEM-9-1-X        REDEFINES HDR-ITEM-9-1.          
084200*...(9) Remittance Address                                        
084300           15 HDR-NAM1           PIC X(30).                       
084400                                                                  
084500     05 HDR-LINE-5               PIC X(150).                      
084600     05 HDR-LINE-5-RE            REDEFINES HDR-LINE-5.            
084700        10 HDR-5-CB              PIC X(01).                       
084800        10 HDR-5-FONT            PIC X(01).                       
084900        10 HDR-5-ROUTE           PIC X(01).                       
085000        10 HDR-5-PAGE            PIC X(01).                       
085100        10 FILLER                PIC X(17).                       
085200        10 HDR-ITEM-8-2          PIC X(43).                       
085300        10 HDR-ITEM-8-2-X        REDEFINES HDR-ITEM-8-2.          
085400           15 HDR-AGY-NAM2       PIC X(30).                       
085500        10 HDR-ITEM-9-2          PIC X(34).                       
085600        10 HDR-ITEM-9-2-X        REDEFINES HDR-ITEM-9-2.          
085700            15 HDR-NAM2          PIC X(30).                       
085800                                                                  
085900                                                                  
086000     05 HDR-LINE-6               PIC X(150).                      
086100     05 HDR-LINE-6-RE            REDEFINES HDR-LINE-6.            
086200        10 HDR-6-CB              PIC X(01).                       
086300        10 HDR-6-FONT            PIC X(01).                       
086400        10 HDR-6-ROUTE           PIC X(01).                       
086500        10 HDR-6-PAGE            PIC X(01).                       
086600        10 FILLER                PIC X(17).                       
086700        10 HDR-ITEM-8-3          PIC X(43).                       
086800        10 HDR-ITEM-8-3-X        REDEFINES HDR-ITEM-8-3.          
FXC---           15 HDR-AGY-STREET     PIC X(35).                       
086900*>         15 HDR-AGY-STREET     PIC X(25).                       
087000        10 HDR-ITEM-9-3          PIC X(35).                       
087100        10 HDR-ITEM-9-3-X        REDEFINES HDR-ITEM-9-3.          
087200           15 HDR-STREET         PIC X(25).                       
087300                                                                  
087400                                                                  
087500     05 HDR-LINE-7               PIC X(150).                      
087600     05 HDR-LINE-7-RE            REDEFINES HDR-LINE-7.            
087700        10 HDR-7-CB              PIC X(01).                       
087800        10 HDR-7-FONT            PIC X(01).                       
087900        10 HDR-7-ROUTE           PIC X(01).                       
088000        10 HDR-7-PAGE            PIC X(01).                       
088100        10 FILLER                PIC X(17).                       
088200        10 HDR-ITEM-8-4          PIC X(43).                       
088300        10 HDR-ITEM-8-4-X        REDEFINES HDR-ITEM-8-4.          
088400           15 HDR-AGY-CITY       PIC X(17).                       
088500           15 FILLER             PIC X(01).                       
088600           15 HDR-AGY-CNTRY.                                      
088700              20 HDR-AGY-STATE   PIC X(02).                       
088800              20 FILLER          PIC X(01).                       
088900              20 HDR-AGY-ZIP.                                     
089000                 25 HDR-AGY-ZIP-5  PIC X(05).                     
089100                 25 HDR-AGY-ZIP-SL PIC X(01).                     
089200                 25 HDR-AGY-ZIP-4  PIC X(04).                     
089300              20 FILLER          PIC X(04).                       
089400        10 HDR-ITEM-9-4          PIC X(35).                       
089500        10 HDR-ITEM-9-4-X        REDEFINES HDR-ITEM-9-4.          
089600           15 HDR-CITY           PIC X(11).                       
089700           15 FILLER             PIC X(01).                       
089800           15 HDR-STATE          PIC X(03).                       
089900           15 FILLER             PIC X(01).                       
090000           15 HDR-ZIP.                                            
090100              20 HDR-ZIP-5       PIC X(05).                       
090200              20 HDR-ZIP-SL      PIC X(01).                       
090300              20 HDR-ZIP-4       PIC X(04).                       
089700* CTS-02/23/07 CHANGES BEGINS                                     
086000     05 HDR-LINE-7-A             PIC X(150).                      
087600     05 HDR-LINE-7-A-RE          REDEFINES HDR-LINE-7-A.          
087700        10 HDR-7-A-CB            PIC X(01).                       
087800        10 HDR-7-A-FONT          PIC X(01).                       
087900        10 HDR-7-A-ROUTE         PIC X(01).                       
088000        10 HDR-7-A-PAGE          PIC X(01).                       
088100        10 FILLER                PIC X(17).                       
089800        10 HDR-CNTRY-LIT         PIC X(10).                       
089800        10 HDR-CNTRY             PIC X(15).                       
089700* CTS-02/23/07 CHANGES ENDS                                       
090400                                                                  
090500     05 HDR-LINE-8               PIC X(150).                      
090600     05 HDR-LINE-8-RE            REDEFINES HDR-LINE-8.            
090700        10 HDR-8-CB              PIC X(01).                       
090800        10 HDR-8-FONT            PIC X(01).                       
090900        10 HDR-8-ROUTE           PIC X(01).                       
091000        10 HDR-8-PAGE            PIC X(01).                       
091100        10 FILLER                PIC X(05).                       
091100        10 HDR-AGY-NBR-9         PIC 9(09) BLANK WHEN ZERO.       
092400                                                                  
090500     05 HDR-LINE-9               PIC X(150).                      
090600     05 HDR-LINE-9-RE            REDEFINES HDR-LINE-9.            
090700        10 HDR-9-CB              PIC X(01).                       
090800        10 HDR-9-FONT            PIC X(01).                       
090900        10 HDR-9-ROUTE           PIC X(01).                       
091000        10 HDR-9-PAGE            PIC X(01).                       
091100        10 FILLER                PIC X(05).                       
091100        10 HDR-ADV-NBR-9         PIC 9(09) BLANK WHEN ZERO.       
091100        10 FILLER                PIC X(26).                       
091200*...(X) Further info Call                                         
al-02 ***     10 HDR-REP-NAME          PIC X(20).                       
al-02         10 HDR-REP-NAME          PIC X(30).                       
091400        10 FILLER                PIC X(05).                       
091500        10 HDR-TEL-NBR.                                           
091600           15 HDR-TEL-A          PIC X(01).                       
091700           15 HDR-TEL-1          PIC X(03).                       
091800           15 HDR-TEL-B          PIC X(01).                       
091900           15 FILLER             PIC X(01).                       
092000           15 HDR-TEL-2          PIC X(03).                       
092100           15 HDR-TEL-C          PIC X(01).                       
092200           15 HDR-TEL-3          PIC X(04).                       
                                                                        
092500 01  FOOTER-LINES.                                                
092600     05 FOOT-LINE-1              PIC X(150).                      
092700     05 FOOT-LINE-1-RE           REDEFINES FOOT-LINE-1.           
092800        10 FOOT-1-CB             PIC X(01).                       
092900        10 FOOT-1-FONT           PIC X(01).                       
093000        10 FOOT-1-ROUTE          PIC X(01).                       
093100        10 FOOT-1-PAGE           PIC X(01).                       
093200        10 FOOT-ITEM-21          PIC X(17).                       
093300        10 FOOT-ITEM-21-X        REDEFINES FOOT-ITEM-21.          
093400           15 FILLER             PIC X(01).                       
093500           15 FOOT-TOT-CURR-AMT  PIC ZZZ,ZZZ,ZZ9.99CR.            
093600           15 FOOT-TOT-CURR-AMT-X REDEFINES FOOT-TOT-CURR-AMT     
093700                                 PIC X(16).                       
093800        10 FILLER                PIC X(56).                       
093900        10 FOOT-ITEM-23          PIC X(22).                       
094000        10 FOOT-ITEM-23-X        REDEFINES FOOT-ITEM-23.          
094100           15 FILLER             PIC X(06).                       
094200           15 FOOT-TOT-AMT       PIC ZZZ,ZZZ,ZZ9.99CR.            
094300           15 FOOT-TOT-AMT-X     REDEFINES FOOT-TOT-AMT           
094400                                 PIC X(16).                       
AL-06A        10 FILLER                PIC X(02).                       
AL-06A        10 FOOT-IGNORE           PIC X(12).                       
094500                                                                  
094600     05 FOOT-LINE-2              PIC X(150).                      
094700     05 FOOT-LINE-2-RE           REDEFINES FOOT-LINE-2.           
094800        10 FOOT-2-CB             PIC X(01).                       
094900        10 FOOT-2-FONT           PIC X(01).                       
095000        10 FOOT-2-ROUTE          PIC X(01).                       
095100        10 FOOT-2-PAGE           PIC X(01).                       
095200        10 FOOT-ITEM-24          PIC X(11).                       
095300        10 FOOT-ITEM-24-X        REDEFINES FOOT-ITEM-24.          
095400           15 FILLER             PIC X(02).                       
095500           15 FOOT-DOCUMENT.                                      
095600              20 FOOT-DOCUMENT-1 PIC 9(01).                       
095700              20 FOOT-DOCUMENT-2 PIC 9(02).                       
095800              20 FOOT-DOCUMENT-3 PIC 9(04).                       
095500           15 FOOT-DOCUMENT-X    REDEFINES FOOT-DOCUMENT.         
095800              20 FOOT-DOCUMENT-6 PIC 9(06).                       
095600              20 FILLER          PIC X(01).                       
095900           15 FILLER             PIC X(02).                       
096000        10 FOOT-ITEM-25-1        PIC X(24).                       
096100        10 FOOT-ITEM-25-1-X      REDEFINES FOOT-ITEM-25-1.        
096200           15 FILLER             PIC X(02).                       
096300           15 FOOT-BILLING-FROM  PIC X(08).                       
096400           15 FOOT-THRU          PIC X(03).                       
096500           15 FOOT-BILLING-TO    PIC X(08).                       
096600           15 FILLER             PIC X(03).                       
096700        10 FOOT-ITEM-25-6        PIC X(20).                       
096800        10 FOOT-ITEM-25-6-X      REDEFINES FOOT-ITEM-25-6.        
096900           15 FILLER             PIC X(08).                       
097000           15 FOOT-ACCT-NBR.                                      
097500              20 FOOT-AGY-NBR-X.                                  
097600                 25 FILLER       PIC X(01).                       
097700                 25 FOOT-AGY-NBR-8 PIC X(08).                     
097800                 25 FILLER       PIC X(01).                       
097900        10 FOOT-ITEM-25-7        PIC X(09).                       
098000        10 FOOT-ITEM-25-7-X      REDEFINES FOOT-ITEM-25-7.        
098100           15 FILLER             PIC X(02).                       
098200           15 FOOT-ADV-NBR-X     PIC X(06).                       
098300           15 FILLER             PIC X(01).                       
098400        10 FOOT-ITEM-25-2        PIC X(38).                       
098500        10 FOOT-ITEM-25-2-X      REDEFINES FOOT-ITEM-25-2.        
098600           15 FILLER             PIC X(01).                       
098700           15 FOOT-ADV-NAME      PIC X(30).                       
098800                                                                  
099000 01  DTL-LINES.                                                   
099100     05 DTL-LINE-SAVE            PIC X(150).                      
099200                                                                  
099300     05 DTL-LINE                 PIC X(150).                      
099400     05 DTL-LINE-A               REDEFINES DTL-LINE.              
099500        10 DTL-1-CB              PIC X(01).                       
099600        10 DTL-1-FONT            PIC X(01).                       
099700        10 DTL-1-ROUTE           PIC X(01).                       
099800        10 DTL-1-PAGE            PIC X(01).                       
099900        10 DTL-ITEM-10           PIC X(10).                       
100000        10 DTL-ITEM-10-X         REDEFINES DTL-ITEM-10.           
100100           15 DTL-SUNDAY         PIC X(01).                       
100200           15 DTL-ISSUE          PIC X(08).                       
100300        10 DTL-ITEM-11           PIC X(10).                       
100400        10 DTL-ITEM-11-X         REDEFINES DTL-ITEM-11.           
100500           15 DTL-JOB-NBR        PIC 9(09).                       
100600           15 DTL-REF-NBR-9      REDEFINES DTL-JOB-NBR            
100700                                 PIC X(09).                       
100600           15 DTL-REF-NBR-X      REDEFINES DTL-JOB-NBR.           
100600              20 DTL-REF-NBR-6   PIC X(06).                       
100600              20 FILLER          PIC X(03).                       
100800        10 DTL-ITEM-12-13-14     PIC X(38).                       
100900        10 DTL-ITEM-12-13-14-X   REDEFINES DTL-ITEM-12-13-14.     
101000           15 DTL-DESCRIPTION    PIC X(38).                       
101100           15 DTL-DESC           REDEFINES DTL-DESCRIPTION.       
101200              20 DTL-PUB         PIC X(04).                       
101300              20 FILLER          PIC X(01).                       
101400              20 DTL-EDITION     PIC X(04).                       
101500              20 FILLER          PIC X(01).                       
101600              20 DTL-AD-TYPE     PIC X(04).                       
101700              20 FILLER          PIC X(01).                       
101800              20 DTL-COLOR       PIC X(02).                       
101900              20 FILLER          PIC X(01).                       
102000              20 DTL-BLEED       PIC X(01).                       
102100              20 DTL-LVL.                                         
102200                 25 FILLER       PIC X(01).                       
102300                 25 DTL-LEVEL    PIC Z(08).                       
102400              20 DTL-LVL-RE      REDEFINES DTL-LVL.               
102500                 25 DTL-LVL-C    OCCURS 8 TIMES                   
102600                                 PIC X(01).                       
102700           15 DTL-DESC2          REDEFINES DTL-DESCRIPTION.       
102800              20 DTL-CLASS-CODE  PIC X(04).                       
102900              20 DTL-ZONE-CODE   PIC X(02).                       
103000              20 FILLER          PIC X(01).                       
103100              20 DTL-AD-DESC     PIC X(20).                       
103200              20 FILLER          PIC X(01).                       
103300              20 DTL-COPY-NBR-3  PIC X(03).                       
101100           15 DTL-DESC3          REDEFINES DTL-DESCRIPTION.       
101200              20 DTL3-PUB         PIC X(04).                      
101300              20 FILLER           PIC X(01).                      
101400              20 DTL3-EDITION     PIC X(04).                      
101500              20 FILLER           PIC X(01).                      
101600              20 DTL3-AD-TYPE    PIC X(04).                       
101700              20 FILLER          PIC X(01).                       
101600              20 DTL3-AD-TYPE-DESC PIC X(20).                     
101700              20 FILLER          PIC X(03).                       
103400           15 DTL-ACCT-INFO      REDEFINES DTL-DESCRIPTION.       
103500              20 DTL-ACCT-LIT    PIC X(05).                       
103600              20 FILLER          PIC X(01).                       
103700              20 DTL-ACCT-NBR    PIC 9(09).                       
103800              20 FILLER          PIC X(03).                       
103900              20 DTL-J-XREF      PIC X(09).                       
104000           15 DTL-POSN-INFO      REDEFINES DTL-DESCRIPTION.       
104100              20 DTL-DESC-POSN   PIC X(20).                       
104200              20 FILLER          PIC X(02).                       
104300              20 DTL-POSN-RATE   PIC $$$$.99-.                    
104400              20 DTL-POSN-PERCENT REDEFINES DTL-POSN-RATE.        
104500                 25 DTL-POSN-PCT PIC ZZZ9.99.                     
104600                 25 DTL-POSN-LIT PIC X(01).                       
104700           15 DTL-SR-INFO        REDEFINES DTL-DESCRIPTION.       
104800              20 DTL-SR-LIT-1    PIC X(11).                       
104900              20 DTL-SR-PCT      PIC Z9.99.                       
105000              20 DTL-SR-LIT-2    PIC X(09).                       
105100           15 DTL-CODE-INFO      REDEFINES DTL-DESCRIPTION.       
105200              20 DTL-CODE-CODE   PIC X(05).                       
105300              20 DTL-CODE-LIT    PIC X(03).                       
105400              20 DTL-CODE-NAME   PIC X(20).                       
105500           15 DTL-ORD-COM-INFO   REDEFINES DTL-DESCRIPTION.       
105600              20 DTL-ORD-LIT     PIC X(04).                       
105700              20 FILLER          PIC X(01).                       
AL-18 *             20 DTL-ORD-NBR     PIC X(08).                       
AL-18               20 DTL-ORD-NBR     PIC X(12).                       
105900              20 FILLER          PIC X(02).                       
106000              20 DTL-COM-LIT     PIC X(04).                       
106100              20 FILLER          PIC X(01).                       
106110*             20 DTL-COM-NBR     PIC X(08).                       
106200              20 DTL-COM-NBR     PIC X(10).                       
106300           15 DTL-SUB-INFO       REDEFINES DTL-DESCRIPTION.       
106400              20 DTL-SUB-LIT     PIC X(08).                       
106500              20 FILLER          PIC X(01).                       
106600              20 DTL-SUB-NAME    PIC X(25).                       
106700           15 DTL-SUB-INFO-M     REDEFINES DTL-DESCRIPTION.       
106800              20 DTL-SUB-LIT-M   PIC X(05).                       
106900              20 FILLER          PIC X(01).                       
107000              20 DTL-SUB-NAME-M  PIC X(28).                       
107100           15 DTL-ADJUSTMENT-AREA REDEFINES DTL-DESCRIPTION.      
107200              20 FILLER          PIC X(08).                       
107300              20 DTL-ADJ-LIT-1   PIC X(05).                       
107400              20 DTL-ADJ-AMT     PIC X(10).                       
107500              20 DTL-ADJ-AMT-D   REDEFINES DTL-ADJ-AMT            
107600                                    PIC $$$$$$9.99.               
107700              20 DTL-ADJ-AMT-P   REDEFINES DTL-ADJ-AMT            
107800                                    PIC ZZZZZZ9.99.               
107900              20 DTL-ADJ-LIT-2   PIC X(02).                       
108000           15 DTL-CLS-INFO-M     REDEFINES DTL-DESCRIPTION.       
108100              20 DTL-CLS-SUB-NAME PIC X(18).                      
108200              20 FILLER          PIC X(04).                       
108300              20 DTL-CLS-SUB-INFO PIC X(12).                      
108000           15 DTL-WO-INFO        REDEFINES DTL-DESCRIPTION.       
108100              20 DTL-WO-LIT      PIC X(14).                       
AL-04 *             20 DTL-WO-NBR-9    PIC 9(09).                       
AL-04 *             20 DTL-WO-NBR      REDEFINES DTL-WO-NBR-9           
AL-04 *                                PIC X(09).                       
AL-04               20 DTL-WO-NBR-9    PIC z(10).                       
AL-04               20 DTL-WO-NBR      REDEFINES DTL-WO-NBR-9           
AL-04                                  PIC X(10).                       
108400        10 DTL-ITEM-15-16        PIC X(17).                       
108500        10 DTL-ITEM-15-16-X      REDEFINES DTL-ITEM-15-16.        
108600           15 DTL-INFO-AREA      PIC X(17).                       
108700           15 DTL-INFO-AREA-RE   REDEFINES DTL-INFO-AREA.         
108800              20 DTL-DIMENSIONS.                                  
108900                 25 FILLER       PIC X(08).                       
109000                 25 DTL-CLS-LINES-2 PIC X(01).                    
109100              20 FILLER          PIC X(01).                       
109200              20 DTL-LINEAGE-I   PIC ZZZ,ZZZ.                     
109300              20 DTL-LINEAGE-D   REDEFINES DTL-LINEAGE-I          
109400                                    PIC ZZZZ.ZZ.                  
109500              20 DTL-CLS-LINEAGE REDEFINES DTL-LINEAGE-I.         
109600                 25 FILLER       PIC X(01).                       
109700                 25 DTL-CLS-QUANTITY PIC Z(02)9.                  
109800                 25 FILLER       PIC X(01).                       
109900                 25 DTL-CLS-LINES PIC X(01).                      
110000           15 DTL-INFO-AREA-CLS  REDEFINES DTL-INFO-AREA.         
110100              20 DTL-CLS-PROD    PIC X(17).                       
110200        10 DTL-ITEM-17-18        PIC X(08).                       
110300        10 DTL-ITEM-17-18-X      REDEFINES DTL-ITEM-17-18.        
110400           15 FILLER             PIC X(01).                       
AL-06 **         15 DTL-AVG-RATE       PIC ZZZZ.ZZ.                     
AL-06            15 DTL-AVG-RATE       PIC ZZZZ.ZZ                      
AL-06                                  BLANK WHEN ZERO.                 
110600        10 DTL-ITEM-19           PIC X(14).                       
110700        10 DTL-ITEM-19-X         REDEFINES DTL-ITEM-19.           
110800           15 DTL-AMT-MSG.                                        
AL-06               20 DTL-GROSS-AMT-P PIC Z,ZZZ,ZZ9.99CR.              
AL-06 **            20 DTL-GROSS-AMT-P PIC Z,ZZZ,ZZ9.99CR               
AL-06 **                               BLANK WHEN ZERO.                 
111000              20 DTL-GROSS-AMT-X REDEFINES DTL-GROSS-AMT-P        
111100                                 PIC X(14).                       
111200        10 DTL-ITEM-20           PIC X(12).                       
111300        10 DTL-ITEM-20-X         REDEFINES DTL-ITEM-20.           
AL-06 **         15 DTL-NET-AMT-P      PIC ZZZ,ZZ9.99CR.                
AL-06            15 DTL-NET-AMT-P      PIC ZZZ,ZZ9.99CR                 
AL-06                                  BLANK WHEN ZERO.                 
111500           15 DTL-NET-AMT-X      REDEFINES DTL-NET-AMT-P          
111600                                 PIC X(12).                       
111200* CTS-02/23/07 BEGINS                                             
111200        10 DTL-CURR-VAL        PIC X(05).                         
111200* CTS-02/23/07 ENDS                                               
111700                                                                  
111800     05 DTL-LINE-B               REDEFINES DTL-LINE.              
111900        10 FILLER                PIC X(07).                       
112000        10 DTL-PAGE-LIT          PIC X(03).                       
112100        10 DTL-PAGE              PIC X(03).                       
112200        10 FILLER                PIC X(04).                       
112300        10 DTL-COPY-NBR          PIC X(06).                       
112400                                                                  
112500     05 DTL-LINE-C               REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-CNT-START         PIC X(08).                       
112800        10 DTL-CNT-LIT2          PIC X(03).                       
112900        10 DTL-CNT-END           PIC X(08).                       
113000        10 FILLER                PIC X(02).                       
113100        10 DTL-CNT-LIT11         PIC X(05).                       
113200        10 DTL-CNT-NBR           PIC Z(03)9.                      
113300                                                                  
113400     05 DTL-LINE-D               REDEFINES DTL-LINE.              
113500        10 FILLER                PIC X(24).                       
113600        10 DTL-CNT-OPEN.                                          
113700           15 DTL-CNT-LEVEL      PIC Z(05)9.                      
113800           15 FILLER             PIC X(01).                       
113900           15 DTL-CNT-OPTION.                                     
114000              20 DTL-CNT-LIT7    PIC X(10).                       
114100              20 FILLER          PIC X(01).                       
114200              20 DTL-CNT-LIT8A   PIC X(02).                       
114300              20 FILLER          PIC X(01).                       
114400              20 DTL-CNT-LIT8    PIC X(20).                       
114500           15 DTL-CNT-OPTION-2   REDEFINES DTL-CNT-OPTION.        
114600              20 DTL-CNT-LIT9    PIC X(07).                       
114700              20 FILLER          PIC X(01).                       
114800              20 DTL-CNT-LIT10A  PIC X(02).                       
114900              20 FILLER          PIC X(01).                       
115000              20 DTL-CNT-LIT10   PIC X(20).                       
115100                                                                  
115200     05 DTL-LINE-E               REDEFINES DTL-LINE.              
115300        10 FILLER                PIC X(24).                       
115400        10 DTL-CNT-LIT3          PIC X(06).                       
115500        10 DTL-CNT-PUB           PIC X(04).                       
115600        10 FILLER                PIC X(01).                       
115700        10 DTL-CNT-LIT4          PIC X(06).                       
115800        10 DTL-CNT-EDTN          PIC X(04).                       
115900        10 FILLER                PIC X(01).                       
116000        10 DTL-CNT-LIT5          PIC X(06).                       
116100        10 DTL-CNT-ADTP          PIC X(04).                       
116200                                                                  
116300     05 DTL-LINE-F               REDEFINES DTL-LINE.              
116400        10 FILLER                PIC X(24).                       
116500        10 DTL-PFM-LIT1          PIC X(16).                       
116600        10 DTL-PFM-AMT-1A        PIC ZZZ.                         
116700        10 FILLER                PIC X(02).                       
116800        10 DTL-PFM-LIT2          PIC X(07).                       
116900        10 DTL-PFM-AMT-2A-L      PIC ZZ,ZZZ.                      
117000        10 DTL-PFM-AMT-2A-I      REDEFINES DTL-PFM-AMT-2A-L       
117100                                 PIC ZZZ.ZZ.                      
117200                                                                  
117300     05 DTL-LINE-F               REDEFINES DTL-LINE.              
117400        10 FILLER                PIC X(07).                       
117500        10 DTL-INVC-MSG          PIC X(75).                       
117600                                                                  
117700     05 DTL-LINE-TOT             REDEFINES DTL-LINE.              
117800        10 FILLER                PIC X(79).                       
117900        10 DTL-AMT-TOT-P         PIC ZZZ,ZZZ,ZZ9.99CR.            
118000        10 DTL-AMT-TOT-X         REDEFINES DTL-AMT-TOT-P          
118100                                 PIC X(16).                       
118200                                                                  
117200                                                                  
AL-06A     05 DTL-LINE-G               REDEFINES DTL-LINE.              
AL-06A        10 FILLER                PIC X(32).                       
AL-06A        10 DTL-HEAD-MSG          PIC X(30).                       
AL-06A                                                                  
111200* CTS-02/23/07 BEGINS                                             
112500     05 DTL-LINE-H               REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-WO-INV-LIT        PIC X(20).                       
113000        10 FILLER                PIC X(01).                       
alxx          10 DTL-WO-INVC-NBR       PIC 9(12).                       
112400                                                                  
112500     05 DTL-LINE-H-A             REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-EDN-EURO-LIT      PIC X(15).                       
113000        10 FILLER                PIC X(01).                       
112800        10 DTL-EDN-EURO-DESC     PIC X(20).                       
112400                                                                  
112500     05 DTL-LINE-H-B             REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-GLB-BUY-LIT       PIC X(03).                       
113000        10 FILLER                PIC X(01).                       
112800        10 DTL-GLB-BUY-VAL       PIC X(06).                       
112400                                                                  
112500     05 DTL-LINE-H-c             REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-AD-DESC-LIT       PIC X(08).                       
113000        10 FILLER                PIC X(01).                       
112800        10 DTL-AD-DESC-VAL       PIC X(24).                       
112400                                                                  
112500     05 DTL-LINE-I               REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-AGY-PCT-LIT       PIC X(05).                       
113000        10 FILLER                PIC X(01).                       
112700        10 DTL-AGY-COMM-PCT      PIC z9.99.                       
113000        10 FILLER                PIC X(01).                       
112700        10 DTL-AGY-AMT-LIT       PIC X(06).                       
113000        10 FILLER                PIC X(01).                       
112800        10 DTL-AGY-COMM-AMT      PIC ZZZ,ZZZ,ZZ9.99.              
113000        10 FILLER                PIC X(01).                       
112700        10 DTL-TAX-PCT-LIT       PIC X(04).                       
113000        10 FILLER                PIC X(01).                       
112900        10 DTL-TAX-WO-PCT        PIC Z9.99.                       
113000        10 FILLER                PIC X(01).                       
112700        10 DTL-TAX-AMT-LIT       PIC X(06).                       
113000        10 FILLER                PIC X(01).                       
113100        10 DTL-TAX-WO-AMT        PIC ZZZ,ZZZ,ZZ9.99.              
112400                                                                  
112500     05 DTL-LINE-I-A             REDEFINES DTL-LINE.              
112600        10 FILLER                PIC X(24).                       
112700        10 DTL-VAT-CD-LIT        PIC X(16).                       
113000        10 FILLER                PIC X(01).                       
112700        10 DTL-VAT-CD-VAL        PIC X(15).                       
111200* CTS-02/23/07 ENDS                                               
116200                                                                  
118300     05 DTL-LINE-BLANK.                                           
118400        10 DTL-P-CB              PIC X(01).                       
118500        10 DTL-P-FONT            PIC X(01).                       
118600        10 DTL-P-ROUTE           PIC X(01).                       
118700        10 DTL-P-PAGE            PIC X(01).                       
118800/                                                                 
-2001- 01  WS-LOCKBOX-REC.                                              
09/07-     05 LOCKBOX-REC-TYPE-1.                                       
09/07-        10 FILLER                PIC 9(03)    VALUE 100.          
09/07-        10 FILLER                PIC X(10)    VALUE 'THE NYTIME'. 
09/07-        10 FILLER                PIC X(10)    VALUE ZEROS.        
09/07-        10 LOCKBOX-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LOCKBOX-DEPOSIT-TIME.                                  
09/07-           15 LOCKBOX-DEPOSIT-HH PIC 9(02)    VALUE ZEROS.        
09/07-           15 LOCKBOX-DEPOSIT-MM PIC 9(02)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACES.       
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-5.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 5.            
09/07-        10 LBX-R5-BCH-NBR        PIC 9(03)    VALUE 1.            
09/07-        10 LBX-R5-ITEM-NBR       PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R5-LOCKBOX-NBR.                                    
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(02)    VALUE 16.           
09/07-           15 FILLER             PIC 9(04)    VALUE 3000.         
09/07-        10 LBX-R5-DEPOSIT-DATE   PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(60)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-6.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBX-R6-BCH-NBR        PIC 9(03)    VALUE 1.            
09/07-        10 LBX-R6-ITEM-NBR       PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R6-REMIT-AMT      PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 LBX-R6-CC-NBR         PIC X(16)    VALUE SPACE.        
09/07-        10 LBX-R6-CC-EXP-DATE    PIC X(04)    VALUE SPACE.        
09/07-        10 LBX-R6-CHECK-NBR      PIC X(07)    VALUE SPACE.        
09/07-        10 LBX-R6-ACCT-NAME      PIC X(27)    VALUE SPACE.        
09/07-        10 LBX-R6-INVC-NBR       PIC 9(09)    VALUE ZEROS.        
09/07-        10 LBX-R6-INVC-NBR-9     REDEFINES    LBX-R6-INVC-NBR.    
09/07-           15 LBX-R6-INVC-NBR-6  PIC 9(06).                       
09/07-           15 LBX-R6-INVC-NBR-3  PIC 9(03).                       
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-4.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 4.            
09/07-        10 LBX-R4-BCH-NBR        PIC 9(03)    VALUE 1.            
09/07-        10 LBX-R4-ITEM-NBR       PIC 9(03)    VALUE ZEROS.        
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBX-R4-SEQ-NBR        PIC 9(02)    VALUE ZEROS.        
09/07-        10 LBX-R4-OVERFLOW-REC   PIC 9(01)    VALUE ZEROS.        
09/07-        10 LBX-R4-MULTI-INVC                  VALUE SPACE.        
09/07-           15 LBX-R4-INVC-NBR OCCURS 7 TIMES INDEXED BY R4X       
09/07-                                 PIC 9(09).                       
09/07-        10 FILLER                PIC X(06)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-7.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 7.            
09/07-        10 LBX-R7-BCH-NBR        PIC 9(03)    VALUE 1.            
09/07-        10 LBX-R7-ITEM-NBR       PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R7-LOCKBOX-NBR.                                    
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBX-R7-DEPOSIT-DATE   PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBX-R7-NBR-OF-CHECKS  PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R7-BCH-TOTAL      PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(47)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-8.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 8.            
09/07-        10 FILLER                PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R8-NBR-OF-ITEMS   PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBX-R8-LOCKBOX-NBR.                                    
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBX-R8-DEPOSIT-DATE   PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBX-R8-NBR-OF-CHECKS  PIC 9(04)    VALUE ZEROS.        
09/07-        10 LBX-R8-REMIT-TOT      PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOX-REC-TYPE-9.                                       
09/07-        10 FILLER                PIC 9(01)    VALUE 9.            
09/07-        10 LBX-R9-NBR-OF-RECS    PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(73)    VALUE SPACE.        
09/07-                                                                  
      *CTS - 04/12/07 CHANGE BEGINS                                     
-2001- 01  WS-LOCKBOXI-REC.                                             
09/07-     05 LOCKBOXI-REC-TYPE-1.                                      
09/07-        10 FILLER                PIC 9(03)    VALUE 100.          
09/07-        10 FILLER                PIC X(10)    VALUE 'THE NYTIME'. 
09/07-        10 FILLER                PIC X(10)    VALUE ZEROS.        
09/07-        10 LOCKBOXI-DEPOSIT-DATE PIC 9(06)    VALUE ZEROS.        
09/07-        10 LOCKBOXI-DEPOSIT-TIME.                                 
09/07-           15 LOCKBOXI-DEPOSIT-HH PIC 9(02)    VALUE ZEROS.       
09/07-           15 LOCKBOXI-DEPOSIT-MM PIC 9(02)    VALUE ZEROS.       
09/07-        10 FILLER                PIC X(46)    VALUE SPACES.       
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-5.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 5.            
09/07-        10 LBXI-R5-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXI-R5-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R5-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(02)    VALUE 16.           
09/07-           15 FILLER             PIC 9(04)    VALUE 3000.         
09/07-        10 LBXI-R5-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(60)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-6.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXI-R6-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXI-R6-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R6-REMIT-AMT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 LBXI-R6-CC-NBR        PIC X(16)    VALUE SPACE.        
09/07-        10 LBXI-R6-CC-EXP-DATE   PIC X(04)    VALUE SPACE.        
09/07-        10 LBXI-R6-CHECK-NBR     PIC X(07)    VALUE SPACE.        
09/07-        10 LBXI-R6-ACCT-NAME     PIC X(27)    VALUE SPACE.        
09/07-        10 LBXI-R6-INVC-NBR      PIC 9(09)    VALUE ZEROS.        
09/07-        10 LBXI-R6-INVC-NBR-9    REDEFINES    LBXI-R6-INVC-NBR.   
09/07-           15 LBXI-R6-INVC-NBR-6 PIC 9(06).                       
09/07-           15 LBXI-R6-INVC-NBR-3 PIC 9(03).                       
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-4.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 4.            
09/07-        10 LBXI-R4-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXI-R4-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXI-R4-SEQ-NBR       PIC 9(02)    VALUE ZEROS.        
09/07-        10 LBXI-R4-OVERFLOW-REC  PIC 9(01)    VALUE ZEROS.        
09/07-        10 LBXI-R4-MULTI-INVC                  VALUE SPACE.       
09/07-           15 LBXI-R4-INVC-NBR OCCURS 7 TIMES INDEXED BY R4XI     
09/07-                                 PIC 9(09).                       
09/07-        10 FILLER                PIC X(06)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-7.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 7.            
09/07-        10 LBXI-R7-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXI-R7-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R7-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXI-R7-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXI-R7-NBR-OF-CHECKS PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R7-BCH-TOTAL     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(47)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-8.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 8.            
09/07-        10 FILLER                PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R8-NBR-OF-ITEMS  PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXI-R8-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXI-R8-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXI-R8-NBR-OF-CHECKS PIC 9(04)    VALUE ZEROS.        
09/07-        10 LBXI-R8-REMIT-TOT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXI-REC-TYPE-9.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 9.            
09/07-        10 LBXI-R9-NBR-OF-RECS   PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(73)    VALUE SPACE.        
09/07-                                                                  
       01  CC-TRAILERI-REC.                                             
           05 TRL-TRANI-CODE           PIC 9(02)    VALUE ZEROS.        
           05 FILLER                   PIC X(37)    VALUE SPACES.       
           05 TRL-TOTALI-AMT           PIC 9(08)V99 VALUE ZEROS.        
           05 FILLER                   PIC X(01)    VALUE SPACES.       
           05 TRL-RECI-COUNT           PIC 9(06)    VALUE ZEROS.        
           05 FILLER                   PIC X(24)    VALUE SPACES.       
                                                                        
       01  CC-SUMMARYI-REC.                                             
           05 SUM-TRANI-CODE           PIC 9(02)    VALUE ZEROS.        
           05 FILLER                   PIC X(01)    VALUE SPACES.       
           05 SUM-MERCHI-NBR           PIC 9(11)    VALUE ZEROS.        
           05 FILLER                   PIC X(16)    VALUE SPACES.       
           05 SUM-NET-SALESI-AMT       PIC 9(08)V99 VALUE ZEROS.        
           05 FILLER                   PIC X(40)    VALUE SPACES.       
                                                                        
-2001- 01  WS-LOCKBOXA-REC.                                             
09/07-     05 LOCKBOXA-REC-TYPE-1.                                      
09/07-        10 FILLER                PIC 9(03)    VALUE 100.          
09/07-        10 FILLER                PIC X(10)    VALUE 'THE NYTIME'. 
09/07-        10 FILLER                PIC X(10)    VALUE ZEROS.        
09/07-        10 LOCKBOXA-DEPOSIT-DATE PIC 9(06)    VALUE ZEROS.        
09/07-        10 LOCKBOXA-DEPOSIT-TIME.                                 
09/07-           15 LOCKBOXA-DEPOSIT-HH PIC 9(02)    VALUE ZEROS.       
09/07-           15 LOCKBOXA-DEPOSIT-MM PIC 9(02)    VALUE ZEROS.       
09/07-        10 FILLER                PIC X(46)    VALUE SPACES.       
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-5.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 5.            
09/07-        10 LBXA-R5-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXA-R5-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R5-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(02)    VALUE 16.           
09/07-           15 FILLER             PIC 9(04)    VALUE 3000.         
09/07-        10 LBXA-R5-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(60)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-6.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXA-R6-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXA-R6-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R6-REMIT-AMT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 LBXA-R6-CC-NBR        PIC X(16)    VALUE SPACE.        
09/07-        10 LBXA-R6-CC-EXP-DATE   PIC X(04)    VALUE SPACE.        
09/07-        10 LBXA-R6-CHECK-NBR     PIC X(07)    VALUE SPACE.        
09/07-        10 LBXA-R6-ACCT-NAME     PIC X(27)    VALUE SPACE.        
09/07-        10 LBXA-R6-INVC-NBR      PIC 9(09)    VALUE ZEROS.        
09/07-        10 LBXA-R6-INVC-NBR-9    REDEFINES    LBXA-R6-INVC-NBR.   
09/07-           15 LBXA-R6-INVC-NBR-6 PIC 9(06).                       
09/07-           15 LBXA-R6-INVC-NBR-3 PIC 9(03).                       
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-4.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 4.            
09/07-        10 LBXA-R4-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXA-R4-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXA-R4-SEQ-NBR       PIC 9(02)    VALUE ZEROS.        
09/07-        10 LBXA-R4-OVERFLOW-REC  PIC 9(01)    VALUE ZEROS.        
09/07-        10 LBXA-R4-MULTI-INVC                  VALUE SPACE.       
09/07-           15 LBXA-R4-INVC-NBR OCCURS 7 TIMES INDEXED BY R4XA     
09/07-                                 PIC 9(09).                       
09/07-        10 FILLER                PIC X(06)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-7.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 7.            
09/07-        10 LBXA-R7-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXA-R7-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R7-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXA-R7-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXA-R7-NBR-OF-CHECKS PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R7-BCH-TOTAL     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(47)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-8.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 8.            
09/07-        10 FILLER                PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R8-NBR-OF-ITEMS  PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXA-R8-LOCKBOX-NBR.                                   
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXA-R8-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXA-R8-NBR-OF-CHECKS PIC 9(04)    VALUE ZEROS.        
09/07-        10 LBXA-R8-REMIT-TOT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXA-REC-TYPE-9.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 9.            
09/07-        10 LBXA-R9-NBR-OF-RECS   PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(73)    VALUE SPACE.        
09/07-                                                                  
       01  CC-TRAILERA-REC.                                             
           05 TRL-TRANA-CODE           PIC 9(02)    VALUE ZEROS.        
           05 FILLER                   PIC X(37)    VALUE SPACES.       
           05 TRL-TOTALA-AMT           PIC 9(08)V99 VALUE ZEROS.        
           05 FILLER                   PIC X(01)    VALUE SPACES.       
           05 TRL-RECA-COUNT           PIC 9(06)    VALUE ZEROS.        
           05 FILLER                   PIC X(24)    VALUE SPACES.       
                                                                        
       01  CC-SUMMARYA-REC.                                             
           05 SUM-TRANA-CODE           PIC 9(02)    VALUE ZEROS.        
           05 FILLER                   PIC X(01)    VALUE SPACES.       
           05 SUM-MERCHA-NBR           PIC 9(11)    VALUE ZEROS.        
           05 FILLER                   PIC X(16)    VALUE SPACES.       
           05 SUM-NET-SALESA-AMT       PIC 9(08)V99 VALUE ZEROS.        
           05 FILLER                   PIC X(40)    VALUE SPACES.       
                                                                        
       01  wk-ftp-info.                                                 
           05 WK-INV-EU-FTP            PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.EU.MBL.NEW.TECH.RECORD'' '.  
           05 WK-INV-EU-FTP1           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.EU.DBL.NEW.TECH.RECORD'' '.  
           05 WK-INV-EU-FTP2           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.EU.WBL.NEW.TECH.RECORD'' '.  
      *CTS - 07/05/07 CHANGE BEGINS                                     
           05 WK-INV-EU-FTP3           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.EU.RBL.NEW.TECH.RECORD'' '.  
      *CTS - 07/05/07 CHANGE ENDS                                       
           05 WK-INV-DO-FTP            PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.DO.MBL.NEW.TECH.RECORD'' '.  
           05 WK-INV-DO-FTP1           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.DO.DBL.NEW.TECH.RECORD'' '.  
           05 WK-INV-DO-FTP2           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.DO.WBL.NEW.TECH.RECORD'' '.  
      *CTS - 07/05/07 CHANGE BEGINS                                     
           05 WK-INV-DO-FTP3           PIC x(60) value space.           
      *dw-30          VALUE 'PUT ''NAMPROD.DO.RBL.NEW.TECH.RECORD'' '.  
      *CTS - 07/05/07 CHANGE ENDS                                       
       01  WK-FILE-NAME.                                                
           05 WK-FILE-DATE             PIC X(08).                       
           05 WK-FILE-FILLER           PIC X(01).                       
           05 WK-FILE-CUR              PIC X(02).                       
           05 WK-FILE-FILLER1          PIC X(01).                       
           05 WK-FILE-FREQ             PIC X(07).                       
                                                                        
      *CTS - 04/12/07 CHANGE ENDS                                       
dw-08- 01  WS-LOCKBOXN-REC.                                             
09/07-     05 LOCKBOXN-REC-TYPE-1.                                      
09/07-        10 FILLER                PIC 9(03)    VALUE 100.          
09/07-        10 FILLER                PIC X(10)    VALUE 'THE NYTIME'. 
09/07-        10 FILLER                PIC X(10)    VALUE ZEROS.        
09/07-        10 LOCKBOXN-DEPOSIT-DATE PIC 9(06)    VALUE ZEROS.        
09/07-        10 LOCKBOXN-DEPOSIT-TIME.                                 
09/07-           15 LOCKBOXN-DEPOSIT-HH PIC 9(02)   VALUE ZEROS.        
09/07-           15 LOCKBOXN-DEPOSIT-MM PIC 9(02)   VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACES.       
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-5.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 5.            
09/07-        10 LBXN-R5-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXN-R5-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R5-LOCKBOXN-NBR.                                  
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(02)    VALUE 16.           
09/07-           15 FILLER             PIC 9(04)    VALUE 3000.         
09/07-        10 LBXN-R5-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(60)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-6.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXN-R6-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXN-R6-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R6-REMIT-AMT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 LBXN-R6-CC-NBR        PIC X(16)    VALUE SPACE.        
09/07-        10 LBXN-R6-CC-EXP-DATE   PIC X(04)    VALUE SPACE.        
09/07-        10 LBXN-R6-CHECK-NBR     PIC X(07)    VALUE SPACE.        
09/07-        10 LBXN-R6-ACCT-NAME     PIC X(27)    VALUE SPACE.        
09/07-        10 LBXN-R6-INVC-NBR      PIC 9(09)    VALUE ZEROS.        
09/07-        10 LBXN-R6-INVC-NBR-9    REDEFINES    LBXN-R6-INVC-NBR.   
09/07-           15 LBXN-R6-INVC-NBR-6 PIC 9(06).                       
09/07-           15 LBXN-R6-INVC-NBR-3 PIC 9(03).                       
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-4.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 4.            
09/07-        10 LBXN-R4-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXN-R4-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 FILLER                PIC 9(01)    VALUE 6.            
09/07-        10 LBXN-R4-SEQ-NBR       PIC 9(02)    VALUE ZEROS.        
09/07-        10 LBXN-R4-OVERFLOW-REC  PIC 9(01)    VALUE ZEROS.        
09/07-        10 LBXN-R4-MULTI-INVC                 VALUE SPACE.        
09/07-           15 LBXN-R4-INVC-NBR OCCURS 7 TIMES INDEXED BY R4XN     
09/07-                                 PIC 9(09).                       
09/07-        10 FILLER                PIC X(06)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-7.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 7.            
09/07-        10 LBXN-R7-BCH-NBR       PIC 9(03)    VALUE 1.            
09/07-        10 LBXN-R7-ITEM-NBR      PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R7-LOCKBOXN-NBR.                                  
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXN-R7-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXN-R7-NBR-OF-CHECKS PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R7-BCH-TOTAL     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(47)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-8.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 8.            
09/07-        10 FILLER                PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R8-NBR-OF-ITEMS  PIC 9(03)    VALUE ZEROS.        
09/07-        10 LBXN-R8-LOCKBOXN-NBR.                                  
09/07-           15 FILLER             PIC X(01)    VALUE SPACE.        
09/07-           15 FILLER             PIC 9(06)    VALUE 163000.       
09/07-        10 LBXN-R8-DEPOSIT-DATE  PIC 9(06)    VALUE ZEROS.        
09/07-        10 LBXN-R8-NBR-OF-CHECKS PIC 9(04)    VALUE ZEROS.        
09/07-        10 LBXN-R8-REMIT-TOT     PIC 9(08)V99 VALUE ZEROS.        
09/07-        10 FILLER                PIC X(46)    VALUE SPACE.        
09/07-                                                                  
09/07-     05 LOCKBOXN-REC-TYPE-9.                                      
09/07-        10 FILLER                PIC 9(01)    VALUE 9.            
09/07-        10 LBXN-R9-NBR-OF-RECS   PIC 9(06)    VALUE ZEROS.        
09/07-        10 FILLER                PIC X(73)    VALUE SPACE.        
09/07-                                                                  
09/07- 01  RPT-CC-LINE.                                                 
09/07-     05  RPT-DTL-LINE.                                            
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-ACCT-JXRF.                                    
09/07-             15  RPT-CC-ACCT-NBR PIC 9(09).                       
09/07-             15  FILLER          PIC X(03).                       
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-TYPE         PIC X(01).                       
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-NBR          PIC X(20).                       
09/07-         10  FILLER              PIC X(02).                       
09/07-         10  RPT-CC-EXP-DATE     PIC 9(04).                       
09/07-         10  FILLER              PIC X(02).                       
09/07-         10  RPT-CC-AUTH         PIC X(06).                       
09/07-         10  FILLER              PIC X(02).                       
09/07-         10  RPT-CC-INVC-NBR     PIC 9(09).                       
09/07-         10  FILLER              PIC X(03).                       
09/07-         10  RPT-CC-JOB-NBR      PIC 9(09).                       
09/07-         10  FILLER              PIC X(02).                       
09/07-         10  RPT-CC-ADV-NAME     PIC X(28).                       
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-INVC-AMT     PIC ZZZZ,ZZ9.99CR.               
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-CHRG-AMT     PIC ZZZZ,ZZ9.99CR.               
AL-17          10  FILLER              PIC X(01).                       
AL-17          10  RPT-CC-LEVEL-DISC   PIC X(01).                       
09/07-                                                                  
09/07-     05  RPT-SUM-LINE            REDEFINES RPT-DTL-LINE.          
09/07-         10  FILLER              PIC X(75).                       
09/07-         10  RPT-CC-TOT-DESC     PIC X(22).                       
09/07-         10  RPT-CC-TOT-COUNT    PIC ZZ,ZZ9.                      
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-TOT-AMT      PIC ZZZZ,ZZ9.99CR.               
09/07-         10  FILLER              PIC X(01).                       
09/07-         10  RPT-CC-TOT-CHRG     PIC ZZZZ,ZZ9.99CR.               
09/07-                                                                  
09/07- 01  CC-RPT-HEADERS.                                              
09/07-     05 RPT-HDR1.                                                 
09/07-        10 FILLER                PIC X(02) VALUE SPACE.           
09/07-        10 FILLER                PIC X(25) VALUE                  
09/07-           'The New York Times CO.'.                              
09/07-        10 FILLER                PIC X(05) VALUE SPACE.           
09/07-        10 FILLER                PIC X(30) VALUE                  
09/07-           'NYT Admarc Invoice Processing'.                       
09/07-        10 FILLER                PIC X(34) VALUE                  
09/07-           'Report - NON Transient Classified'.                   
09/07-        10 FILLER                PIC X(18) VALUE SPACE.           
09/07-        10 FILLER                PIC X(10) VALUE 'Page Nbr: '.    
09/07-        10 HDR-CC-PAGE           PIC ZZZZZZZ9.                    
09/07-                                                                  
09/07-     05 RPT-HDR2.                                                 
09/07-        10 FILLER                PIC X(02) VALUE SPACE.           
09/07-        10 FILLER                PIC X(25) VALUE                  
09/07-           'Report: AMFBNYTG 7.0'.                                
09/07-        10 FILLER                PIC X(07) VALUE SPACE.           
09/07-        10 FILLER                PIC X(60) VALUE                  
09/07-        '                      Credit Card List'.                 
09/07-        10 FILLER                PIC X(20) VALUE SPACE.           
09/07-        10 FILLER                PIC X(10) VALUE 'Run Date: '.    
09/07-        10 HDR-CC-DATE           PIC X(08).                       
09/07-                                                                  
09/07-     05 RPT-HDR3.                                                 
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(12) VALUE 'Account#'.      
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(22) VALUE 'Credit Card#'.  
09/07-        10 FILLER                PIC X(02) VALUE SPACE.           
09/07-        10 FILLER                PIC X(03) VALUE 'Exp'.           
09/07-        10 FILLER                PIC X(03) VALUE SPACE.           
09/07-        10 FILLER                PIC X(04) VALUE 'Auth'.          
09/07-        10 FILLER                PIC X(04) VALUE SPACE.           
09/07-        10 FILLER                PIC X(08) VALUE 'Invoice#'.      
09/07-        10 FILLER                PIC X(04) VALUE SPACE.           
09/07-        10 FILLER                PIC X(04) VALUE 'Job#'.          
09/07-        10 FILLER                PIC X(07) VALUE SPACE.           
09/07-        10 FILLER                PIC X(28) VALUE 'Advertiser'.    
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(12) VALUE 'Invoice Amt'.   
09/07-        10 FILLER                PIC X(03) VALUE SPACE.           
09/07-        10 FILLER                PIC X(11) VALUE 'Charge Amt'.    
09/07-                                                                  
09/07-     05 RPT-HDR4.                                                 
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(12) VALUE '________'.      
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(22) VALUE '______ _____'.  
09/07-        10 FILLER                PIC X(02) VALUE SPACE.           
09/07-        10 FILLER                PIC X(03) VALUE '___'.           
09/07-        10 FILLER                PIC X(03) VALUE SPACE.           
09/07-        10 FILLER                PIC X(04) VALUE '____'.          
09/07-        10 FILLER                PIC X(04) VALUE SPACE.           
09/07-        10 FILLER                PIC X(08) VALUE '________'.      
09/07-        10 FILLER                PIC X(04) VALUE SPACE.           
09/07-        10 FILLER                PIC X(04) VALUE '____'.          
09/07-        10 FILLER                PIC X(07) VALUE SPACE.           
09/07-        10 FILLER                PIC X(28) VALUE '__________'.    
09/07-        10 FILLER                PIC X(01) VALUE SPACE.           
09/07-        10 FILLER                PIC X(12) VALUE '_______ ___'.   
09/07-        10 FILLER                PIC X(03) VALUE SPACE.           
FXC---        10 FILLER                PIC X(11) VALUE '______ ___'.    
      /                                                                 
118900 01  CDT-REC.                    COPY AMZRCDT.                    
119000/                                                                 
119100 01  NAD-REC.                    COPY AMZRNAD.                    
119200/                                                                 
119300 01  NA2-REC.                    COPY AMZRNA2.                    
119400/                                                                 
119500 01  XRF-REC.                    COPY AMZRXRF.                    
119600/                                                                 
119700 01  RC-REC.                     COPY AMZRRC.                     
119800/                                                                 
119900 01  CNT-REC.                    COPY AMZRCNT.                    
120000/                                                                 
120100 01  PUB-REC.                    COPY AMZRPUB.                    
120200/                                                                 
120300 01  WO-REC.                     COPY AMZRWO.                     
120400/                                                                 
120500 01  MSG-REC.                    COPY AMZRMSG.                    
120600/                                                                 
120700 01  COD-REC.                    COPY AMZRCOD.                    
120800/                                                                 
120900 01  COD2-REC.                   COPY AMZRCOD2.                   
121000/                                                                 
121100 01  COD3-REC.                   COPY AMZRCOD3.                   
121200/                                                                 
121300 01  SH-REC.                     COPY AMZRSH.                     
121400/                                                                 
121500 01  SHI-REC.                    COPY AMZRSHI.                    
121600/                                                                 
121700 01  CD-REC.                     COPY AMZRCOM.                    
121800/                                                                 
121900 01  AR-NAD-REC.                 COPY AMZRAR.                     
122000/                                                                 
122100 01  IO-PKT.                     COPY AMZWIOP.                    
122200/                                                                 
122300 01  DB-FILES.                   COPY AMZWFILE.                   
122400/                                                                 
122500 01  UTL-WORK.                   COPY AMZWUTL.                    
122600/                                                                 
122700 01  CALC-PKT.                   COPY AMZWCALC.                   
122800/                                                                 
122900 01  SAVE-WO-PKT.                COPY AMZWINFO.                   
123000/                                                                 
123100 01  DAY-PKT.                    COPY AMZWDAY.                    
123200/                                                                 
123300 01  PRT-PKT.                    COPY AMZWPRT.                    
123400                                                                  
123600 01  MFLD-FIELDS.                COPY AMZWMFLD.                   
123700*                                                                 
123800 01  GET-CCH-PKT.                COPY AMZWCCH.                    
123900*                                                                 
124000 01  WO3-REC.                    COPY AMZRWO3.      
123900*                                                                 
124000 01  WO4-REC.                    COPY AMZRWO4.                    
123900*                                                                 
AL-28  01  WO6-REC.                    COPY AMZRWO6.                    
AL-28 *                                                                 
055900 01  NA3-REC.                    COPY AMZRNA3.                    
124100/                                                                 
124200 01  RC2-REC.                    COPY AMZRRC2.                    
124100/                                                                 
124200 01  INVC-REC.                   COPY AMZRINV.                    
124300/                                                                 
AL-06  01  SEL4-REC.                   COPY AMZRSEL4.                   
AL-06      05  SEL4-REC-NOT-COPIED.                                     
AL-06          25  SEL4-AGY-TYPE       PIC X(02).                       
AL-06          25  SEL4-ADV-TYPE       PIC X(02).                       
AL-06                                                                   
AL-06          25  SEL4-ADV-PAR-INFO.                                   
AL-06              30 SEL4-ADV-PAR-TYPE PIC X(02).                      
AL-06              30 SEL4-ADV-PAR-KEY PIC X(15).                       
AL-06              30 SEL4-ADV-PAR-NBR PIC 9(09) comp.                  
AL-06                                                                   
AL-06          25  SEL4-WO-INFO.                                        
AL-06              30  SEL4-ISSUE-WO   PIC 9(08) comp.                  
AL-06              30  SEL4-ISSUE-WO-X REDEFINES SEL4-ISSUE-WO          
AL-06                                  PIC X(04).                       
AL-06              30  SEL4-EDITION-WO PIC X(04).                       
AL-06              30  SEL4-PAGE-WO    PIC X(06).                       
AL-06                                                                   
AL-06          25  SEL4-NAT-AGY        PIC X(01).                       
AL-06          25  SEL4-REF-NBR-2      PIC 9(08) comp.                  
AL-06                                                                   
AL-06      05  SEL4-SORT-AGY.                                           
AL-06          25  SEL4-S-AGY-TYPE     PIC X(02).                       
AL-06          25  SEL4-S-AGY-KEY      PIC X(15).                       
AL-06          25  SEL4-S-AGY-ACCT-NBR PIC 9(09) comp.                  
AL-06                                                                   
AL-06      05  SEL4-SORT-ADV-PAR.                                       
AL-06          25  SEL4-S-CATEGORY     PIC X(01).                       
AL-06          25  SEL4-S-ADV-PAR-KEY  PIC X(15).                       
AL-06          25  SEL4-S-ADV-PAR-NBR  PIC 9(09) comp.                  
AL-06                                                                   
AL-06      05  SEL4-SORT-ADV.                                           
AL-06          25  SEL4-S-ADV-KEY      PIC X(15).                       
AL-06          25  SEL4-S-ACCT-NBR     PIC 9(09) comp.                  
AL-06                                                                   
AL-06      05  SEL4-SORT-PUB-CNT.                                       
AL-06          25  SEL4-S-GROUP        PIC X(06).                       
AL-06          25  SEL4-S-PUB          PIC X(04).                       
AL-06          25  SEL4-S-AD-TYPE      PIC X(04).                       
AL-06          25  SEL4-S-REF-NBR-X    PIC X(04).                       
AL-06                                                                   
AL-06      05  SEL4-SORT-WO.                                            
AL-06          25  SEL4-S-PROD-KEY     PIC X(12).                       
AL-06          25  SEL4-S-ISSUE-WO-X   PIC X(04).                       
AL-06          25  SEL4-S-EDITION-WO   PIC X(04).                       
AL-06          25  SEL4-S-PAGE-WO      PIC X(06).                       
AL-06          25  SEL4-S-SEQ-KEY      PIC 9(04) comp.                  
AL-06          25  SEL4-S-ADJ-KEY      PIC 9(04) comp.                  
124300/                                                                 
124400 01  HDRCRD-TBL.                                                  
124500     05 HDRCRD-ID                PIC X(06) VALUE 'BILNYT'.        
124600     05 FILLER                   PIC 9(04) comp VALUE 0.          
124700/                                                                 
124800 01  TB10-REC.                   COPY RTIRB10.                    
124900/                                                                 
125000 01  TB11-REC.                   COPY RTIRB11.                    
      *CTS - 02/23/07 CHANGE BEGINS                                     
124900*NEW-TECH FILE COPY BOOK                                          
********                                COPY AMZRNWT.                   
026500                                  COPY AMZRNWT-NYT-exp.               
124900*                                                                 
004133 01  EOF-EUROR-FLAG               PIC X(01) VALUE 'N'.            
044400 01  WK-PROC-HDR-FLG              PIC X(01) VALUE SPACE.          
004133 01  WK-COUNT                     PIC 9(02).                      
004133 01  WK-COM-PCT                   PIC 9(02)V99.                   
004133 01  WK-COM-PCT1                  PIC 9(02)V99.                   
004133 01  wK-COM-AMT                   PIC 9(09)V99.                   
004133 01  wK-COM-AMT1                  PIC S9(09)V99.                  
004133 01  WK-NET-AMT1                  PIC S9(09)V99.                  
004133 01  WK-TAX-AMT                   PIC S9(09)V99.                  
004133 01  WK-PPD-AMT                   PIC S9(09)V99.                  
004133 01  WK-D-P-AMT                   PIC S9(09)V99 VALUE +0.         
004133 01  WK-SPL-AMT                   PIC S9(09)V99 VALUE +0.         
004133 01  WK-ADJ-AMT                   PIC S9(09)V99 VALUE +0.         
004133 01  WK-NET-AMT2                  PIC S9(09)V99 VALUE +0.         
004133 01  WK-NET-AMT3                  PIC S9(09)V99 VALUE +0.         
AL-20  01  WK-NET-AMT-NOTAX             PIC S9(09)V99 VALUE +0.         
AL-20  01  WK-ADJ-NET-NT-AMT            PIC S9(09)V99 VALUE +0.         
004133*                                                                 
052200 01  WK-GROSS-AMT                 PIC S9(09)V99 comp VALUE +0.    
052200 01  WK-GROSS-AMT1                PIC S9(09)V99 comp VALUE +0.    
004133 01  WK-VAT-VAL                   PIC X(15).                      
004133 01  WK-SKIP-COMM                 PIC X(01).                      
004133 01  WK-CURR-VAL                  PIC X(05).                      
004133 01  WK-MAIN-EDN                  PIC X(01).                      
004133 01  WK-CC-DETAILS                PIC 9(16).                      
044400 01  WK-CC-DETAILS-R              REDEFINES WK-CC-DETAILS.        
004133     05 WK-CC-1-12                PIC X(12).                      
004133     05 WK-CC-13-16               PIC X(04).                      
004133 01  WK-WO-ISSUE.                                                 
004133     05 WK-TEMP-MM                PIC X(02).                      
004133     05 WK-TEMP-DD                PIC X(02).                      
004133     05 WK-TEMP-YY                PIC X(02).                      
044400 01  WK-TEMP-MMYY                 PIC 9(08).                      
004133 01  WK-TEMP-MMYY-R               REDEFINES WK-TEMP-MMYY.         
004133     05 WK-DUMMY                  PIC 9(04).                      
004133     05 WK-T1-MM                  PIC 9(02).                      
004133     05 WK-T1-YY                  PIC 9(02).                      
084700*                                                                 
004133 01  WK-EU-YEAR.                                                  
044400     05 WK-EU-YY1                 PIC X(02).                      
004133     05 WK-EU-YY2                 PIC X(02).                      
004133 01  WK-DATE1.                                                    
004133     05 WK-MM-1                   PIC 9(02).                      
004133     05 WK-DD-1                   PIC 9(02).                      
004133     05 WK-YY-1                   PIC 9(02).                      
004133 01 WK-PROC-REC                   PIC X(01).                      
004133 01 WK-HOLD-DATA.                                                 
044400    05 WK-HOLD-D5                 PIC X(50).                      
004133    05 WK-HOLD-D6                 PIC X(50).                      
004133    05 WK-HOLD-D7                 PIC X(50).                      
004133    05 WK-HOLD-D8                 PIC X(50).                      
004133    05 WK-HOLD-D9                 PIC X(50).                      
004133    05 WK-HOLD-GROSS-AMT          PIC S9(09)V99 comp VALUE +0.    
044400    05 WK-HOLD-NET-AMT            PIC S9(09)V99 comp VALUE +0.    
004133    05 WK-HOLD-TAX-AMT            PIC S9(09)V99 comp VALUE +0.    
004133    05 WK-HOLD-COM-AMT            PIC S9(09)V99 comp VALUE +0.    
AL-20     05 WK-HOLD-NET-AMT-NOTAX      PIC S9(09)V99 comp VALUE +0.    
004133 01 WK-WO-EDN-MAIN                PIC X(04).                      
004133 01 WK-MAIN-EDN-A                 PIC X(22).                      
004133 01 WK-MAIN-EDN-R                 REDEFINES WK-MAIN-EDN-A.        
004133    05 WK-HOLD-EDN-DESC           OCCURS 22 TIMES                 
004133                                  INDEXED BY WK-M1 PIC X(01).     
044400 01 WK-M2                         PIC 9(02).                      
004133 01 WK-MSG-CD3                    PIC X(02).                      
004133*AL 2013 01 LAST-INVC-NBR                 PIC 9(07).
       01 LAST-INVC-NBR                 PIC 9(09).
004133*     
       01  save-job-nbr-7               pic 9(10).
004133 01 WK-SAVE-PAY-MODE1             PIC X(50).                      
004133 01 WK-SAVE-PAY-MODE2             PIC X(50).                      
044400 01 WK-TEMP-ADV-NAME              PIC X(40).                      
004133 01 WK-PAY-MODE1.                                                 
004133    05 WK-PAY-L1                  PIC X(08).                      
004133    05 WK-PAY-L2                  PIC X(10).                      
004133    05 FILLER                     PIC X(32).                      
004133 01 WK-PAY-MODE2.                                                 
044400    05 WK-PAY-LIT1-1              PIC X(03).                      
004133    05 WK-PAY-VAL1-1              PIC X(15).                      
004133    05 WK-PAY-VAL1-2              PIC X(05).                      
004133    05 FILLER                     PIC X(01).                      
004133    05 WK-PAY-L3                  PIC X(03).                      
004133    05 FILLER                     PIC X(01).                      
004133    05 WK-PAY-VAL2-1              PIC 9(02).                      
004133    05 WK-PAY-L4                  PIC X(01).                      
044400    05 WK-PAY-VAL2-2              PIC 9(02).                      
004133    05 FILLER                     PIC X(01).                      
004133    05 WK-PAY-L5                  PIC X(04).                      
004133    05 FILLER                     PIC X(12).                      
004133*                                                                 
       01   wk-temp-z                  pic zzzzz9.
      *CTS - 06/05/07 CHANGE BEGINS                                     
004133 01  WK-TMP-LINE-CNT              PIC 9(02).                      
      *CTS - 06/05/07 CHANGE ENDS                                       
004133 01  WK-LINE-CNT                  PIC 9(02).                      
044400 01  WK-NT-INV-NBR                PIC 9(12).                      
004133 01  WK-NT-INV-NBR-R              REDEFINES WK-NT-INV-NBR.        
004133     05 WK-NT-INV-NBR-6           PIC X(09).                      
004133     05 FILLER                    PIC X(03).                      
004133 01  WK-HOLD-INV-NBR-6            PIC 9(09).                      
004133 01  WK-IHT-INVC-NBR              PIC 9(12).                      
084700*                                                                 
004133 01  WK-JOB-SEQ-CNT               PIC 9(04).                      
004133 01  WK-PUB                       PIC X(04).                      
004133 01  WK-DISP-GROSS                PIC X(01).                      
044400 01  WK-EU-MONTH                  PIC X(10).                      
004133 01  EURO-FIRST-TIME              PIC X(01).                      
004133 01  WK-EURO-PROCESS              PIC X(01).                      
004133 01  WK-LINE-NBR                  PIC X(02).                      
004133 01  WK-END-EDN                   PIC X(01).                      
004133*                                                                 
004133 01  WK-NO-AGY                    PIC X(01).                      
004133 01  WK-NO-AGY-TOT                PIC X(01).                      
       01  WK-NT-JOB-NBR                PIC 9(13).
044400 01  WK-NT-JOB-NBR-R              REDEFINES WK-NT-JOB-NBR.        
              05 WK-NT-JOB-NBR-7        PIC 9(10).
       01  WK-NT-SAVE-JOB-NBR           PIC 9(10). 
       01  WK-EU-JOB-NBR                PIC 9(13). 
AL-28  01  WK-PO-NBR                    PIC x(12).                      
004133*                                                                 
004133 01 WK-TOT-AMOUNTS.                                               
004133    05 WK-TTL-ALL-GRS-AMT         PIC S9(14)V99 VALUE +0.         
044400    05 WK-TTL-ALL-NET-AMT         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-COM-AMT         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-TAX-AMT         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-D-P-AMT         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-SPL-CHG         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-ADJ-AMT         PIC S9(14)V99 VALUE +0.         
004133    05 WK-TTL-ALL-CC-AMT          PIC S9(14)V99 VALUE +0.         
044400*                                                                 
004133 01  TTL-AGY-ADV-GROSS-AMT        PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-GROSS-AMT-ADJ    PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-GROSS-AMT-T      PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-GROSS-AMT-ADJ-T  PIC S9(12)V99 comp VALUE +0.    
AL-20  01  TTL-AGY-ADV-NET-AMT-NT-T     PIC S9(12)V99 comp VALUE +0.    
AL-20  01  TTL-AGY-ADV-NET-AMT-ADJ-NT-T PIC S9(12)V99 comp VALUE +0.    
AL-20  01  TTL-AGY-ADV-NET-AMT-ADJ-NT   PIC S9(12)V99 comp VALUE +0.    
AL-20  01  TTL-AGY-ADV-NET-AMT-NT       PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-NET-AMT          PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-NET-AMT-ADJ      PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-AGY-ADV-COM-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-AGY-ADV-TAX-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-AGY-ADV-D-P-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-AGY-ADV-SPL-CHG          PIC S9(12)V99 VALUE +0.         
044400 01  TTL-AGY-ADV-ADJ-AMT          PIC S9(12)V99 VALUE +0.         
AL-20  01  TTL-AGY-ADV-ADJ-AMT-NT       PIC S9(12)V99 VALUE +0.         
004133 01  TTL-AGY-ADV-CC-AMT           PIC S9(12)V99 comp VALUE +0.    
004133 01  TTL-TOT-AGY-COM-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-TOT-AGY-TAX-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-TOT-AGY-D-P-AMT          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-TOT-AGY-SPL-CHG          PIC S9(12)V99 VALUE +0.         
004133 01  TTL-TOT-AGY-ADJ-AMT          PIC S9(12)V99 VALUE +0.         
AL-20  01  TTL-TOT-AGY-ADJ-AMT-NT       PIC S9(12)V99 VALUE +0.         
004133 01  TTL-TOT-AGY-CC-AMT           PIC S9(12)V99 VALUE +0.         
044400*                                                                 
004133 01 TTL-TOT-AMOUNTS.                                              
044400    05 TTL-TTL-ALL-GRS-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-NET-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-COM-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-TAX-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-D-P-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-SPL-CHG        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-ADJ-AMT        PIC S9(14)V99 VALUE +0.         
004133    05 TTL-TTL-ALL-CC-AMT         PIC S9(14)V99 VALUE +0.         
044400*                                                                 
004133 01  WK-INVC-COUNT1               PIC 9(09) comp VALUE 0.         
004133 01  WK-CURR-VAL1                 PIC X(05).                      
004133 01  WK-EU-PG-FLG                 PIC X(01).                      
057900 01  EURO-R-MAX-TBL               PIC 9(02) VALUE 50.             
085000 01  EURO-PUB-LKUP-TABLE.                                         
056900     05 EURO-X-MAX                PIC 9(02) comp VALUE 0.         
085100     05 EU-PUB-EDN-LU   OCCURS 50 TIMES INDEXED BY EULU-X.        
003403        10 EU-PUB-CD              PIC X(04).                      
003403        10 EU-EDN-DTL             PIC X(01).                      
003403        10 EU-EDNS                PIC X(04) OCCURS 15 TIMES       
003403                                  INDEXED BY EU-1.                
084700*                                                                 
057900 01  EURO-AMT-MAX-TBL             PIC 9(03) VALUE 999.            
085000 01  EURO-REACH-AMTS-TABLE.                                       
056900     05 EURO-AMT-MAX              PIC 9(03) comp VALUE 0.         
085100     05 EU-AMTS-UNLOAD     OCCURS 999 TIMES INDEXED BY EU-R.      
003403        10 EU-DISC-SPL-ADJ-DESC   PIC X(50).                      
003403        10 EU-DISC-SPL-ADJ-AMT    PIC S9(09)V99 comp.             
AL-20         10 EU-DISC-SPL-ADJ-NOTAX  PIC S9(09)V99 comp.             
084700*                                                                 
      *CTS - 02/23/07 CHANGE ENDS
*dw   *.. needed by amzpcalc  
*dw    01  io-area.
*dw        05  IO-PARAMETERS.          COPY AMZWLADV.
*dw        05  io-wo.                  COPY AMZWLWO.
*dw        05  io-woc.                 COPY AMZWLWOC.
*dw        05  io-woe.                 COPY AMZWLWOE.
*dw    01  WK-XCPL-LIST.               COPY AMZWXCPL.
       01 wo-cc-auth-amt pic s9(8)v99 value 0.
       01 wo-cc-auth-code pic x.
       01 wo-cc-exp-date pic 9(8) value 0.
       01 wo-cc-number pic x(16).
*******al FORCE JOB NUMBER BACK TO 7-2 FORMAT FROM 10-3
       01  WS-JOB-NBR                  PIC 9(09). 
       01  WS-JOB-NBR-R                REDEFINES WS-JOB-NBR. 
           05  WS-JOB-NBR-7            PIC 9(07). 
           05  WS-JOB-NBR-2            PIC 9(02). 

       01  WS-JOB-NBR-NEW              PIC 9(18). 
       01  WS-JOB-NBR-NEW-R            REDEFINES WS-JOB-NBR-NEW.  
           05  WS-JOB-NBR-15           PIC 9(15). 
           05  WS-JOB-NBR-3            PIC 9(03). 
*******al                 
*******al FORCE JOB NUMBER BACK TO 7-2 FORMAT FROM 10-3
       01  WS-INV-NBR                  PIC 9(12). 
       01  WS-INV-NBR-R                REDEFINES WS-INV-NBR. 
           05  WS-INV-NBR-9            PIC 9(09). 
           05  WS-INV-NBR-3            PIC 9(03). 

       01  WS-INV-NBR-NEW              PIC 9(18). 
       01  WS-INV-NBR-NEW-R            REDEFINES WS-INV-NBR-NEW.  
           05  WS-INV-NBR-15           PIC 9(15). 
           05  WS-INV-NBR-3            PIC 9(03). 
*******al                 
dw-30  01  wk-today-date               pic 9(08).

       01  wk-ad-posn                  pic x(04).
           88  digital-posn            value 'RPRE'
                                             'RPHW'.
                                             
       01  wk-upsell-clas-edtn         pic x(04).
           88  upsell-edition          value 'AFC ',
                                             'BLD ',
                                             'CAN ',
                                             'DIV ',
                                             'FEEB',
                                             'FEJ ',
                                             'FEJB',
                                             'FJTR',
                                             'INYT',
                                             'JSEB',
                                             'MON ',
                                             'MSL ',
                                             'PRIB',
                                             'REF ',
                                             'RESB',
                                             'SOCB',
                                             'TTR ',
                                             'WEB ',.

       01  wk-upsell-display           pic x(04).
           88  upsell-section          value 'GOLD',
                                             'PLAT',
                                             'GOLN',
                                             'GOTR',
                                             'PLTN',
                                             'PLTR'
                                             'GOTT',. 
                                             
       01  wk-upsell-clas-posn         pic x(04).
           88  upsell-ad-posn          value 'ADNW',
                                             'ADNY',
                                             'APNW',
                                             'APNY',
                                             'ASNW',
                                             'ASNY',
                                             'AU1W',
                                             'AU2W',
                                             'AU3W',
                                             'AU4W',
                                             'AU5W',
                                             'AUTO',
                                             'BDNW',
                                             'BO1W',
                                             'BO2W',
                                             'BO3W',
                                             'BO4W',
                                             'BO5W',
                                             'BOLD',
                                             'BPNW',
                                             'BSNW',
                                             'CAN1',
                                             'CAN2',
                                             'CAN4',
                                             'CDNW',
                                             'CPNW',
                                             'CR1W',
                                             'CR2W',
                                             'CR3W',
                                             'CR4W',
                                             'CR5W',
                                             'CSNW',
                                             'DDNW',
                                             'DN1W',
                                             'DPNW',
                                             'DSNW',
                                             'FEEB',
                                             'FEJB',
                                             'HDNW',
                                             'HDNY',
                                             'HPNW',
                                             'HPNY',
                                             'HSNW',
                                             'HSNY',
                                             'HWCE',
                                             'HWCS',
                                             'HWDV',
                                             'HWFE',
                                             'HWGL',
                                             'HWMC',
                                             'HWMN',
                                             'HWPL',
                                             'INYT',
                                             'JSEB',
                                             'PRIB',
                                             'RDNW',
                                             'RDNY',
                                             'RESB',
                                             'RPNW',
                                             'RPNY',
                                             'RR1W',
                                             'RR2W',
                                             'RR3W',
                                             'RR4W',
                                             'RR5W',
                                             'RRZ1',
                                             'RRZ2',
                                             'RRZ3',
                                             'RRZ4',
                                             'RRZ5',
                                             'RSNW',
                                             'RSNY',
                                             'SM1W',
                                             'SM2W',
                                             'SM3W',
                                             'SM4W',
                                             'SM5W',
                                             'SOCB',
                                             'TCOR',
                                             'TDNW',
                                             'TDNY',
                                             'TGNW',
                                             'TGNY',
                                             'TGOL',
                                             'TSNW',
                                             'TSNY',
                                             'TTR ',
                                             'TTRA',
                                             'TTRD',
                                             'TTRF',.
                                             
125100 01  FILLER                      PIC X(28) VALUE                  
125200                                'WORKING STORAGE ENDS HERE.'.     
125300/                                                                 
      *CTS - 02/05/07 CHANGE BEGINS                                     
      ********************************************************          
      *   special call to call working storage variables for *          
      *   Paid Flag handling                                 *          
      *   DATE: 02/05/07                                     *          
      ********************************************************          
       COPY AMZWEURO.                                                   
      *CTS - 02/05/07 CHANGE ENDS                                       
125400 PROCEDURE DIVISION.                                              
125500*------------------*                                              
125600                                                                  
125700 MAIN SECTION.                                                    
126000*------------*                                                    
126100                                                                  
      *CTS - 02/05/07 CHANGE BEGINS                                     
      ********************************************************          
      *   special call to build table for Paid Flag handling *          
      *   DATE: 02/05/07                                     *          
      ********************************************************          
       COPY AMZPEURO.                                                   
      *CTS - 02/05/07 CHANGE ENDS                
      *al-33
AL-01A
           MOVE FUNCTION CURRENT-DATE TO Y2K-DATE-HOLD.
           MOVE WS-NUMERIC-YEAR TO  WS-NUMERIC-YEAR-2.
           MOVE WS-NUMERIC-MONTH TO WS-NUMERIC-MONTH-2.
AL-01A      
           move 'G' to wk-eu-pg-flg.
           move spaces to wk-curr-val1.
           initialize hdr-lines
                      footer-lines
                      dtl-lines.
           
126200     MOVE WHEN-COMPILED          TO WK-COMP-DATE.                 
           DISPLAY ' '.                                                 
           DISPLAY 'PRGID: Afbprint - V.M. 7.1'                   
           DISPLAY '       This program creates following files'.       
           DISPLAY ' '.                                                 
           DISPLAY 'Compiled Date: ' WK-COMP-HOUR                       
                           ' Time: ' WK-COMP-DAY.                       
           DISPLAY ' '.                                                 
126400                                                                  
126500     CALL 'JSTART' USING IO-PKT IO-PKT IO-PKT.                    
AL-03      ACCEPT WS-DATE-YYMMDD       FROM DATE.    
dw-30      PERFORM GET-DATE.    
dw-30      move utl-edit-date          to wk-today-date
dw-30      display '===== today date = ' wk-today-date
126700     OPEN OUTPUT INVOICE-FILE                                     
126700                 SETTLMT-FILE                                     
126700                 SETTLMTI-FILE                                    
126700                 SETTLMTA-FILE                                    
FXC---                 CC-REPORT                                        
FXC---                 CC-REPORTI                                       
FXC---                 CC-REPORTA                                       
FXC---                 LOCKBOX-FILE                                     
                       LOCKBOXI-FILE                                    
                       LOCKBOXA-FILE                                    
FXC---                 LOCKBOXN-FILE
AL-01A                 CONTROL-FILE

                                                                        
127100     SORT SSL-FILE                                                
127200       ON ASCENDING KEY  
                SSL-BTA-JXRF    
                SsL-AGY-ACCT-NBR
     ***        ssl-agy-key
                ssl-adv-key
127400          SSL-SORT-AGY                                            
127500          SSL-SORT-ADV-PAR                                        
127600          SSL-SORT-ADV                                            
127700          SSL-SORT-PUB-CNT                                        
127800          SSL-SORT-WO                                             
127900        INPUT PROCEDURE IS SORT-IN                                
128000       OUTPUT PROCEDURE IS SORT-OUT.                              
128100
AL-01A**** 
AL-01A      MOVE Y2K-DATE-HOLD         TO CTL-REC-RUN.
AL-01A      MOVE TTL-TTL-NET-AMT       TO CTL-REC-NET.
            subtract ppd-cc-amt        from ctl-rec-net
AL-01A      MOVE TTL-TTL-GROSS-AMT     TO CTL-REC-GROSS.
AL-01A      MOVE '0'                   TO CTL-REC-AGING.
*L-01A**      MOVE WK-INVC-DATE-F      TO CTL-REC-FROM.
*L-01A**      MOVE WK-INVC-DATE-T      TO CTL-REC-TO.
AL-01A      MOVE WK-INVC-RUN           TO CTL-REC-TYPE.
AL-01A      MOVE WK-INVC-ORG           TO CTL-REC-org.
            move TTL-INVC-NBR          to ctl-rec-cnt
AL-07      MOVE PPD-CC-AMT             TO CTL-REC-cc-amt                        
            

AL-01A      WRITE CONTROL-REC.
AL-01A
128200     CLOSE INVOICE-FILE                                           
FXC---           LOCKBOX-FILE                                           
                 LOCKBOXI-FILE                                          
                 LOCKBOXA-FILE                                          
FXC---           SETTLMT-FILE                                           
FXC---           SETTLMTI-FILE                                          
FXC---           SETTLMTA-FILE                                          
FXC---           CC-REPORT                                              
FXC---           CC-REPORTI                                             
FXC---           CC-REPORTA                                             
FXC---           LOCKBOXN-FILE
AL-01A           CONTROL-FILE
                                                                        
DW-08      IF  WK-TTL-CC-COUNT-N not > 0                                
DW-08          OPEN OUTPUT LOCKBOXN-FILE                                
DW-08          CLOSE       LOCKBOXN-FILE.                               
      *CTS - 07/02/07 CHANGE BEGINS                                     
CTS-02     IF  WK-TTL-CC-COUNT-I NOT > 0                                
CTS-02         OPEN OUTPUT LOCKBOXI-FILE                                
CTS-02         OPEN OUTPUT SETTLMTI-FILE                                
CTS-02         OPEN OUTPUT CC-REPORTI                                   
CTS-02         CLOSE       LOCKBOXI-FILE                                
CTS-02         CLOSE       SETTLMTI-FILE                                
CTS-02         CLOSE       CC-REPORTI.                                  
CTS-02     IF  WK-TTL-CC-COUNT-A NOT > 0                                
CTS-02         OPEN OUTPUT LOCKBOXA-FILE                                
CTS-02         OPEN OUTPUT SETTLMTA-FILE                                
CTS-02         OPEN OUTPUT CC-REPORTA                                   
CTS-02         CLOSE       LOCKBOXA-FILE                                
CTS-02         CLOSE       SETTLMTA-FILE                                
CTS-02         CLOSE       CC-REPORTA.                                  
      *CTS - 07/02/07 CHANGE ENDS                                       
                                                                        
AL-06      COMPUTE WS-DECIMAL  = TTL-GB-NBR.                            
128600     CALL 'JEND' USING IO-PKT IO-PKT IO-PKT.                      

           move 0                      to return-code                                                                  
128800     GO TO EXIT-MOD.                                              
128900                                                                  
129000 EXIT-MOD. COPY AMZPXPRG.                                         
129100/                                                                 
129300 SORT-OUT SECTION.                                                
129400*----------------*                                                
                                                                        
129700     MOVE LOW-VALUES             TO TOT-TABLE.                    
AL-08      MOVE LOW-VALUES             TO GOLD-TABLE.                   
AL-15      MOVE LOW-VALUES             TO PLAT-TABLE
                                          mega-table          
129800     PERFORM CHECK-SORT.                                          
129900                                                                  
130100     CALL 'AMSPRNT' USING PRT-INIT   HDRCRD-TBL.                  
130200     CALL 'AMSPRNT' USING PRT-OPENDD WK-SYS031-DD.                
130400                                                                  
130500     MOVE 1                      TO PRT-SKIP.                     
130600     MOVE SPACES                 TO DTL-LINES.                    
130700     MOVE 'OPN'                  TO SSL-FILE-STATUS.              
130800                                                                  

130900     OPEN OUTPUT IAR-FILE MAG-TAPE-FILE NEWT-OUT                  
FXC---                 CCWORK-FILE.                                     
                                                                        
131000     IF  WK-IAR-STATUS NOT = '00'                                 
131100         CALL 'AMSABRT' USING IN19.                               
131200                                                                  
131300     MOVE SPACES                 TO TB10-REC                      
131400                                    TB11-REC.                     
131500     MOVE '10'                   TO TB10-ACTION-CODE.             
131600     MOVE '11'                   TO TB11-ACTION-CODE.             
131700     MOVE '1'                    TO TB10-ENTITY.                  
131800     MOVE '1'                    TO TB11-ENTITY.                  
132000     MOVE 'D'                    TO MSG-TYPE.                     
132100     MOVE 'SEP1'                 TO MSG-NBR.                      
                                                                        
132200     PERFORM CALL-GIT-MSG.                                        
132300                                                                  
132400     IF  STATUS-CODE = ZEROS                                      
132500         MOVE MSG-TEXT           TO WK-SEP-LIT                    
132600     ELSE                                                         
132700         MOVE '111'              TO WK-SEP-LIT.                   
132800                                                                  
AL-06      MOVE 'N'                    TO PROCESS-GROUP-CODE.           
132900     PERFORM RETURN-SSL-READ-WO.                                  
132910     PERFORM GET-INVC-NBR.                                        
133000                                                                  
           INITIALIZE TTL-TOT-AMOUNTS.                                  
133300                                                                  
133400     PERFORM PROCESS-GROUPS                                       
133500       UNTIL SSL-FILE-STATUS = 'EOF'.                             
133600                                                                  
           PERFORM PROCESS-ALL-TOTALS.                                  
133700     PERFORM PROCESS-TOTAL-RECAP.                                 
133800                                                                  
133900     CLOSE IAR-FILE MAG-TAPE-FILE NEWT-OUT                        
FXC---           CCWORK-FILE.                                           
                                                                        
134000     IF  WK-IAR-STATUS NOT = '00'                                 
134100         CALL 'AMSABRT' USING IN20.                               
134200                                                                  
134300     CALL 'AMSPRNT' USING PRT-CLOSE IO-PKT.                       
                                                                        
134400     GO TO SORT-OUT-EXIT.                                         
134600                                                                  
134700 PROCESS-GROUPS.                                                  
134800*--------------*              
           display '****************************************'.                                             

134900*>   Process all Adv/Agy types together                           
135100     MOVE SSL-AGY-TYPE           TO SAVE-TYPE.                    
135200       
            display 'PROCESS-GROUPS ' SAVE-TYPE                                    

135410     PERFORM GET-INVC-MSGS.                                       
135500                                                                  
135600     PERFORM PROCESS-INVOICES                                     
135700       UNTIL SSL-AGY-TYPE NOT =  SAVE-TYPE                        
135800          OR SSL-FILE-STATUS  = 'EOF'.                            
135900                                                                  
136300 PROCESS-INVOICES.                                                
136400*----------------*                                                
136500*>   Process all Invoices for one Bta or Sta                      
AL-06      DISPLAY 'start PROCESS INVOICES'.                                  
136600                                                              
136700     IF  SSL-AGY-ACCT-NBR NOT =  SAVE-AGY-ACCT-NBR                
136800     OR  SSL-NAT-AGY          = 'Y'                               
136900         ADD 1                   TO WK-INVC-NBR                   
*****************                         TTL-INVC-NBR
           end-if.                  
                                                                                                            
137200     IF  SSL-AGY-ACCT-NBR NOT = SAVE-AGY-ACCT-NBR                 
137300         ADD 1                   TO WK-INVC-COUNT                 
137400         MOVE ZEROS              TO WK-PAGE-CTR                   
137500         MOVE ZEROS              TO TTL-AGY-GROSS-AMT             
137600                                    TTL-AGY-GROSS-AMT-ADJ         
137700                                    TTL-AGY-NET-AMT               
137800                                    TTL-AGY-NET-AMT-ADJ           
137800                                    TTL-TOT-AGY-COM-AMT           
137900                                    TTL-TOT-AGY-TAX-AMT           
137700                                    TTL-TOT-AGY-D-P-AMT           
137800                                    TTL-TOT-AGY-SPL-CHG           
004133                                    TTL-TOT-AGY-CC-AMT            
AL-20                                     TTL-TOT-AGY-ADJ-AMT-NT        
137900                                    TTL-TOT-AGY-ADJ-AMT.          
137900                                                                  
137700     IF  SSL-AGY-ACCT-NBR NOT = SAVE-AGY-ACCT-NBR OR              
137800         SSL-ADV-PAR-NBR  NOT = SAVE-ADV-PAR-NBR                  
137800         ADD 1                   TO WK-INVC-COUNT1                
137800         MOVE ZEROS              TO TTL-AGY-ADV-GROSS-AMT         
137900                                    TTL-AGY-ADV-GROSS-AMT-ADJ     
137900                                    TTL-AGY-ADV-GROSS-AMT-T       
137900                                    TTL-AGY-ADV-GROSS-AMT-ADJ-T   
AL-20                                     TTL-AGY-ADV-NET-AMT-NT        
AL-20                                     TTL-AGY-ADV-NET-AMT-ADJ-NT    
AL-20                                     TTL-AGY-ADV-NET-AMT-ADJ-NT-T  
AL-20                                     TTL-AGY-ADV-NET-AMT-NT-T      
AL-20                                     DTL-NET-NOTAX                 
137700                                    TTL-AGY-ADV-COM-AMT           
137800                                    TTL-AGY-ADV-TAX-AMT           
137800                                    TTL-AGY-ADV-CC-AMT            
137700                                    TTL-AGY-ADV-D-P-AMT           
137800                                    TTL-AGY-ADV-SPL-CHG           
137800                                    TTL-AGY-ADV-ADJ-AMT           
137800                                    TTL-AGY-ADV-ADJ-AMT-NT        
137800                                    TTL-AGY-ADV-NET-AMT           
137900                                    TTL-AGY-ADV-NET-AMT-ADJ.      
137700     
           display 'SSL-S-INV-6 ' SSL-S-INV-6   
AL-20      MOVE SSL-S-INV-6            TO SAVE-INVNBR-6.                19771001

138000     MOVE SSL-AGY-ACCT-NBR       TO SAVE-AGY-ACCT-NBR.            
138100     MOVE SSL-ADV-PAR-NBR        TO SAVE-ADV-PAR-NBR.             
138200     MOVE SSL-ACCT-NBR           TO SAVE-ACCT-NBR.                
138300     MOVE SSL-NAT-AGY            TO SAVE-NAT-AGY.                 
138400     MOVE SSL-CATEGORY           TO SAVE-CATEGORY.         
AL-06      MOVE SSL-WO-GROUP-CODE      TO SAVE-GROUP-CODE.  
           MOVE SSL-CNT-KEY            TO SAVE-CNT-KEY.                 
********   MOVE SSL-pub                TO SAVE-pub                      

*******    MOVE SSL-job-nbr-7          TO SAVE-job-nbr-7        

137900     MOVE 'Y'                    TO WK-PROC-HDR-FLG.              
137700     MOVE 'N'                    TO WK-NO-AGY.                    
137700     INITIALIZE WK-HOLD-DATA.                                     
138000     MOVE ZEROS                  TO WK-JOB-SEQ-CNT.               
138000     MOVE SPACES                 TO WK-PUB.                       
138100     MOVE ZEROS                  TO WK-LINE-CNT.                  
138200     MOVE SPACES                 TO WK-EURO-PROCESS               
138300                                    WK-PROC-REC                   
138400                                    WK-SAVE-PAY-MODE1             
137800                                    WK-SAVE-PAY-MODE2             
137900                                    WK-PAY-MODE1                  
137700                                    WK-PAY-MODE2.                 
138500                                                                  
138600     MOVE 'Y'                    TO WK-FIRST-TIME.                
AL-20      MOVE SPACES                 TO WK-BARTER-AD.                 
138700     MOVE SPACE                  TO WK-READY-TOTAL                
138800                                    WK-MORE-CNT.                  
138900     ADD 1                       TO TTL-ACCT-NBR.                 
139000                                                                  
139100     IF  SSL-ADV-PAR-NBR NOT = SSL-AGY-ACCT-NBR                   
139200     AND SSL-ADV-PAR-NBR NOT = SSL-ACCT-NBR                       
139300         ADD 1                   TO TTL-ACCT-NBR.                 
139400                                                                  
39700                                                                   
139800     MOVE ZEROS                  TO WK-CNT-COUNT.                 
139900                   
AL-06      SET GCT-X                   TO 1.                            
AL-06      MOVE SPACES                 TO TEMP-GCT-ENTRY,               
AL-06                                     HOLD-SSL-REC,                 
AL-06                                     GROUP-CONTROL-TABLE.          
AL-06      MOVE ZEROS                  TO WO-INVC-NBR,                  
AL-06      MOVE ZEROS                  TO GCT-MAX-ENTRY.    
********   MOVE SSL-job-nbr-7          TO SAVE-job-nbr-7                
145300     MOVE ZEROS                  TO WK-PFM-WO-ISSUE.              
145400     MOVE LOW-VALUES             TO WK-PFM-AREA (1)               
145500                                    WK-PFM-AREA (2).              

145300     MOVE ZEROS                  TO WK-PFM-WO-ISSUE.              
145400     MOVE LOW-VALUES             TO WK-PFM-AREA (1)               
145500                                    WK-PFM-AREA (2).              
AL-06      display SSL-S-INV-6   ' '                                    19981101
AL-06              SAVE-INVNBR-6.                                       19982001

AL-06      display SSL-job-nbr-7  ' '                  
AL-06              SAVE-job-nbr-7                                       

140000     PERFORM PROCESS-DETAILS-PER-CNT                              
140100       UNTIL  SSL-AGY-TYPE     NOT =  SAVE-TYPE    
140200          OR  SSL-AGY-ACCT-NBR NOT =  SAVE-AGY-ACCT-NBR           
140300          OR (SSL-ADV-PAR-NBR  NOT =  SAVE-ADV-PAR-NBR            
140400         AND  SAVE-ADV-PAR-NBR NOT =  SAVE-ACCT-NBR)              
140500          OR (SSL-ACCT-NBR     NOT =  SAVE-ACCT-NBR               
140600         AND  SSL-CATEGORY     NOT =  LOW-VALUE)                  
140700          OR  SSL-CATEGORY     NOT =  SAVE-CATEGORY   
140800          OR  SSL-FILE-STATUS      = 'EOF'.

141000     IF  SAVE-CATEGORY = LOW-VALUE       
               display 'if# 1'
141100         PERFORM PROCESS-MISC-TOTAL                               
141200     ELSE    
                  display 'if# 2'
141300            MOVE 'Y'             TO WK-READY-TOTAL                
141500            PERFORM PROCESS-CNT-PFM-PRINT.                        
141600                                                                  
141400     MOVE 'N'               TO WK-NO-AGY-TOT.                     
141600     IF  SSL-AGY-ACCT-NBR   NOT =  SAVE-AGY-ACCT-NBR              
141300     OR  SSL-ADV-PAR-NBR    NOT =  SAVE-ADV-PAR-NBR               
141400     OR  SAVE-NAT-AGY           = 'Y'                             
141500     OR  SSL-FILE-STATUS        = 'EOF'                           
141600     OR  SSL-AGY-TYPE       NOT =  SAVE-TYPE  
********   OR  SSL-S-INV-6         NOT =  SAVE-INVNBR-6                 20335101
              display 'if# 3'
141300         PERFORM PROCESS-AGY-ADV-TOTAL-NEW-TECH                   
141400         IF WK-NO-AGY = 'Y'                                       
                  display 'if# 4'
141500            PERFORM PROCESS-AGY-TOTAL                             
141600            MOVE 'Y'        TO WK-NO-AGY-TOT                      
141300         END-IF                                                   
141400     END-IF.                                                      
141700     IF  (SSL-AGY-ACCT-NBR  NOT =  SAVE-AGY-ACCT-NBR              
141800     OR   SSL-FILE-STATUS       = 'EOF'            
141900     OR   SSL-AGY-TYPE      NOT =  SAVE-TYPE)                     
142000     AND (SAVE-AGY-ACCT-NBR NOT =  SAVE-ADV-PAR-NBR               
142100     OR  (SAVE-AGY-ACCT-NBR     =  SAVE-ADV-PAR-NBR               
142200     AND  SAVE-ADV-PAR-NBR  NOT =  SAVE-ACCT-NBR)                 
142300     OR  (SAVE-AGY-ACCT-NBR     =  SAVE-ADV-PAR-NBR               
142400     AND  SAVE-AGY-ACCT-NBR     =  SAVE-ACCT-NBR                  
142500     AND  WK-CNT-COUNT          >  1))      
               display 'if# 5'
142400         IF WK-NO-AGY-TOT NOT = 'Y'                               
                   display 'if# 6'
142500            PERFORM PROCESS-AGY-TOTAL                             
141300         END-IF                                                   
142600     END-IF.                                                      
143300                                                                  
143400     PERFORM PROCESS-INVOICE-TOTAL.                               
AL-06      MOVE SPACES TO DETAIL-DATA.                                  

AL-06      DISPLAY 'end   PROCESS INVOICES'.                                  

AL-06  PROCESS-GB-INFO.                                                 
AL-06 *----------------*   
           display 'PROCESS-GB-INFO ' GCT-MAX-ENTRY
 dwww      move 'Y'                    to process-group-code         
AL-06      PERFORM PROCESS-DETAILS THRU                                 
AL-06              PROCESS-DETAILS-EXIT UNTIL                           
AL-06              GCT-X > GCT-MAX-ENTRY                                
********dww    OR  GCT-WO-GROUP-CODE(GCT-X) NOT = WK-SAVE-GROUP-CODE.   
AL-06      SET GCT-MAX-ENTRY-1        TO GCT-X.                         
AL-06      DISPLAY 'process gb info: '                                  
AL-06              GCT-MAX-ENTRY ' '                                    
AL-06              GCT-WO-GROUP-CODE(GCT-X) ' '                         
AL-06              GCT-max-entry-1 ' '.                                 
AL-06      IF GCT-X > GCT-MAX-ENTRY                                     
AL-06         display 'gct > max'                                       
AL-06         SET GCT-X               TO 1                              
AL-06         PERFORM PRINT-GROUP-BUY-INFO                              
AL-06         MOVE HOLD-SSL-REC       TO SSL-REC                        
AL-06         MOVE 'N'                TO PROCESS-GROUP-CODE             
AL-06         SET GCT-X               TO 1                              
AL-06         MOVE ZEROS              TO GCT-MAX-ENTRY                  
AL-06         MOVE ZEROS              TO WK-TEMP-GROUP-RATE             
AL-06         MOVE 'Y'                TO WK-READY-TOTAL                 
AL-06         PERFORM PROCESS-CNT-PFM-PRINT                             
AL-06      ELSE                                                         
AL-06         SET GCT-HOLD-ENTRY      TO GCT-X                          
AL-06         SET GCT-X               TO 1                              
AL-06         PERFORM PRINT-GROUP-BUY-INFO                              
AL-06         MOVE ZEROS              TO WK-TEMP-GROUP-RATE             
AL-06         SET GCT-X               TO GCT-HOLD-ENTRY                 
AL-06         MOVE GCT-WO-GROUP-CODE(GCT-X) TO WK-SAVE-GROUP-CODE.      
AL-06                                                                   
AL-06                                                                   
AL-06  PROCESS-GB-INFO-EXIT. EXIT.                                      
AL-06 *---------------------------*                                     
AL-06                                                                   
AL-06  PRINT-GROUP-BUY-INFO.                                            
AL-06 *--------------------*                                            
AL-06      display 'PRINT-GROUP-BUY-INFO '                             
                        GCT-MAX-ENTRY 
AL-06      MOVE ZEROS                TO WK-GRP-INV-AMT,                 
AL-06                                   WK-GRP-GRS-AMT.                 
AL-06      MOVE SPACES               TO SEL4-REC.                       
AL-06      PERFORM SUM-GROUP-DETAILS UNTIL GCT-X > GCT-MAX-ENTRY        
AL-06                                OR    GCT-X > GCT-MAX-ENTRY-1.     
AL-06      MOVE  SPACES              TO DTL-LINE-A.                     
AL-06      PERFORM PROCESS-GROUP-BUY.                                   
AL-06                                                                   
AL-06                                                                   
AL-06  SUM-GROUP-DETAILS.                                               
AL-06 *--------------------*                                            
AL-06      IF GCT-WO-GROUP-CODE(GCT-X) = WK-SAVE-GROUP-CODE             
AL-06         COMPUTE WK-GRP-INV-AMT = WK-GRP-INV-AMT                   
AL-06                                + GCT-WO-INVC-AMT(GCT-X)           
AL-06         COMPUTE WK-GRP-GRS-AMT = WK-GRP-GRS-AMT                   
AL-06                                + GCT-WO-GROSS-AMT(GCT-X).  

AL-06      IF GCT-WO-GROUP-CODE(GCT-X) = WK-SAVE-GROUP-CODE             
AL-06         IF GCT-PUB(GCT-X) = 'NYT'                                 
AL-06            display 'found nyt: ' gct-wo-group-code(gct-x)         
AL-06            MOVE GCT-SEL-REC(GCT-X)  TO SEL4-REC.                  
AL-06      SET GCT-X              UP BY 1.                              

143700 PROCESS-DETAILS-PER-CNT.                                         
143800*-----------------------*                                         
             display 'start PROCESS-DETAILS-PER-CNT '
143900*>   Process all details for each contract                        
144000                                                                  
144100     IF  WK-FIRST-TIME = 'Y'                                      
144200         MOVE SPACE              TO WK-FIRST-TIME  
                 display 'if #1'
144300     ELSE                                                         
144400     IF  SAVE-CATEGORY NOT = LOW-VALUE   
                 display 'if #2'

144500         MOVE 'Y'                TO WK-MORE-CNT                   
144700         PERFORM PROCESS-CNT-PFM-PRINT                            
144800         PERFORM PROCESS-INVOICE-TOTAL.                           
144900                                                                  
145000     PERFORM PROCESS-HEADER.                                      
145100  
AL-06          display 'ssl job# ' SSL-job-nbr-7  ' '                  
AL-06                 'save job# ' SAVE-job-nbr-7    

           if  SSL-job-nbr-7 not = SAVE-job-nbr-7 
145200         MOVE SSL-job-nbr-7          TO SAVE-job-nbr-7   
           end-if
                        
           move space                  to edtn-table
           move zero                   to edtn-table-count
                        
AL-08      MOVE LOW-VALUES             TO GOLD-TABLE.                   
AL-08      MOVE SPACES                 TO WK-GOLD-IND.                  
AL-08      MOVE ZEROS                  TO GOLD-TABLE-COUNT.             
AL-08      MOVE ZEROS                  TO WK-GOLD-GROSS-AMT,            
AL-08                                     WK-GOLD-INVC-AMT,             
AL-08                                     WK-GOLD-BLEED-AMT            
AL-08                                     WK-GOLD-adj-AMT          
                                                                        
                                                                        
AL-15      MOVE LOW-VALUES             TO PLAT-TABLE.                   
AL-15      MOVE SPACES                 TO WK-PLAT-IND.                  
AL-15      MOVE ZEROS                  TO PLAT-TABLE-COUNT.             
AL-15      MOVE ZEROS                  TO WK-PLAT-GROSS-AMT,            
AL-15                                     WK-PLAT-INVC-AMT,             
AL-15                                     WK-PLAT-BLEED-AMT
                                          wk-plat-adj-amt

AL-15      MOVE LOW-VALUES             TO mega-TABLE.                   
           move 'N'                    to wk-mega-override
           move 0                      to wk-mega-job-nbr
                                          wk-mega-job-sum
AL-15      MOVE ZEROS                  TO mega-TABLE-COUNT.             
AL-15      MOVE ZEROS                  TO WK-mega-GROSS-AMT,            
AL-15                                     WK-mega-INVC-AMT,             
AL-15                                     WK-mega-BLEED-AMT
                                          wk-mega-adj-amt
145200     MOVE SSL-pub                TO SAVE-pub                      
                                                                        
145700     PERFORM PROCESS-DETAILS                                      
AL-06              THRU PROCESS-DETAILS-EXIT                            
145800       UNTIL  SSL-AGY-TYPE     NOT =  SAVE-TYPE   
145900          OR  SSL-AGY-ACCT-NBR NOT =  SAVE-AGY-ACCT-NBR           
146000          OR (SSL-ADV-PAR-NBR  NOT =  SAVE-ADV-PAR-NBR            
146100         AND  SAVE-ADV-PAR-NBR NOT =  SAVE-ACCT-NBR)              
146200          OR (SSL-ACCT-NBR     NOT =  SAVE-ACCT-NBR               
146300         AND  SSL-CATEGORY     NOT =  LOW-VALUE)                  
146400          OR  SSL-CATEGORY     NOT =  SAVE-CATEGORY               
                OR SSL-pub           NOT =  SAVE-pub                    
                or ssl-job-nbr-7     not = save-job-nbr-7
146700          OR  SSL-FILE-STATUS      = 'EOF'.                       
146800   
AL-06        DISPLAY 'DONE WITH PROCESS-DETAILS'.
              
AL-08        display 'GOLD-TABLE-COUNT ' gOLD-TABLE-COUNT ' '
             display 'GOLD-TABLE ' GOLD-TABLE 
AL-08        display 'PLAT-TABLE-COUNT ' PLAT-TABLE-COUNT ' '
             display 'PLAT-TABLE ' PLAT-TABLE 
AL-08        display 'EDTN-TABLE-COUNT ' EDTN-TABLE-COUNT ' '
             display 'EDTN-TABLE ' EDTN-TABLE
AL-08        display 'MEGA-TABLE-COUNT ' MEGA-TABLE-COUNT ' '
             display 'MEGA-TABLE ' MEGA-TABLE 
             display 'wk-mega-job-sum ' wk-mega-job-sum
             
           IF  mega-TABLE-COUNT > 0      
           and wk-mega-job-sum = 0
               set mega-x              to 01
               move mega-NBR(mega-X)   TO wk-mega-JOB-NBR                
               move 'Y'                to wk-mega-override
               display 'writing mega line ' wk-mega-JOB-NBR ' ' 
                        'mega sum ' wk-mega-job-sum
               Perform PROCESS-DETAILS 
***********    Perform Write-Mega-Line
***********        varying mega-x from 1 by 1
***********        until mega-x > mega-table-count
           end-if                                                              
             
AL-08      IF GOLD-TABLE-COUNT > 0                                      
AL-08         DISPLAY 'PROCESS-GOLD: '                  
AL-08         MOVE 'Y'         TO WK-GOLD-IND                           
AL-16         MOVE ' '         TO WK-PLAT-IND                           
AL-08         SET GOLD-X       TO 1   
AL-08         MOVE GOLD-NBR(GOLD-X) TO WO-JOB-NBR                     
AL-08         CALL 'GIT' USING JNBR-WO WO-REC IO-PKT                  
AL-08         IF  STATUS-CODE NOT = ZEROS         
               move 1                  to return-code
AL-08               CALL 'AMSABRT' USING IN40                           
AL-08         ELSE                                  
                    display 'moving to wo'
AL-08               MOVE GOLD-GROSS-AMT(GOLD-X) TO WO-GROSS-AMT         
AL-08               MOVE GOLD-BLEED-AMT(GOLD-X) TO WO-BLEED-AMT         
AL-08               MOVE GOLD-INVC-AMT (GOLD-X) TO WO-INVC-AMT          
AL-08               MOVE GOLD-adj-AMT (GOLD-X)  TO WO-adj-AMT   
**************      MOVE WK-gold-adj-AMT  TO WO-adj-AMT   
                    display 'gold-order'
                    display 'WO-GROSS-AMT  ' WO-GROSS-AMT  
                            'WO-BLEED-AMT  ' WO-BLEED-AMT  
                            'WO-INVC-AMT   ' WO-INVC-AMT   
                            'WO-adj-AMT   '  WO-adj-AMT   
                    
               end-if
               Perform PROCESS-GROUP-BUY
           end-if.
AL-15                                                                   
AL-15      IF PLAT-TABLE-COUNT > 0                                      
AL-15         DISPLAY 'PROCESS-PLAT: ' PLAT-TABLE-COUNT                 
AL-15         MOVE 'Y'         TO WK-PLAT-IND                           
AL-16         MOVE ' '         TO WK-GOLD-IND                           
AL-08         SET PLAT-X       TO 1   
AL-08         MOVE PLAT-NBR(PLAT-X) TO WO-JOB-NBR                     
AL-08         CALL 'GIT' USING JNBR-WO WO-REC IO-PKT                  
AL-08         IF  STATUS-CODE NOT = ZEROS                 
               move 1                  to return-code
AL-08               CALL 'AMSABRT' USING IN40                           
AL-08         ELSE                                  
                    display 'moving to wo'
AL-08               MOVE PLAT-GROSS-AMT(PLAT-X) TO WO-GROSS-AMT         
AL-08               MOVE PLAT-BLEED-AMT(PLAT-X) TO WO-BLEED-AMT         
AL-08               MOVE PLAT-INVC-AMT (PLAT-X) TO WO-INVC-AMT          
AL-08               MOVE PLAT-adj-AMT (PLAT-X)  TO WO-adj-AMT  
**************      MOVE WK-plat-adj-AMT  TO WO-adj-AMT  
                    display 'plat-order'
                    display 'WO-GROSS-AMT  ' WO-GROSS-AMT  
                            'WO-BLEED-AMT  ' WO-BLEED-AMT  
                            'WO-INVC-AMT   ' WO-INVC-AMT   
                            'WO-adj-AMT   '  WO-adj-AMT   
                    
               end-if
               Perform PROCESS-GROUP-BUY
           end-if.
           
           if  SSL-pub     NOT =  SAVE-pub
           OR  SSL-ACCT-NBR NOT =  SAVE-ACCT-NBR           
           OR  SSL-AGY-ACCT-NBR NOT =  SAVE-AGY-ACCT-NBR           
           OR  SSL-FILE-STATUS = 'EOF'                      
               Perform Pub-Total
           end-if.
           
           display 'end   PROCESS-DETAILS-PER-CNT'


147000 PROCESS-DETAILS.                                                 
147100*---------------*                                                 
            display 'PROCESS-DETAILS ' ssl-job-nbr-7    
            display 'bta-xrf ' ssl-bta-jxrf
           display 'WK-GOLD-IND ' WK-GOLD-IND ' '
                    'WK-PLAT-IND '  WK-PLAT-IND 
                                                             
147300     IF  WK-FIRST-LINE = 'N'                                      
147400         PERFORM WRITE-DETAIL-LINE.                               
147500                                                                  
147600     MOVE SPACE                  TO WK-MAG-TAPE-REC.              
147900     ADD 1                       TO TTL-WO-NBR.                   
           
148000                                                                  
148100*>   Item 10                                                      
148200                         
           IF WK-GOLD-IND = 'Y'                                    
           or WK-PLAT-IND = 'Y'
              next sentence
           else
              display 'git wo'
147700        PERFORM CALL-GIT-WO.   

AL-20      MOVE ZEROS                  TO WK-NET-AMT-NOTAX.                         
           add wo-invc-amt             to wk-mega-job-sum
           
           if  WK-GOLD-IND = space
           and wk-plat-ind = space
               display 'adding to pub total '  wo-invc-amt  ' '  
	                TTL-pub-invc-amt ' ' wo-job-nbr
               add wo-invc-amt         to TTL-pub-invc-amt    
               display 'adding to pub total '  wo-invc-amt  ' ' 
                        TTL-pub-invc-amt ' '
           end-if 
           

AL-08      DISPLAY 'WO: ' WO-JOB-NBR  
               display 'section ' wo-section ' ' 
                       'ad-posn ' wo-ad-posn ' '
                       'edition ' wo-edition
                   '='    WK-GOLD-IND                                   
                   '='    WK-PLAT-IND.                                  
AL-08            
           move 'EDTN'                 to cod-key
           move wo-pub                 to cod-code1
           move wo-edition             to cod-code2
           call 'GIT'                  using cod-file cod-rec io-pkt
           
           display 'edtn cod-key ' cod-key ' '
                              status-code ' '
                              COD-FLAGS
           display 'wk-mega-override ' wk-mega-override 
           if  status-code = 0
           and cod-flag8   = 'Y'
           and wo-invc-amt = 0
           and wk-mega-override = 'N'
               Perform Add-Megaplex-Table               
AL-08           GO TO BYPASS-GROUP-INFO
           end-if
               
AL-15      IF (WK-GOLD-IND NOT = 'Y' AND WK-PLAT-IND NOT = 'Y')         
               MOVE WO-SECTION         TO WK-UPSELL-DISPLAY
               move wo-ad-posn         to wk-upsell-clas-posn
               move wo-edition         to wk-upsell-clas-edtn
L-15        IF  upsell-section
*********    or  upsell-ad-posn
             or  upsell-edition             
AL-15           PERFORM ADD-PACKAGE-TABLE                               
AL-08           GO TO BYPASS-GROUP-INFO                                 
             end-if
           end-if
           
     ************* ======    
             if  WK-GOLD-IND = 'Y' 
             or  WK-PLAT-IND = 'Y'   
                 move 0                to wo-ad-quantity
*******          move wo-section       to wo-edition
             end-if
AL-08                      
             display 'gross '  WO-GROSS-AMT ' '
                     'net '   wo-invc-amt ' '
                     'adj ' wo-adj-amt         
                     
AL-20      MOVE ZEROS              TO WK-NET-AMT-NOTAX.                 
AL-20                                                                   
AL-20      COMPUTE WK-NET-AMT-NOTAX = WO-INVC-AMT + WO-TOT-SALES-TAX.   
AL-20                                                                   
                                                                        
CTS-02*CTS - 02/23/07 CHANGE BEGINS                                     
CTS-02*++++ TO PRINT 6 DIGITS OF THE INVOICE NUMBER                     
CTS-02*                                                                 
CTS-02     IF  WK-PROC-HDR-FLG = 'Y'                                    
TEST-2         MOVE ZEROS             TO WK-HOLD-INV-NBR-6              
CTS-02         MOVE '1'               TO NT-CODE-1A   
      *         add 1000 to test-nbr
      *         move test-nbr to wo-invc-nbr   
               move wo-invc-nbr             to wk-invc-nbr-18           
      
                    if  wo-cred-memo not = 0
                        move wo-cred-memo   to wk-invc-nbr-18
                    end-if

CTS-02         MOVE wk-invc-nbr-7-18        TO WK-NT-INV-NBR
               move wk-invc-nbr-7-18        to last-invc-nbr            
               MOVE WK-NT-INV-NBR-6         TO NT-INVC-NBR-6, nt-doc-nbr
CTS-02         MOVE WK-NT-INV-NBR-6         TO WK-HOLD-INV-NBR-6 
               MOVE wk-invc-nbr-7-18        TO NT-INVC-NBR-6  
               display 'in ' wo-invc-nbr
                       ' '   wk-invc-nbr-7-18
                       ' '   WK-NT-INV-NBR
                       ' '   NT-DOC-NBR        
                       ' '   NT-INVC-NBR-6                       
CTS-02         WRITE NEWT-REC         FROM 
                  Function Upper-case(WK-NEW-TECH-REC)       
AL-01A         ADD 1     TO WK-NT-REC-CNT  
            display 'wk-new ' WK-NEW-TECH-REC

CTS-02         MOVE SPACES            TO   WK-NEW-TECH-REC              
CTS-02         MOVE SPACES            TO   WK-PROC-HDR-FLG              
CTS-02     END-IF.                                                      
CTS-02*                                                                 
CTS-02     MOVE WO-JOB-NBR            TO WK-NT-JOB-NBR.                 
CTS-02*                                                                 
AL-20                                                                   
AL-28                                                                   
CTS-02     IF WK-NT-JOB-NBR-7 NOT = WK-NT-SAVE-JOB-NBR                  
CTS-02        MOVE WK-NT-JOB-NBR-7    TO WK-NT-SAVE-JOB-NBR             
CTS-02        ADD  1                  TO WK-JOB-SEQ-CNT                 
CTS-02     END-IF.                                                      

              MOVE WO-INVC-NBR        TO WS-INV-NBR-NEW
              if  wo-cred-memo not = 0
                  move wo-cred-memo   to WS-INV-NBR-NEW
              end-if
              
              MOVE WS-INV-NBR-NEW(7:12) TO NT-INV-NBR.
              display 'inv: ' wo-invc-nbr
                      ' ' wo-cred-memo
                      ' ' ws-inv-nbr-new 
                      ' ' nt-inv-nbr.
                      
al
CTS-02        MOVE ZEROS              TO WK-LINE-CNT                    
CTS-02        INITIALIZE NEW-TECH-REC-2                                 
CTS-02        MOVE '2'                TO NT-CODE-2A                     
CTS-02        MOVE WK-EU-PG-FLG       TO NT-CODE-2B                     
CTS-02        MOVE '01'               TO NT-TAB2-P1                     
CTS-02        MOVE '01'               TO NT-LVL-A                       
CTS-02        MOVE WK-JOB-SEQ-CNT     TO NT-JOB-SEQ                     

             
              MOVE WO-INVC-NBR        TO WS-INV-NBR-NEW
              if  wo-cred-memo not = 0
                  move wo-cred-memo     to WS-INV-NBR-NEW
              end-if
              
AL-01         MOVE WS-INV-NBR-NEW(7:12) TO NT-INV-NBR.
CTS-02*                                                                 
CTS-02     IF WO-PUB = WK-CLS-PUB               
CTS-02        IF WO-AD-DESC = SPACE                                     
CTS-02        OR WO-AD-DESC(1:3) = 'N/A'                                
CTS-02           MOVE WO-AI-CONTACT    TO NT-AD-DESC-VAL                
CTS-02        ELSE                                                      
CTS-02           IF WO-AD-DESC NOT = SPACES                             
CTS-02              MOVE WO-AD-DESC    TO NT-AD-DESC-VAL                
                END-IF                                                  
CTS-02        END-IF                                                    
CTS-02     ELSE                                                         
CTS-02        IF WO-AD-DESC NOT = SPACES                                
CTS-02           MOVE WO-AD-DESC       TO NT-AD-DESC-VAL                
CTS-02        END-IF                                                    
CTS-02     END-IF.                                                      
CTS-02*
CTS-02     IF  WO-AD-DESC NOT = SPACES                                
CTS-02        MOVE WO-AD-DESC       TO NT-AD-DESC-VAL                
CTS-02     END-IF                                                    

*********  IF WO-PUB = WK-CLS-PUB               
**********       MOVE space            TO NT-AD-DESC-VAL                
*******    end-if
           display 'ad-desc ' WO-AD-DESC ' '
                             wo-pub ' ' NT-AD-DESC-VAL 
CTS-02        ADD  1                   TO WK-LINE-CNT                   
CTS-02        MOVE WK-LINE-CNT         TO NT-LVL-C                      
CTS-02        MOVE '02'                TO NT-TAB2-P3                    
CTS-02*CTS - 02/23/07 CHANGE ENDS                    
                           
148700*                                                                 
 48300     MOVE WO-ISSUE              TO UTL-EDIT-DATE.                 
148400     PERFORM EDIT-OUT-DATE.                                       
148500     MOVE UTL-EDIT-DATE-SL       TO DTL-ISSUE.                    
148600     MOVE UTL-EDIT-DATE-6        TO MT-WO-ISSUE.                  
CTS-02*CTS - 02/23/07 CHANGE BEGINS                                     
CTS-02     MOVE SPACES                 TO WK-WO-ISSUE.                  
CTS-02     MOVE UTL-EDIT-DATE-6        TO WK-WO-ISSUE.                  
CTS-02*                                                                 
CTS-02     MOVE WK-TEMP-YY             TO NT-BILL-YY-2.                 
CTS-02     MOVE WK-TEMP-MM             TO NT-BILL-MM-1.                 
CTS-02     MOVE WK-TEMP-DD             TO NT-BILL-DD-1.                 
CTS-02     MOVE '/'                    TO NT-BILL-LB-1.                 
CTS-02     MOVE '/'                    TO NT-BILL-LB-2.                 
CTS-02*                                                                 
CTS-02     IF WK-YY-1 < 83                                              
CTS-02        MOVE '20'                TO NT-BILL-YY-1                  
CTS-02     ELSE                                                         
CTS-02        MOVE '19'                TO NT-BILL-YY-1                  
CTS-02     END-IF.                                                      
CTS-02*                                                                 
CTS-02     MOVE WO-PUB                TO NT-WO-ISSUE-PUB.               
CTS-02     ADD  1                   TO WK-LINE-CNT                      
CTS-02     MOVE WK-LINE-CNT         TO NT-LVL-B                         
CTS-02     MOVE '02'                TO NT-TAB2-P2                       
            

148700*                                                                 
148800*>   Item 11                                                      
148700                                                 
al-01      MOVE WO-INVC-NBR        TO WS-INV-NBR-NEW
           if  wo-cred-memo not = 0
               move wo-cred-memo       to WS-INV-NBR-NEW
           end-if
al-01      MOVE WS-INV-NBR-NEW(7:12)   TO dtl-ref-nbr-9.
196700     MOVE DTL-REF-NBR-6          TO MT-INVC-NBR.                  
TEST-2a    MOVE WS-INV-NBR-NEW(7:12)   TO WK-NT-INV-NBR                 
CTS-02*                                                                 
149600                                                                  
149700*>   Item 15                                                      
149800                                                                  
149900     MOVE SPACES                 TO WK-DIMENSIONS.                
CTS-02     MOVE SPACES                 TO NT-DATA-6.                    
CTS-02     MOVE SPACES                 TO NT-DATA-7.                    
150000     MOVE WO-AD-TYPE             TO WK-WO-AD-TYPE.                
150100   
           if  wo-ad-quantity not = WO-PROD-AD-QUANTITY
               display 'override qty '
                 wo-ad-quantity ' ' WO-PROD-AD-QUANTITY       
               move WO-PROD-AD-QUANTITY to WO-AD-QUANTITY
               
           end-if
           if  wo-ad-size not = WO-PROD-AD-SIZE
               display 'override size '
                    WO-PROD-AD-SIZE ' 'wo-ad-size
               move WO-PROD-AD-SIZE    to wo-ad-size
               
           end-if
           if  WO-PROD-AD-SHAPE not = wo-ad-shape
               display 'override shape '
                    WO-PROD-AD-SHAPE ' '  wo-ad-shape
           
               move WO-PROD-AD-SHAPE   to wo-ad-shape
           end-if

150300     MOVE WO-AD-QUANTITY         TO WK-AD-QUANTITY                

150200     IF  WO-PUB = WK-FSI-PUB                                      
150300         MOVE WO-AD-QUANTITY     TO WK-AD-QUANTITY                
150400         MOVE WK-AD-QUANTITY-2   TO WK-FSI-DIM-1                  
***********    MOVE 1                  TO NT-FSI-DIM-1                  
150500         MOVE ' X '              TO WK-FSI-DIM-X                  
150600         MOVE WO-AD-SIZE         TO WK-FSI-DIM-2                  
CTS-02         STRING WK-AD-QUANTITY-2 DELIMITED BY SIZE                
                      ' w '            DELIMITED BY SIZE                
                      WO-AD-SIZE       DELIMITED BY SIZE                
               INTO                       NT-FSI-DIM-2                  
150700     ELSE                                                         
150800     IF   WO-PUB        =  WK-NYT-PUB                             
151000     AND (WK-WO-AD-TYPE = 'LMD' OR 'LLN' OR 'LNF')                
151100          MOVE WO-RATE-QUANTITY  TO WK-AD-QUANTITY                
151200          MOVE WK-AD-QUANTITY-2  TO WK-CLS-DIM-1                  
CTS-02          MOVE 1                 TO NT-CLS-DIM-1                  
151300          MOVE ' X '             TO WK-CLS-DIM-TIMES              
151400          MOVE WO-RATE-AD-SIZE   TO WK-AD-SIZE                    
151500          PERFORM MOVE-AD-SIZE                                    
151600     ELSE                                                         
151700       IF  WO-PUB = WK-CLS-PUB                                  
                 INITIALIZE WK-DIMENSIONS-RE                            
                 PERFORM FORMAT-OTHER
             ELSE     
151800         MOVE WO-AD-QUANTITY     TO WK-AD-QUANTITY                
151900         MOVE WK-AD-QUANTITY-2   TO WK-CLS-DIM-1                  
***********    MOVE 1                  TO NT-CLS-DIM-1                  
152000         MOVE ' X '              TO WK-CLS-DIM-TIMES              
152100         MOVE WO-AD-SIZE         TO WK-AD-SIZE  
               display 'xx ' wk-ad-quantity
                       ' ' wk-cls-dim-1
                       ' ' nt-cls-dim-1
                       ' ' wk-cls-dim-times
                       ' ' wo-ad-size
                        ' ' wk-ad-size                       
152200         PERFORM MOVE-AD-SIZE.                                    

CTS-02     MOVE WO-AD-QUANTITY     TO WK-AD-QUANTITY                    
AL-20 *take out                                                         
CTS-02*    MOVE 1                  TO NT-FSI-DIM-1                      
AL-20 *take out       
al
CTS-02                                                                  
CTS-02     MOVE '04'                   TO NT-TAB2-P7.                   
CTS-02     MOVE NT-LVL-B               TO NT-LVL-F2.                    
152900     MOVE WK-DIM-1           TO DTL-DIMENSIONS. 
153000*                                                                 
152500                                                                  
153100*>   Item ???????                                                 
153200                                                                  
153400     MOVE WO-CLASS               TO MT-CLASS-CODE.                
153500     MOVE WO-AD-DESC             TO MT-DESCRIPTION.               
153600     MOVE DTL-DIMENSIONS         TO MT-SAU-SIZE.                  
153700                                                                  
154900     MOVE WO-PUB             TO DTL-PUB                           
155000     MOVE WO-EDITION         TO DTL-EDITION                       
155100     MOVE WO-AD-TYPE         TO DTL-AD-TYPE                       
155200     MOVE WO-AD-COLOR        TO DTL-COLOR                         
155300     MOVE WO-AD-SHAPE        TO DTL-BLEED.                        
155400                                                                  
152500                                                                  
155800     IF  SSL-CATEGORY = LOW-VALUE                                 
155900     AND WO-FREQ      > ZEROS                                     
156000         MOVE WO-FREQ            TO DTL-LEVEL                     
156100         IF  DTL-LVL-C (8) = SPACE                                
156200             MOVE 'L'            TO DTL-LVL-C (7)                 
156300             MOVE 'V'            TO DTL-LVL-C (8)                 
156400         ELSE                                                     
156500         IF  DTL-LVL-C (7) = SPACE                                
156600             MOVE 'L'            TO DTL-LVL-C (6)                 
156700             MOVE 'V'            TO DTL-LVL-C (7)                 
156800         ELSE                                                     
156900         IF  DTL-LVL-C (6) = SPACE                                
157000             MOVE 'L'            TO DTL-LVL-C (5)                 
157100             MOVE 'V'            TO DTL-LVL-C (6)                 
157200         ELSE                                                     
157300         IF  DTL-LVL-C (5) = SPACE                                
157400             MOVE 'L'            TO DTL-LVL-C (4)                 
157500             MOVE 'V'            TO DTL-LVL-C (5)                 
157600         ELSE                                                     
157700         IF  DTL-LVL-C (4) = SPACE                                
157800             MOVE 'L'            TO DTL-LVL-C (3)                 
157900             MOVE 'V'            TO DTL-LVL-C (4)                 
158000         ELSE                                                     
158100         IF  DTL-LVL-C (3) = SPACE                                
158200             MOVE 'L'            TO DTL-LVL-C (2)                 
158300             MOVE 'V'            TO DTL-LVL-C (3)                 
158400         ELSE                                                     
158500         IF  DTL-LVL-C (2) = SPACE                                
158600         AND DTL-BLEED     = SPACE                                
158700             MOVE 'L'            TO DTL-LVL-C (1)                 
158800             MOVE 'V'            TO DTL-LVL-C (2)                 
158900         ELSE                                                     
159000         IF  DTL-LVL-C (2) = SPACE                                
159100             MOVE 'L'            TO DTL-LVL-C (2)                 
159200         ELSE                                                     
159300         IF  DTL-LVL-C (1) = SPACE                                
159400         AND DTL-BLEED     = SPACE                                
159500             MOVE 'L'            TO DTL-LVL-C (1).                
159600                                                                  
159700*>   Item 17                                                      
159800                                                                  
159900     PERFORM GET-LINEAGE.   

             if  WK-GOLD-IND = 'Y' 
             or  WK-PLAT-IND = 'Y'   
                 move 0                to WK-TEMP-LINEAGE-I
                                          WK-TEMP-LINEAGE-D
             end-if
             
160100     IF  WO-PUB = WK-CLS-PUB OR WK-MAGC-PUB  OR WK-NYTL-PUB       
160200         MOVE WK-TEMP-LINEAGE-I       TO DTL-CLS-QUANTITY         
160300                                         nT-BILLED-UNITS-I        
160400         MOVE 'L'                     TO DTL-CLS-LINES-2          
160500         ADD WK-TEMP-LINEAGE-I        TO TTL-UNIT-AMT             
160600                                         TTL-UNIT-AMT-LINE        
               display 'NT-BILLED-UNITS ' wo-pub ' '
                               nT-BILLED-UNITS-I
160700     ELSE                                                         
*dwww***       PERFORM GET-FSI                                          
*dwww***                                                                
*dwww***       IF COD2-FLAG1 = 'I'                                      
*dwww***          MOVE WK-TEMP-LINEAGE-I    TO DTL-LINEAGE-I            
*dwww***                                       MT-BILLED-UNITS-I        
*dwww***       ELSE                                                     
*dwww***          ADD WK-TEMP-LINEAGE-D     TO TTL-UNIT-AMT             
*dwww***                                       TTL-UNIT-AMT-INCH        
161500            IF COD-FLAG1 = 'D'                                    
161600               MOVE WK-TEMP-LINEAGE-D TO DTL-LINEAGE-D            
161700                                         nT-BILLED-UNITS-D        
161800            ELSE                                                  
161900               MOVE WK-TEMP-LINEAGE-I TO DTL-LINEAGE-I            
162000                                         nT-BILLED-UNITS-I
                  end-if
           end-if             
                                     
           IF  WO-PUB = WK-CLS-PUB OR WK-NYTL-PUB                     
             display 'nT-BILLED-UNIT-i ' wo-pub ' ' 
                                  nT-BILLED-UNITS-I
             move nT-BILLED-UNITS-I to wk-temp-z
            STRING wk-temp-z          DELIMITED BY SIZE
                      ' X '  DELIMITED BY SIZE                
                      'L'  DELIMITED BY size               
               INTO   NT-FSI-DIM-2       
             display 'clas dim ' nt-fsi-dim-2               
           ELSE                                               
               MOVE WK-DIMensions        TO nt-fsi-dim-2.     

162100***                                                               
CTS-02     MOVE NT-LVL-B               TO NT-LVL-G.                     
CTS-02     MOVE '05'                   TO NT-TAB2-P8.                   
162900*>   Item 18                                                      
163000                                                                  
163100     IF  COD-FLAG1             = 'D'                              
163200     AND WK-TEMP-LINEAGE-D NOT =  ZEROS                           
163300         COMPUTE WK-TEMP-RATE 
                                    ROUNDED                             
163400            = ((WO-GROSS-AMT - WO-BLEED-AMT) / WK-TEMP-LINEAGE-D) 
163500     ELSE                                                         
163600     IF  COD-FLAG1         NOT = 'D'                              
163700     AND WK-TEMP-LINEAGE-I NOT =  ZEROS                           
163800         COMPUTE WK-TEMP-RATE 
                                    ROUNDED                             
163900            = ((WO-GROSS-AMT - WO-BLEED-AMT) / WK-TEMP-LINEAGE-I) 
164000     ELSE                                                         
164100         MOVE ZEROS              TO WK-TEMP-RATE.                 
164200    
             if  WK-GOLD-IND = 'Y' 
             or  WK-PLAT-IND = 'Y'   
                 move 0                to WK-TEMP-RATE        
             end-if

           display 'WK-TEMP-RATE ' WK-TEMP-RATE 
AL-06      MOVE WK-TEMP-RATE           TO MT-RATE.                      
AL-06      MOVE WK-TEMP-RATE           TO DTL-AVG-RATE                  
CTS-02*CTS - 02/23/07 CHANGE BEGINS                                     
CTS-02     MOVE WK-TEMP-RATE           TO NT-RATE.                      
CTS-02     MOVE NT-LVL-B               TO NT-LVL-H.                     
CTS-02     MOVE '06'                   TO NT-TAB2-P9.                   
CTS-02*                        
AL-06      MOVE WK-TEMP-RATE           TO MT-RATE.                      23610001
AL-06      MOVE WK-TEMP-RATE           TO DTL-AVG-RATE                  23620000
********   IF PROCESS-GROUP-CODE = 'Y'                                  23621000
*******       MOVE ZEROS                  TO DTL-AVG-RATE               23622000
*******       COMPUTE WK-TEMP-GROUP-RATE = WK-TEMP-GROUP-RATE           23623000
*********                                + WK-TEMP-RATE.                23624000
           display 'WK-TEMP-RATE ' WK-TEMP-RATE ' ' 
                    'MT-RATE ' MT-RATE ' ' 
                    'DTL-AVG-RATE ' DTL-AVG-RATE

169000*>   Item 19                 

CTS-02        COMPUTE DTL-GROSS-AMT = WO-GROSS-AMT + WO-GROSS-ADJ * -1          

169100                                                                  23640000
***********IF  WO-REVISED-FLAG = 'B' OR 'I'     
***********      display 'WO-REVISED-FLAG ' WO-GROSS-ADJ
***********    MOVE WO-GROSS-ADJ       TO DTL-GROSS-AMT                 23660000
***********    ADD DTL-GROSS-AMT       TO TTL-GROSS-AMT-ADJ             23670000
***********                               TTL-AGY-ADV-GROSS-AMT-ADJ
***********                             TTL-AGY-GROSS-AMT-ADJ  
***********ELSE                                                         23690000
169700         MOVE WO-GROSS-AMT       TO DTL-GROSS-AMT                 23700000
************   ADD WO-GROSS-AMT        TO TTL-GROSS-AMT                 23710000
169800         ADD WO-GROSS-AMT        TO TTL-AGY-ADV-GROSS-AMT 
169900                                    TTL-AGY-GROSS-AMT
***********end-if
           
170100     MOVE DTL-GROSS-AMT          TO DTL-GROSS-AMT-P               
CTS-02                                    NT-GROSS-AMT                  
CTS-02                                    NT-DISP-GRS-AMT               
                                          MT-GROSS-AMT.                 
170200     MOVE ZEROS                  TO WK-DISCOUNTS.                 
CTS-02     MOVE NT-LVL-B               TO NT-LVL-I.                     
CTS-02     MOVE '07'                   TO NT-TAB2-P10.                  
AL-20                                                                   
AL-20      MOVE NT-LVL-B               TO NT-LVL-O.                     
AL-20      MOVE '08'                   TO NT-TAB2-P16.                  
AL-20      COMPUTE NT-NET-DTL-AMT = WO-INVC-AMT - WO-TOT-SALES-TAX.     
AL-20      COMPUTE DTL-NET-NOTAX = WO-INVC-AMT  - WO-TOT-SALES-TAX.     
AL-20      ADD DTL-NET-NOTAX       TO TTL-AGY-ADV-NET-AMT-NT-T          
AL-20                                 TTL-AGY-ADV-NET-AMT-NT            
AL-20                                 TTL-AGY-NET-AMT-NT.               

AL-01A**************************************************
AL-01A      MOVE '02'                TO NT-TAB2-P19.
AL-01A      ADD  1                   TO WK-LINE-CNT                     
AL-01A      MOVE WK-LINE-CNT         TO NT-LVL-R.                       
            MOVE WO-EDITION          TO NT-EDTN-LIT
            
            if  wk-gold-ind = 'Y'
            or  wk-plat-ind = 'Y'
                MOVE space             TO NT-EDTN-LIT
AL-01A                                    NT-TAB2-P19   
                                          NT-LVL-R
            end-if
            
AL-01A      MOVE WO-AD-COLOR         TO NT-COLOR
AL-01A      MOVE WO-CLASS-4          TO NT-CLASS
            if  WO-PAGE not = space
            if  WO-PAGE (1:3) not = '000'
AL-01A          MOVE 'PAGE: '        TO NT-PAGE-LIT
AL-01A          MOVE WO-PAGE         TO NT-PAGE
            end-if

            if  WO-AUTH-NBR not = spaces
AL-01A          MOVE '02'            TO NT-TAB2-P18                
AL-01A          MOVE 'PO# '          TO NT-PO-LIT
AL-01A          MOVE WO-AUTH-NBR     TO NT-PO-nbr
AL-01A          ADD  1               TO WK-LINE-CNT                     
AL-01A          MOVE WK-LINE-CNT     TO NT-LVL-Q
            end-if
                                                                    
CTS-02* ++++ SAVE COMMISSION DETAILS FOR AGY OR ADV                     
CTS-02*                                                                 
CTS-02     MOVE ZEROS                     TO WK-COM-PCT1                
CTS-02                                       WK-COM-AMT                 
CTS-02                                       WK-COM-AMT1                
CTS-02                                       WK-GROSS-AMT1              
CTS-02                                       WK-TAX-AMT                 
CTS-02                                       WK-PPD-AMT                 
CTS-02     IF WO-COM-FLAG  NOT = '$' OR '%'                             
CTS-02        IF WO-AGY-COMM NOT = ZEROS                                
CTS-02           MOVE WO-AGY-COMM      TO WK-COM-AMT1                   
CTS-02*          COMPUTE WK-COM-AMT1   = - WK-COM-AMT1                  
CTS-02           MOVE WK-COM-AMT1      TO NT-DISP-COM-AMT               
CTS-02           ADD WK-COM-AMT1       TO TTL-AGY-ADV-COM-AMT           
CTS-02                                    TTL-TOT-AGY-COM-AMT           
CTS-02        END-IF                                                    
CTS-02     ELSE                                                         
CTS-02        IF WO-COM-FLAG    = '%'                                   
CTS-02           DISPLAY 'INSIDE %' WO-COM-FLAG                         
CTS-02           MOVE WO-COM-PCT          TO WK-COM-PCT1                
CTS-02              MOVE WO-GROSS-AMT     TO WK-COM-AMT                 
CTS-02           COMPUTE WK-COM-AMT1 =                                  
CTS-02                   (WK-COM-AMT * WK-COM-PCT1) / 100               
CTS-02           ADD WK-COM-AMT1          TO TTL-AGY-ADV-COM-AMT        
CTS-02                                       TTL-TOT-AGY-COM-AMT        
CTS-02           MOVE WK-COM-AMT1         TO NT-DISP-COM-AMT            
CTS-02           DISPLAY 'INSIDE % AMOUNT' WK-COM-AMT1                  
CTS-02        ELSE                                                      
CTS-02           IF WO-COM-FLAG = '$'                                   
CTS-02              DISPLAY 'INSIDE $' WO-COM-FLAG                      
CTS-02              MOVE WO-AGY-COMM      TO WK-COM-AMT1                
CTS-02              ADD WK-COM-AMT1       TO TTL-AGY-ADV-COM-AMT        
CTS-02                                       TTL-TOT-AGY-COM-AMT        
CTS-02              MOVE WK-COM-AMT1      TO NT-DISP-COM-AMT            
CTS-02              DISPLAY 'INSIDE $ AMOUNT' WK-COM-AMT1               
CTS-02           END-IF                                                 
CTS-02        END-IF                                                    
CTS-02     END-IF.                                                      
CTS-02*                                                                 
CTS-02        IF WO-TOT-SALES-TAX NOT = ZEROS                           
CTS-02           MOVE WO-TOT-SALES-TAX    TO WK-TAX-AMT                 
CTS-02           MOVE WK-TAX-AMT          TO NT-DISP-TAX-AMT            
CTS-02                                                                  
CTS-02           ADD  WK-TAX-AMT          TO TTL-AGY-ADV-TAX-AMT        
CTS-02                                       TTL-TOT-AGY-TAX-AMT        
CTS-02        END-IF                                                    
08/30-**        MOVE WO-INVC-CODE (1:1)     TO WS-INVC-CODE             
FXC--***        IF  (CRCARD OR NYTDCC OR WO-INVC-CODE (1:3) = 'PPD')    
al-01a**        IF  WO-INVC-CODE (1:3) = 'PPD'              
DW-08 **            AND  WO-INVC-AMT > ZEROS                            
CTS-02**           MOVE WO-INVC-AMT         TO WK-PPD-AMT
                         
                display 'creditflag ' invc-credit-flag ' ' 
                         wo-invc-amt
CTS-02*++++ NET AMOUNT PER 02-LEVEL                                     
CTS-02*                                                                 
CTS-02     MOVE ZEROS            TO WK-GROSS-AMT1                       
CTS-02        COMPUTE WK-GROSS-AMT1 = WO-GROSS-AMT + WO-GROSS-ADJ * -1  
CTS-02*                                                                 
CTS-02     COMPUTE WK-NET-AMT1  = WK-GROSS-AMT1                         
CTS-02     MOVE WK-NET-AMT1             TO NT-DISP-NET-AMT.             
CTS-02*  
CTS-02        COMPUTE nt-GROSS-AMT = WO-GROSS-AMT

           display 'write 3 ' NEW-TECH-REC-2 
              WRITE NEWT-REC       FROM 
                    Function Upper-case(NEW-TECH-REC-2)                 
              ADD 1     TO WK-NT-REC-CNT              
              INITIALIZE NEW-TECH-REC-2     
AL-01*                                                                  

174700*>   Item 12 C                                                    
174800                                                                  
174900     MOVE 'POS'                  TO COD-KEY.                      
175000     MOVE WO-PUB                 TO COD-CODE1.                    
175100     MOVE WO-AD-POSN             TO COD-CODE2.                    
175200     PERFORM GET-COD-ISS.                                         
175300                                                                  
175700     IF  STATUS-CODE = ZEROS                                      
175900         PERFORM PROCESS-DETAIL-POSN.                             
176000                                                                  
176100*>   Item 12 C                                                    
176200                                                                  
176600     IF  WO-SR-TYPE NOT = SPACE                                   
176700         PERFORM PROCESS-DETAIL-SR.                               
176800                                                                  
176900*>   Item 12 B                                                    
177000                                                                  
CTS-02     MOVE ZEROS                      TO WK-D-P-AMT                
CTS-02                                        WK-SPL-AMT                
AL-20                                         WK-ADJ-NET-NT-AMT         
CTS-02                                        WK-ADJ-AMT.               
178500         SET WO-DAX WO-DCX           TO 1                         
CTS-02         SET DSC-X                   TO 1                         
178600         PERFORM PROCESS-DETAIL-DISC 3 TIMES.                     
178700                                                                  
178800*>   Item 12 A                                                    
178700                                                                  
180300         SET WO-SAX WO-SCX           TO 1                         
CTS-02         SET SPL-X                   TO 1                         
180400         PERFORM PROCESS-DETAIL-SPCH 4 TIMES.                     
180500                                                                  
180600*>   Detail line A part 5                                         
180700                                                                  
181100     IF  WO-ACR-CODE NOT = SPACES                                 
181200         PERFORM PROCESS-DETAIL-ACR.                              
                                
                                
           iF  WO-RESP-CODE NOT = SPACES OR WO-ACR-CODE (1:3) = 'MAS'   
181600         COMPUTE DTL-GROSS-AMT  = WO-ADJ-AMT + WO-GROSS-ADJ * -1          
             IF DTL-GROSS-AMT NOT = ZEROES                             
                display 'resp-code ' wo-resp-code ' ' 
                                    WO-GROSS-Amt 
                                    WO-ADJ-AMT ' ' 
                                   WO-GROSS-ADJ 
               display 'resp code if ' DTL-GROSS-AMT
CTS-02         MOVE '2'                TO NT-CODE-2A                    
CTS-02         MOVE WK-EU-PG-FLG       TO NT-CODE-2B                    
CTS-02         MOVE WK-JOB-SEQ-CNT     TO NT-JOB-SEQ                    
CTS-02         ADD  1                  TO WK-LINE-CNT                   
CTS-02         MOVE WK-LINE-CNT        TO NT-LVL-C                      
CTS-02                                    NT-LVL-I                      
CTS-02         MOVE '02'               TO NT-TAB2-P3                    
CTS-02         MOVE '07'               TO NT-TAB2-P10                   
181500         PERFORM PROCESS-DETAIL-ADJ                               
               COMPUTE DTL-GROSS-AMT  = - DTL-GROSS-AMT                 
181800         ADD DTL-GROSS-AMT       TO TTL-GROSS-AMT-ADJ                     
CTS-02                                    TTL-AGY-ADV-GROSS-AMT-ADJ             
CTS-02                                    TTL-AGY-ADV-GROSS-AMT-ADJ-T           
CTS-02                                    TTL-AGY-ADV-ADJ-AMT                   
CTS-02                                    TTL-TOT-AGY-ADJ-AMT                   
181900                                    TTL-AGY-GROSS-AMT-ADJ                 
CTS-02                                    WK-ADJ-AMT                            
182000         MOVE DTL-GROSS-AMT      TO DTL-GROSS-AMT-P                       
CTS-02                                    NT-ADJ-AMT                            
CTS-02                                    NT-DISP-ADJ-AMT                       
CTS-02                                    NT-DISP-NET-AMT                       
CTS-02         PERFORM CHECK-AND-LOAD-ADJ-AMT                           
             END-IF                                                     
           END-IF.       
                                                       
CTS-02     INITIALIZE NEW-TECH-REC-2                                    
CTS-02     MOVE '2'                    TO NT-CODE-2A                    
CTS-02     MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
CTS-02     MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
CTS-02     MOVE '02'                   TO NT-TAB2-P14                   
CTS-02     ADD  1                      TO WK-LINE-CNT                   
CTS-02     MOVE WK-LINE-CNT            TO NT-LVL-M                      
CTS-02     MOVE 'REF NBR: '            TO NT-JOB-NBR-LIT                
CTS-02     MOVE wo-job-nbr             TO NT-JOB-NBR 
               display 'write 5 ' NEW-TECH-REC-2 
 
 CTS-02     WRITE NEWT-REC       FROM 
                Function Upper-case(NEW-TECH-REC-2)
AL-01A     ADD 1     TO WK-NT-REC-CNT              

           display 'invc-credit-flag ' invc-credit-flag ' '
                     WO-INVC-AMT

*****************ppd
           if invc-credit-flag = 'P' 
CTS-02            INITIALIZE NEW-TECH-REC-2                                    
                  move '2'               to NT-CODE-2A
                  move 'G'              to NT-CODE-2B
           	   move space            to NT-DATA-15
CTS-02           MOVE WO-INVC-AMT         TO WK-PPD-AMT                 
************     MOVE WK-PPD-AMT          TO NT-DISP-PPD-AMT            
CTS-02             MOVE '02'                TO NT-TAB2-P15              
CTS-02             ADD  1                   TO WK-LINE-CNT              
AL-20            MOVE '08'                   TO NT-TAB2-P16
CTS-02             MOVE WK-LINE-CNT         TO NT-LVL-N  
                                                NT-LVL-O   
CTS-02             MOVE 'PREPAID AMT: '     TO NT-PPD-DTL-LIT           
**************     MOVE NT-DISP-PPD-AMT     TO NT-PPD-DTL-AMT           
AL-07              COMPUTE PPD-CC-AMT  = PPD-CC-AMT  + WO-INVC-AMT      
09/07-             COMPUTE WO-CC-AUTH-AMT = ZEROS       - WO-INVC-AMT   
***************    MOVE WO-CC-AUTH-AMT     TO NT-PPD-DTL-AMT           
                   MOVE WO-CC-AUTH-AMT     TO NT-NET-DTL-AMT     


09/07-           ADD WO-CC-AUTH-AMT       TO TTL-NET-AMT                
09/07-                                                                  
CTS-02           ADD  WK-PPD-AMT          TO TTL-AGY-ADV-CC-AMT         
CTS-02                                       TTL-TOT-AGY-CC-AMT         
                   display 'PPD-CC-AMT ' PPD-CC-AMT ' '
                           'WO-INVC-AMT ' WO-INVC-AMT ' '
                           'WK-PPD-AMT ' WK-PPD-AMT ' ' 
                           'WO-CC-AUTH-AMT ' WO-CC-AUTH-AMT ' '
                           'TTL-NET-AMT ' TTL-NET-AMT ' '
                           'TTL-AGY-ADV-CC-AMT ' TTL-AGY-ADV-CC-AMT ' '
                         'TTL-TOT-AGY-CC-AMT ' TTL-TOT-AGY-CC-AMT

                if  NT-NET-DTL-AMT = 0
                      display 'zero ppd'
                else    
                     display 'write p ' NEW-TECH-REC-2 
CTS-02                WRITE NEWT-REC       FROM 
                         Function Upper-case(NEW-TECH-REC-2)
AL-01A               ADD 1     TO WK-NT-REC-CNT              
                 end-if                         
CTS-02     END-IF                                                  

CTS-02     INITIALIZE NEW-TECH-REC-2                                    
182400*>   Detail line A part 6                                         
182500                                                                  
FXC---     IF  WO-CC-AUTH-CODE  = SPACE                                 
182900     AND WO-INVC-CODE NOT = SPACE                                 
                 PERFORM PROCESS-DETAIL-INVC.                           
183100                                                                  
183200*>   Item 13                                                      
183300                                                                  
185400     IF  WO-AUTH-NBR NOT = SPACES                                 
185500     OR  WO-PROD-KEY NOT = SPACES                                 
185600         PERFORM WRITE-DETAIL-NONB.                               
185700                                                                  
186200     IF  WO-AUTH-NBR NOT = SPACES                                 
186300         MOVE 'Ord#'             TO DTL-ORD-LIT                   
186500         MOVE WO-AUTH-NBR        TO DTL-ORD-NBR                   
186600                                    MT-ORDER-NBR.                 
186700                                                                  
187600                                                                  
187700     MOVE ZEROS                  TO NAD-ACCT-NBR.                 
187800                                                                  
187900     IF (SSL-ACCT-NBR     NOT = SSL-AGY-ACCT-NBR                  
188000     AND                        SSL-ADV-PAR-NBR                   
188100     AND SSL-AGY-ACCT-NBR NOT = SSL-ADV-PAR-NBR)                  
188200     OR  SSL-CATEGORY         = LOW-VALUE                         
188300         ADD 1                   TO TTL-ACCT-NBR                  
188400         MOVE SSL-ACCT-NBR       TO NAD-ACCT-NBR                  
188500         PERFORM GET-ADV                                          
               PERFORM FORMAT-MISC-INFO.                                
190200                                                                  
191100                                                                  
191200*>   NYT 1                                                        
191300                                                                  
191700     IF  NAD-ACCT-NBR NOT = ZEROS                                 
191800         PERFORM WRITE-DETAIL-NONB                                
191900         MOVE 'Acct#'            TO DTL-ACCT-LIT                  
192000         MOVE SSL-ACCT-NBR       TO DTL-ACCT-NBR                  
192100         PERFORM GET-J-XREF-2.                                    
192300                                                                  
192400*>   Item 19                                                      
192500*                                                                 
CTS-02     MOVE ZEROS              TO WK-NET-AMT2.                      
CTS-02     MOVE ZEROS              TO WK-NET-AMT3.                      
CTS-02*                                                                 
           COMPUTE WK-NET-AMT2 = (WK-GROSS-AMT1 + WK-D-P-AMT +          
                   WK-SPL-AMT + WK-ADJ-AMT).                            
CTS-02*                                                                 
********   ADD WK-NET-AMT2         TO TTL-AGY-ADV-NET-AMT.              
*****2*                                                                 
           COMPUTE WK-NET-AMT3 = (WK-GROSS-AMT1 + WK-D-P-AMT +          
                   WK-SPL-AMT + WK-ADJ-AMT + WK-TAX-AMT - WK-COM-AMT1). 
ct2-02*                                                                 
********   ADD WK-NET-AMT3         TO TTL-AGY-NET-AMT.                  
*********  ADD WK-NET-AMT3         TO TTL-NET-AMT.      
AL-20      ADD  DTL-NET-NOTAX      TO TTL-AGY-ADV-NET-AMT-NT.   
CTS-02     ADD wo-invc-amt         TO TTL-AGY-NET-AMT                   
                                      TTL-AGY-ADV-NET-AMT 
                                      TTL-NET-AMT.      

AL-20                                                                   
193500*                                                                 
193600     MOVE DTL-NET-AMT            TO DTL-NET-AMT-P.                
193700     ADD  DTL-NET-AMT            TO TTL-IAR-10-AMT.               
      **     
AL-06                                                                   26091001
AL-06      IF PROCESS-GROUP-CODE = 'Y'                                  26100000
AL-06         MOVE ZEROS               TO DTL-NET-AMT-P.                26110000
AL-06                                                                   26120000
193800                                                                  
CTS-02     PERFORM PROCESS-CREDIT-CARD-DETAILS.                         
FXC---                                                                  
194200     MOVE WO-AD-TYPE             TO WK-PFM-IND.                   
194300                                                                  
                                                                        
194400     PERFORM PROCESS-CNT-PFM-ACCUM.                               
194500                                                                  
194600     MOVE SPACE                  TO WK-FIRST-LINE.                
194800     MOVE '1'                    TO MT-CODE-1.                    
194900                                                                  
195000     IF  WK-AGY-JXRF-2 NUMERIC                                    
195100         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-1                  
195200     ELSE                                                         
195300         MOVE SPACE              TO MT-AGY-NBR-1.                 
195400                                                                  
195500     IF  WK-ADV-JXRF-2 NUMERIC                                    
195600         MOVE WK-ADV-JXRF-2      TO MT-ADV-NBR-1                  
195700     ELSE                                                         
195800         MOVE SPACE              TO MT-ADV-NBR-1.                 
195900                                                                  
196000     IF  SSL-CATEGORY = LOW-VALUES                                
196100             MOVE NAD-NAME-1     TO MT-ADV-NAME-1                 
196200             MOVE SPACE          TO MT-ADV-NBR-1.                 
196300                                                                  
196400     MOVE WK-DISCOUNTS           TO MT-SURCHARGES.                
           MOVE DTL-NET-AMT            TO MT-NET-AMT.                   
196800     MOVE WK-PAGE-CTR            TO MT-PAGE-NBR.                  
196810     MOVE WO-PUB                 TO MT-WO-PUB.                    
196820     MOVE WO-EDITION             TO MT-WO-EDITION.                
196830     MOVE WO-AD-TYPE             TO MT-WO-AD-TYPE.                
196900                                                                  
197000     IF  WK-PRODUCE-TAPE = 'Y'                                    
197100         WRITE MAG-TAPE-REC FROM WK-MAG-TAPE-REC.                 
197200                                                                  
197300     MOVE SSL-ACCT-NBR           TO WK-CDT-ACCT-NBR.              
197400     MOVE SSL-AGY-ACCT-NBR       TO WK-CDT-AGY-ACCT-NBR.          
198000         MOVE WO-INVC-AMT        TO WK-CDT-INVC-AMT.              
198100                                                                  
AL-06         MOVE 'Y'    TO DETAIL-DATA.  

           if  SSL-AGY-ACCT-NBR = 0
               add wo-gross-amt        to ttl-gross-amt
           else
               add wo-invc-amt, wo-agy-comm
                                       to ttl-gross-amt
           end-if

           evaluate wo-pub
               when 'CLAS' add wo-invc-amt to wk-CLAS-amt 
               when 'CST ' add wo-invc-amt to wk-CST-amt  
               when 'FSIN' add wo-invc-amt to wk-FSIN-amt 
               when 'MG  ' add wo-invc-amt to wk-MG-amt   
               when 'MGL ' add wo-invc-amt to wk-MGL-amt  
               when 'NCOM' add wo-invc-amt to wk-NCOM-amt 
               when 'NYT ' add wo-invc-amt to wk-NYT-amt  
               when 'NYTL' add wo-invc-amt to wk-NYTL-amt 
               when 'TA48' add wo-invc-amt to wk-TA48-amt 
               when 'TMAG' add wo-invc-amt to wk-TMAG-amt 
               when 'TTR ' add wo-invc-amt to wk-TTR-amt  
CTS-02     END-EVALUATE                                                 
               
           display 'PROCESS-DETAILS - end'
           
AL-06  BYPASS-GROUP-INFO.                                               
AL-06 *------------------*                                              
AL-06                                                                   
AL-06      DISPLAY 'BYPASS GROUP ' PROCESS-GROUP-CODE ' '
                   'WK-GOLD-IND ' WK-GOLD-IND ' '
                   'WK-PLAT-IND ' WK-PLAT-IND
*******dww IF (PROCESS-GROUP-CODE = 'Y' OR                              
AL-08      if (WK-GOLD-IND = 'Y'                                         
AL-15         OR WK-PLAT-IND = 'Y')                                     
AL-08          NEXT SENTENCE                                            
AL-08      ELSE                                                         
AL-06         PERFORM RETURN-SSL-READ-WO.                               
AL-06                                                                   
AL-06  PROCESS-DETAILS-EXIT. EXIT.                                      
AL-06 *--------------------------*                                      
AL-06                                                                   
AL-28  
       Pub-Total.
      *----------
           display 'pub break ' save-pub ' '
                                     ssl-pub ' '
\                TTL-pub-invc-AMT
                display SSL-AGY-ACCT-NBR  ' '
                        SAVE-AGY-ACCT-NBR   
                display SSL-ACCT-NBR  ' '
                        SAVE-ACCT-NBR   
           
AL-20      INITIALIZE NEW-TECH-REC-2                                 
AL-20      MOVE '2'                    TO NT-CODE-2A                 
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B     
AL-20      ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                 
AL-20      MOVE '02'                   TO NT-TAB2-P13                
AL-20      ADD  1                      TO WK-LINE-CNT                
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L   
           
AL-2****   WRITE NEWT-REC              FROM 
    ****        Function Upper-case(NEW-TECH-REC-2) 

   *****   display 'write 1 ' NEW-TECH-REC-2 
AL-01A     ADD 1     TO WK-NT-REC-CNT                     
CTS-02     INITIALIZE NEW-TECH-REC-2                                 
CTS-02     MOVE '2'                    TO NT-CODE-2A                 
CTS-02     MOVE WK-EU-PG-FLG           TO NT-CODE-2B 
AL-20      ADD  1                      TO WK-JOB-SEQ-CNT                
CTS-02     MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                 
           MOVE '02'                   TO NT-TAB2-P13                
           ADD  1                      TO WK-LINE-CNT                
           MOVE WK-LINE-CNT            TO NT-LVL-L                   
           MOVE 'PUB'                  TO COD-KEY                    
           MOVE save-pub               TO COD-CODE1                  
           CALL 'GIT' USING COD-FILE COD-REC IO-PKT                  
           STRING 'TOTAL '                  DELIMITED BY SIZE        
                  COD-NAME                  DELIMITED BY SIZE        
                  ': '                      DELIMITED BY SIZE        
           INTO  NT-PUB-DTL-LIT                                      
********   ADD  1                      TO WK-LINE-CNT                
********** MOVE '07'                   TO NT-TAB2-P10                
*********  MOVE WK-LINE-CNT            TO NT-LVL-I                   
           MOVE '08'                   TO NT-TAB2-P16                
           MOVE WK-LINE-CNT            TO NT-LVL-O                   
AL-20 *???????????                                                   
     **    move TTL-pub-GROSS-AMT     to NT-GROSS-AMT
           move TTL-pub-invc-AMT      to NT-NET-DTL-AMT  
           display 
                    'NT-NET-DTL-AMT ' NT-NET-DTL-AMT 
           
           WRITE NEWT-REC              FROM 
              Function Upper-case (NEW-TECH-REC-2)   
           display 'write 2 ' NEW-TECH-REC-2  
AL-20         COMPUTE WK-AGY-ADV-GROSS-TOT = TTL-AGY-ADV-GROSS-AMT-ADJ-T        
AL-20                                      + TTL-AGY-ADV-GROSS-AMT-T            
AL-20                                      + WK-AGY-ADV-GROSS-TOT               
AL-20         COMPUTE WK-AGY-ADV-NET-NOTAX =                            
AL-20                                    TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20                                  + TTL-AGY-ADV-NET-AMT-NT-T       
AL-20                                  +  WK-AGY-ADV-NET-NOTAX     

           MOVE ZEROES             TO TTL-AGY-ADV-GROSS-AMT-ADJ-T    
                                      TTL-AGY-ADV-GROSS-AMT-T        
                                      TTL-pub-GROSS-AMT
AL-20      MOVE ZEROES             TO TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20                                 TTL-AGY-ADV-NET-AMT-NT-T     
                                      TTL-pub-invc-AMT

CTS-02     INITIALIZE NEW-TECH-REC-2                                 
CTS-02     MOVE '2'                    TO NT-CODE-2A                 
CTS-02     MOVE 'A'                    TO NT-CODE-2B 
AL-20      ADD  1                      TO WK-JOB-SEQ-CNT                
CTS-02     MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                 

145900     if  SSL-AGY-ACCT-NBR  not =  SAVE-AGY-ACCT-NBR           
0          or  SSL-ACCT-NBR      not =  SAVE-ACCT-NBR
           OR  SSL-FILE-STATUS       = 'EOF'                      
AL-20          MOVE WK-EU-PG-FLG       TO NT-CODE-2B
               end-if
           WRITE NEWT-REC              FROM 
              Function Upper-case (NEW-TECH-REC-2)   

CTS-02     MOVE ssl-pub             TO save-PUB                         
      
AL-05                                                                   
AL-05  FORMAT-CC-INFO.                                                  
AL-05 *--------------*                                                  
AL-05      IF WO-CC-NUMBER > 0                                          
AL-05         NEXT SENTENCE                                             
AL-05      ELSE                                                         
AL-05         MOVE WO-MAT-COMMENT      TO WK-CC-COMMENT                 
AL-05         MOVE WO-INVC-CODE        TO WK-CC-TYPE                    
AL-05         MOVE WO-PU-PAGE          TO WK-CC-EXP-DATE                
AL-05         MOVE WK-CC-EXP-DATE      TO WK-CC-EXP-DATE-NUM            
AL-05         MOVE WK-CC-NBR           TO WO-CC-NUMBER                  
AL-05         MOVE WK-CC-AUTH          TO WO-CC-AUTH-CODE               
AL-05         MOVE WK-CC-EXP-DATE-NUM  TO WO-CC-EXP-DATE                
AL-05         MOVE WO-INVC-AMT         TO WO-CC-AUTH-AMT.               
AL-05                                                                   
AL-05                                                                   
       FORMAT-WO-INFO.                                                  
      *--------------*                                                  
                                                                        
           PERFORM WRITE-DETAIL-NONB.                                   
           MOVE 'Reservation # '       TO DTL-WO-LIT                    
                                                                        
           MOVE ZEROS                  TO WS-JOB-NBR-NEW
0          MOVE WO-JOB-NBR             TO ws-job-nbr-NEW  
0          MOVE WS-JOB-NBR-15(9:7)     TO ws-job-nbr-7
0          MOVE WS-JOB-NBR-3(2:2)      TO ws-job-nbr-2
           MOVE WS-JOB-NBR             TO DTL-WO-NBR-9.

                                                                        
       FORMAT-MISC-INFO.                                                
      *----------------*                                                
                                                                        
188700     PERFORM WRITE-DETAIL-NONB                                    
                                                                        
188600     IF  NAD-ACCT-NBR NOT = 068397210                             
188800         IF  SSL-CATEGORY = LOW-VALUE                             
CTS-02             IF  WO-PUB = WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB
189000                 MOVE NAD-NAME-1     TO DTL-CLS-SUB-NAME          
189100                 MOVE WO-ADJ-COMMENT TO DTL-CLS-SUB-INFO          
189200             ELSE                                                 
189300                 MOVE 'Adv.:'        TO DTL-SUB-LIT-M             
189400                 MOVE NAD-NAME-1     TO DTL-SUB-NAME-M            
189500         ELSE                                                     
CTS-02             IF  WO-PUB = WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB
189700                 MOVE NAD-NAME-1     TO DTL-CLS-INFO-M            
189800                 MOVE WO-ADJ-COMMENT TO DTL-CLS-SUB-INFO          
189900             ELSE                                                 
190000                 MOVE 'Sub Adv:'     TO DTL-SUB-LIT               
190100                 MOVE NAD-NAME-1     TO DTL-SUB-NAME              
           ELSE                                                         
188600     IF  WO-MAT-COMMENT NOT = SPACES                              
188700         PERFORM WRITE-DETAIL-NONB                                
                                                                        
188800         IF  SSL-CATEGORY = LOW-VALUE                             
CTS-02             IF  WO-PUB = WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB
189800                 MOVE WO-MAT-COMMENT TO DTL-CLS-SUB-NAME          
189100                 MOVE WO-ADJ-COMMENT TO DTL-CLS-SUB-INFO          
189200             ELSE                                                 
189300                 MOVE 'Adv.:'        TO DTL-SUB-LIT-M             
189400                 MOVE NAD-NAME-1     TO DTL-SUB-NAME-M            
189500         ELSE                                                     
CTS-02             IF  WO-PUB = WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB
189800                 MOVE WO-MAT-COMMENT TO DTL-CLS-SUB-NAME          
189100                 MOVE WO-ADJ-COMMENT TO DTL-CLS-SUB-INFO          
189900             ELSE                                                 
190000                 MOVE 'Sub Adv:'     TO DTL-SUB-LIT               
190100                 MOVE NAD-NAME-1     TO DTL-SUB-NAME.             
                                                                        
198500 FORMAT-OTHER.                                                    
198600*------------*                                                    
                                                                        
198700     MOVE 'ADST'                 TO COD-KEY                       
198800     MOVE WO-PUB                 TO COD-CODE1                     
198900     MOVE WO-AD-QUANTITY         TO WK-TEMP-9                     
199000     MOVE WK-TEMP-X              TO COD-CODE2                     
199100     MOVE WO-AD-SIZE             TO COD-CODE3                     
199200     MOVE WO-AD-SHAPE            TO COD-CODE4                     
199300     PERFORM CALL-GIT-COD                                         
           DISPLAY 'STATUS: ' STATUS-CODE ' ' COD-VALX-12.              
199400     IF  STATUS-CODE = ZEROS                                      
199500         MOVE COD-VALX-12        TO WK-DIMENSIONS                 
199600     ELSE                                                         
199700         MOVE 'ADSZ'             TO COD-KEY                       
199800         MOVE WO-PUB             TO COD-CODE1                     
199900         MOVE WO-AD-SIZE         TO COD-CODE2                     
200000         MOVE WO-AD-SHAPE        TO COD-CODE3                     
200100         PERFORM CALL-GIT-COD                                     
                                                                        
200200         IF  STATUS-CODE = ZEROS                                  
200500             MOVE WO-AD-QUANTITY TO WK-DIMENSIONS-1               
200600             MOVE WO-AD-TYPE     TO WK-PFM-IND                    
201100                 MOVE ' X '      TO WK-DIMENSIONS-2               
                       IF COD-FLAG9 = 'M'                               
                          MOVE 'MM'    TO WK-DIMENSIONS-5               
                       END-IF                                           
                       IF COD-FLAG9 = 'I'                               
                          MOVE 'INCH'  TO WK-DIMENSIONS-5               
                       END-IF                                           
                       IF COD-FLAG9 = 'L' OR COD-FLAG9 = ' '            
                          MOVE '       ' TO WK-DIMENSIONS-5             
                       END-IF                                           
201200                 MOVE COD-NAME   TO WK-DIMENSIONS-3.              
201300                                                                  
201400     IF  WK-DIMENSIONS = SPACE                                    
201500         MOVE '*Unknown*'        TO WK-DIMENSIONS.                
201600                                                                  
201700 MOVE-AD-SIZE.                                                    
201800*------------*                                                    
201900                                                                  
202000     SET WK-SIZE-X               TO 1.                            
202100     SET WK-DIM-X                TO 1.                            
CTS-02     SET NT-DIM-X                TO 1.                            
202200     PERFORM MOVE-AD-SIZE-2 4 TIMES.                              
202300                                                                  
202500 MOVE-AD-SIZE-2.                                                  
202600*--------------*                                                  
202700     display  WK-AD-SIZE-X (WK-SIZE-X)                            
202800     IF  WK-AD-SIZE-X (WK-SIZE-X) NOT = SPACE                     
202900         MOVE WK-AD-SIZE-X (WK-SIZE-X)                            
203000                                 TO WK-CLS-DIM-2-X (WK-DIM-X)     
CTS-02         SET NT-DIM-X UP         BY 1                             
203100         SET WK-DIM-X UP         BY 1.                            
203200                                                                  
203300     SET WK-SIZE-X UP            BY 1.                            
203400                                                                  
203600 PROCESS-DETAIL-ACR.                                              
203700*------------------*                                              
203800      display 'PROCESS-DETAIL-ACR '
               WO-ACR-CODE
               
203900     PERFORM WRITE-DETAIL-NONB.                                   
204000                                                                  
204100     MOVE 'ACR'                  TO COD-KEY.                      
204200     MOVE WO-ACR-CODE            TO COD-CODE1                     
204300                                    DTL-CODE-CODE.                
204400                                                                  
204500     MOVE ' - '                  TO DTL-CODE-LIT.                 
204600                                                                  
204700     PERFORM CALL-GIT-COD.                                        
                                                                        
204800     IF  STATUS-CODE = ZEROS                                      
204900         MOVE COD-NAME           TO DTL-CODE-NAME                 
205000     ELSE                                                         
205100         MOVE '** Unknown **'    TO DTL-CODE-NAME.                
205200                                                                  
205300 PROCESS-DETAIL-ADJ.                                              
205400*------------------*                                              
205500        display 'PROCESS-DETAIL-ADJ '
                 WO-ADJ-FLAG ' ' WO-ADJ-PCT ' ' 
                  WO-ADJ-AMT
205600     PERFORM WRITE-DETAIL-NONB.                                   
205700                                                                  
CTS-02*    MOVE 'Adj: '                TO DTL-ADJ-LIT-1                 
AL-20      MOVE 'Adjustment: '         TO DTL-ADJ-LIT-1                 
CTS-02*                                    NT-ADJ-DESC.                 
AL-01A     MOVE COD-NAME               TO  NT-ADJ-DESC.                 
205900                                                                  
206000     IF  WO-ADJ-FLAG = '%'                                        
206100         MOVE WO-ADJ-PCT         TO DTL-ADJ-AMT-P                 
CTS-02                                    NT-ADJ-PCT                    
206200         MOVE ' %'               TO DTL-ADJ-LIT-2                 
CTS-02                                    NT-ADJ-LIT1                   
206300     ELSE                                                         
206400         MOVE WO-ADJ-AMT         TO DTL-ADJ-AMT-D.                
206500                                                                  
206800 PROCESS-DETAIL-INVC.                                             
206900*-------------------*                                             
207000                                                                  
207100     PERFORM WRITE-DETAIL-NONB.                                   
207200                                                                  
207300     MOVE 'INVC'                 TO COD-KEY.                      
207400     MOVE WO-INVC-CODE           TO COD-CODE1                     
207500                                    DTL-CODE-CODE.                
207600                                                                  
207700     MOVE ' - '                  TO DTL-CODE-LIT.                 
207800                                                                  
207900     PERFORM CALL-GIT-COD.                                        
                                                                        
208000     IF  STATUS-CODE = ZEROS                                      
208100         MOVE COD-NAME           TO DTL-CODE-NAME                 
208200     ELSE                                                         
208300         MOVE '** Unknown **'    TO DTL-CODE-NAME.                
208400                                                                  
208600 PROCESS-DETAIL-PROD.                                             
208700*-------------------*                                             
208800                                                                  
208900     PERFORM WRITE-DETAIL-NONB.                                   
209000                                                                  
209100     MOVE WO-CLASS               TO DTL-CODE-CODE.                
209200     MOVE ' - '                  TO DTL-CODE-LIT.                 
209300     MOVE 'PIB'                  TO COD-KEY.                      
209400     MOVE WO-PROD-4              TO COD-CODE1.                    
209500                                                                  
209600     IF  WO-PROD-1 NOT NUMERIC                                    
209700         MOVE WO-PROD-1          TO COD-CODE2.                    
209800                                                                  
209900     PERFORM CALL-GIT-COD.                                        
                                                                        
210000     IF  STATUS-CODE = ZEROS                                      
210100         MOVE COD-NAME           TO DTL-CODE-NAME                 
210200     ELSE                                                         
210300         MOVE '** Unknown **'    TO DTL-CODE-NAME.                
210500                                                                  
210600 PROCESS-DETAIL-DISC.                                                     
210700*-------------------*                                                     
              if  WO-DISC-AMT (WO-DAX) not = 0
210800         display 'PROCESS-DETAIL-DISC '
                       WO-DISC-AMT (WO-DAX)
               end-if
               
210900     IF  WO-DISC-AMT (WO-DAX) NOT = ZEROS                                 
CTS-02         PERFORM PROCESS-DETAIL-DISC-2                                    
CTS-02         MOVE '2'                TO NT-CODE-2A                            
CTS-02         MOVE WK-EU-PG-FLG       TO NT-CODE-2B                            
CTS-02         MOVE WK-JOB-SEQ-CNT     TO NT-JOB-SEQ                            
CTS-02         ADD  1                  TO WK-LINE-CNT                           
CTS-02         MOVE WK-LINE-CNT        TO NT-LVL-C                              
CTS-02                                    NT-LVL-I                              
CTS-02         MOVE '02'               TO NT-TAB2-P3                            
CTS-02         MOVE '07'               TO NT-TAB2-P10                           
CTS-02         PERFORM CHECK-AND-LOAD-DISC-AMT                                  
CTS-02     END-IF.                                                              
CTS-02                                                                          
CTS-02     SET DSC-X                   UP BY 1.                                 
211200     SET WO-DAX WO-DCX           UP BY 1.                                 
211300                                                                          
211500 PROCESS-DETAIL-DISC-2.                                                   
211600*---------------------*                                                   
211700                                                                          
211800*>   Detail line Discounts                                                
211900                                                                          
212000     PERFORM WRITE-DETAIL-NONB.                                           
212100                                                                          
212200     MOVE 'DISC'                 TO COD-KEY.                              
212300     MOVE WO-PUB                 TO COD-CODE1.                            
212400     MOVE WO-AD-TYPE             TO COD-CODE2                             
212500                                    INPUT-CHARS.                          
212600     MOVE WO-DISC-CODE (WO-DCX)  TO COD-CODE3.                            
212700     PERFORM MATCH-WILDCARD.                                              
212800                                                                          
212900     IF  STATUS-CODE = ZEROS                                              
213000         MOVE COD-NAME           TO DTL-DESCRIPTION                       
CTS-02                                    NT-DSC-CHG-DESC                       
213100     ELSE                                                                 
213200         MOVE WO-AD-TYPE         TO COD-CODE2                             
213300                                    INPUT-CHARS                           
213400         MOVE WO-EDITION         TO COD-CODE4                             
213500         PERFORM MATCH-WILDCARD                                           
                                                                                
213600         IF  STATUS-CODE = ZEROS                                          
213700             MOVE COD-NAME       TO DTL-DESCRIPTION                       
CTS-02                                    NT-DSC-CHG-DESC                       
213800         ELSE                                                             
213900             MOVE WO-DISC-CODE (WO-DCX)                                   
CTS-02                                 TO DTL-DESCRIPTION                       
CTS-02                                    NT-DSC-CHG-DESC.                      
214100                                                                          
214200     MOVE WO-DISC-AMT (WO-DAX)   TO DTL-GROSS-AMT.                        
214300     COMPUTE DTL-GROSS-AMT = - DTL-GROSS-AMT.                             
                                                                                
214400     ADD DTL-GROSS-AMT           TO TTL-DISC-AMT                          
********************                      TTL-GROSS-AMT                         
CTS-02                                    TTL-AGY-ADV-GROSS-AMT                 
CTS-02                                    TTL-AGY-ADV-GROSS-AMT-T               
CTS-02                                    TTL-AGY-ADV-D-P-AMT                   
CTS-02                                    TTL-TOT-AGY-D-P-AMT                   
214600                                    TTL-AGY-GROSS-AMT                     
CTS-02                                    WK-D-P-AMT                            
214700                                    WK-DISCOUNTS.                         
214800     MOVE DTL-GROSS-AMT          TO DTL-GROSS-AMT-P                       
CTS-02                                    NT-DISP-NET-AMT                       
CTS-02                                    NT-DISP-D-P1-AMT(DSC-X)               
CTS-02                                    NT-DISC-AMT.                          
214900                                                                  
215100 PROCESS-DETAIL-SPCH.                                             
215200*-------------------*                                             
              if  WO-CHARG-AMT (WO-SAX) not = 0
215300         display 'PROCESS-DETAIL-SPCH '
                   WO-CHARG-AMT (WO-SAX)
              end-if
                   
215400     IF  WO-CHARG-AMT (WO-SAX) NOT = ZEROS                        
CTS-02         PERFORM PROCESS-DETAIL-SPCH-2                            
CTS-02         MOVE '2'                TO NT-CODE-2A                    
CTS-02         MOVE WK-EU-PG-FLG       TO NT-CODE-2B                    
CTS-02         MOVE WK-JOB-SEQ-CNT     TO NT-JOB-SEQ                    
CTS-02         ADD  1                  TO WK-LINE-CNT                   
CTS-02         MOVE WK-LINE-CNT        TO NT-LVL-C                      
CTS-02                                    NT-LVL-I                      
CTS-02         MOVE '02'               TO NT-TAB2-P3                    
CTS-02         MOVE '07'               TO NT-TAB2-P10                   
CTS-02         PERFORM CHECK-AND-LOAD-SPL-AMT                           
CTS-02     END-IF.                                                      
CTS-02                                                                  
CTS-02     SET SPL-X                   UP BY 1.                         
215700     SET WO-SAX WO-SCX           UP BY 1.                         
215800                                                                  
216000 PROCESS-DETAIL-SPCH-2.                                           
216100*---------------------*                                           
216200                                                                  
216300*>   Detail line Production charges                               
216400                                                                  
216500     PERFORM WRITE-DETAIL-NONB.                                   
216600                                                                  
216700     MOVE 'PROD'                 TO COD-KEY.                      
216800     MOVE WO-CHARG-CODE (WO-SCX) TO COD-CODE1.                    
216900     PERFORM CALL-GIT-COD.                                        
217000                                                                  
217100     IF  STATUS-CODE NOT = ZEROS                                  
217200         MOVE WO-CHARG-CODE (WO-SCX) TO DTL-DESCRIPTION           
CTS-02                                        NT-DSC-CHG-DESC           
217300     ELSE                                                         
CTS-02         MOVE COD-NAME               TO DTL-DESCRIPTION           
CTS-02                                        NT-DSC-CHG-DESC.          
217500                                                                  
218300     MOVE WO-CHARG-AMT (WO-SAX)  TO DTL-GROSS-AMT.                        
218400     ADD DTL-GROSS-AMT           TO TTL-SPCH-AMT                          
******************                        TTL-GROSS-AMT                         
CTS-02                                    TTL-AGY-ADV-GROSS-AMT                 
CTS-02                                    TTL-AGY-ADV-GROSS-AMT-T               
CTS-02                                    TTL-AGY-ADV-SPL-CHG                   
CTS-02                                    TTL-TOT-AGY-SPL-CHG                   
CTS-02                                    WK-SPL-AMT                            
218600                                    TTL-AGY-GROSS-AMT.                    
218700     MOVE DTL-GROSS-AMT          TO DTL-GROSS-AMT-P                       
CTS-02                                    NT-DISP-SPL1-AMT(SPL-X)               
CTS-02                                    NT-DISP-NET-AMT                       
CTS-02                                    NT-DISC-AMT.                          
218800                                                                  
219000 PROCESS-DETAIL-POSN.                                             
219100*-------------------*                                             
219200                                                                  
219300*>   Detail line Position & Premiun charges                       
219400                                                                  
219500     PERFORM WRITE-DETAIL-NONB.                                   
219600                                                                  
219700     IF COD-FLAG4 = '$'                                           
219800        MOVE COD-NAME             TO DTL-DESC-POSN                
219900        DIVIDE COD-VAL (COD-X)    BY 100 GIVING DTL-POSN-RATE     
220000     ELSE                                                         
220100     IF COD-FLAG4 = '%'                                           
220200        MOVE COD-NAME             TO DTL-DESC-POSN                
220300        MOVE COD-FLAG4            TO DTL-POSN-LIT                 
                                                                        
220400        IF COD-VAL (COD-X) > 10000                                
220500           SUBTRACT 10000 FROM COD-VAL (COD-X)                    
220700           DIVIDE COD-VAL (COD-X) BY 100 GIVING DTL-POSN-PCT      
220800         ELSE                                                     
221000           DIVIDE COD-VAL (COD-X) BY 100 GIVING DTL-POSN-PCT      
221100     ELSE                                                         
221200     IF COD-FLAG4 = 'T'                                           
221300        MOVE COD-NAME             TO DTL-DESC-POSN                
221400        DIVIDE COD-VAL (COD-X)    BY 100 GIVING DTL-POSN-RATE     
221500     ELSE                                                         
221600     IF COD-FLAG4 = '1'                                           
221700        MOVE COD-NAME             TO DTL-DESC-POSN                
221800        DIVIDE COD-VAL (COD-X)    BY 100 GIVING DTL-POSN-RATE     
221900     ELSE                                                         
222000        MOVE COD-NAME             TO DTL-DESCRIPTION.             
222100                                                                  
222300 PROCESS-DETAIL-SR.                                               
222400*-----------------*                                               
222500                                                                  
222600*>   Detail line split run percent                                
222700                                                                  
222800     PERFORM WRITE-DETAIL-NONB.                                   
222900                                                                  
223000     MOVE 'Tie-in Ad: '          TO DTL-SR-LIT-1.                 
223100     MOVE WO-SPLIT-PCT           TO DTL-SR-PCT.                   
223200     MOVE '% of Size'            TO DTL-SR-LIT-2.                 
223300                                                                  
223500 PROCESS-HEADER.                                                  
223600*--------------*                                                  
           display 'process header'.   
              display 'ssl-parm-dates ' ssl-parm-dates
223800     MOVE SPACES                 TO HDR-LINES                     
223900                                    DTL-LINES                     
224000                                    FOOTER-LINES                  
224200                                    WK-REF-FIELD.                 
CTS-02     MOVE SPACE                  TO WK-NEW-TECH-REC               
CTS-02                                    WK-AGY-KEY                    
CTS-02                                    WK-AGY-JXRF-AREA              
CTS-02                                    WK-ADV-JXRF-AREA              
CTS-02                                    WK-HOLD-ADV-NBR               
CTS-02                                    WK-HOLD-AGY-NBR               
CTS-02                                    WK-ADV-KEY.                   
CTS-02     MOVE ZEROS                  TO WK-DATE1.                     
CTS-02     MOVE SPACES                 TO WK-MSG-CD3.                   
224400     PERFORM GET-AGY-ADV.                                         
                                                                        
224500*>   NA2-REC now has Bill-to Name, Address Etc...                 
224700*>   Header line                                                  
224800                                                                  
224900**     PERFORM GET-PAPER-ADDR.      

           if  ssl-parm-dates not = space
043300         move ssl-PARM-INVC-DATE-P to WK-INVC-DATE-P
043300         move ssl-PARM-INVC-DATE-F to WK-INVC-DATE-f
043300         move ssl-PARM-INVC-DATE-T to WK-INVC-DATE-t
           end-if

225000*>   NAD-REC now has return Name Address Etc...                   
225100   
           display 'WK-INVC-DATE-p ' WK-INVC-DATE-P
           display 'WK-INVC-DATE-f ' WK-INVC-DATE-f
           display 'WK-INVC-DATE-f ' WK-INVC-DATE-t

225200     MOVE WK-INVC-DATE-P         TO UTL-EDIT-DATE.                
225300     PERFORM EDIT-OUT-DATE.                                       
225400     MOVE UTL-EDIT-DATE-SL       TO HDR-BILLING-DATE.             
CTS-02     MOVE HDR-BILL-MM            TO WK-MM                         
CTS-02                                    WK-MM-1                       
CTS-02                                    NT-BILL-MM.                   
CTS-02     MOVE '/'                    TO NT-BILL-LA-1.                 
CTS-02     MOVE HDR-BILL-DD            TO WK-DD                         
CTS-02                                    WK-DD-1                       
CTS-02                                    NT-BILL-DD.                   
CTS-02     MOVE '/'                    TO NT-BILL-LA-2.                 
CTS-02     MOVE HDR-BILL-YY            TO WK-YY                         
CTS-02                                    WK-YY-1                       
CTS-02                                    NT-BILL-YY2.                  
CTS-02*                                                                 
CTS-02     IF WK-YY-1 < 83                                              
CTS-02        MOVE '20'                TO NT-BILL-YY1                   
CTS-02     ELSE                                                         
CTS-02        MOVE '19'                TO NT-BILL-YY1                   
CTS-02     END-IF.                                                      
225800                                                                  
225900     MOVE WK-INVC-TYPE           TO FOOT-DOCUMENT-1.              
226000     MOVE WK-INVC-NBR            TO FOOT-DOCUMENT-3.              
226100                                                                  
226200     MOVE SSL-INVC-DATE          TO UTL-EDIT-DATE                 
226300                                    WK-PFM-CUT-OFF.               
226400     PERFORM EDIT-OUT-DATE.                                       
226500     MOVE UTL-EDIT-DATE-SL       TO HDR-BILLING-TO                
226600                                    FOOT-BILLING-TO.              
226700                                                                  
226500     MOVE WK-INVC-DATE-T         TO HDR-BILLING-TO                
226600                                    FOOT-BILLING-TO.              
226700                                                                  
228300     MOVE WK-INVC-DATE-T (1:2)   TO NT-BILL-MM-T                  
228300     MOVE WK-INVC-DATE-T (4:2)   TO NT-BILL-DD-T                  
228300     MOVE WK-INVC-DATE-T (7:2)   TO NT-BILL-YY2-T                 
228300     MOVE '/'                    TO NT-BILL-LA-1T                 
228300     MOVE '/'                    TO NT-BILL-LA-2T                 
CTS-02     IF NT-BILL-YY2-T < 83                                        
CTS-02        MOVE '20'                TO NT-BILL-YY1-T                 
CTS-02     ELSE                                                         
CTS-02        MOVE '19'                TO NT-BILL-YY1-T                 
CTS-02     END-IF.                                                      
228300                                                                  
226800     MOVE UTL-YY-SL              TO FOOT-DOCUMENT-2.              
226900     MOVE FOOT-DOCUMENT          TO WK-DOCUMENT.                  
227000     MOVE ' - '                  TO HDR-THRU                      
227100                                    FOOT-THRU.                    
227200                                                                  
227300     COMPUTE UTL-EDIT-DATE = SSL-INVC-DATE / 100.                 
227400     COMPUTE UTL-EDIT-DATE = UTL-EDIT-DATE * 100 + 1.             
227500                                                                  
227600     MOVE UTL-EDIT-DATE          TO WK-MO-FROM                    
227700                                    WK-MO-TO.                     
227800     MOVE 31                     TO WK-MO-TO-DD.                  
227900                                                                  
228000     PERFORM EDIT-OUT-DATE.                                       
228100     MOVE UTL-EDIT-DATE-SL       TO HDR-BILLING-FROM              
228200                                    FOOT-BILLING-FROM.            
228300                                                                  
228100     MOVE WK-INVC-DATE-F         TO HDR-BILLING-FROM              
228200                                    FOOT-BILLING-FROM.            
228300                                                                  
228300     MOVE WK-INVC-DATE-F (1:2)   TO NT-BILL-MM-F                  
228300     MOVE WK-INVC-DATE-F (4:2)   TO NT-BILL-DD-F                  
228300     MOVE WK-INVC-DATE-F (7:2)   TO NT-BILL-YY2-F                 
228300     MOVE '/'                    TO NT-BILL-LA-1F                 
228300     MOVE '/'                    TO NT-BILL-LA-2F                 
CTS-02     IF NT-BILL-YY2-F < 83                                        
CTS-02        MOVE '20'                TO NT-BILL-YY1-F                 
CTS-02     ELSE                                                         
CTS-02        MOVE '19'                TO NT-BILL-YY1-F                 
CTS-02     END-IF.                                                      
228300                                                                  
228400     MOVE 'LIN1'                 TO WK-TEMP-X.                    
228500     PERFORM GET-INVOICE-TERMS.                                   
228600     MOVE MSG-TEXT               TO HDR-TERMS-LIN1.               
228700                                                                  
228800*>   Return Address & info                                        
229000     MOVE NAD-NAME-1             TO HDR-NAM2.                     
229100     MOVE NAD-STREEt-1           TO HDR-STREET.                   
229300     MOVE NAD-CITY-1             TO HDR-CITY.                     
229500     MOVE NAD-STATE (1)          TO HDR-STATE.                    
229700     MOVE NAD-ZIP (1)            TO WK-ZIP-2.                     
229800     MOVE WK-ZIP-2-5             TO HDR-ZIP-5.                    
                                                                        
229900     IF  WK-ZIP-2-4 NOT = SPACES                                  
230000         MOVE '-'                TO HDR-ZIP-SL                    
230100         MOVE WK-ZIP-2-4         TO HDR-ZIP-4.                    
230200                                                                  
230300     IF  SSL-PUB = 'WEB '                                         
230301         MOVE '2125978044'       TO NAD-TEL-NBR.                  
378900                                                                  
                                                                        
230302     MOVE NAD-TEL-NBR            TO WK-TEMP-TEL-NBR.              
230400     MOVE '('                    TO HDR-TEL-A.                    
230500     MOVE WK-TEMP-TEL-1          TO HDR-TEL-1.                    
230600     MOVE ')'                    TO HDR-TEL-B.                    
230700     MOVE WK-TEMP-TEL-2          TO HDR-TEL-2.                    
230800     MOVE '-'                    TO HDR-TEL-C.                    
230900     MOVE WK-TEMP-TEL-3          TO HDR-TEL-3.                    
231000                                                                  
231100     MOVE NAD-NAME-2             TO HDR-REP-NAME.                 
231200                                                                  
231300     MOVE ZEROS                  TO WK-ADV-NBR.                   
231400                                                                  
231500     IF  SSL-CATEGORY = LOW-VALUE                                 
231600         NEXT SENTENCE                                            
231700     ELSE                                                         
231800     IF  SSL-AGY-ACCT-NBR NOT = SSL-ADV-PAR-NBR                   
231900         MOVE SSL-ADV-PAR-NBR    TO WK-ADV-NBR                    
232000     ELSE                                                         
232100     IF  SSL-ADV-PAR-NBR  NOT = SSL-ACCT-NBR                      
232200         MOVE SSL-ACCT-NBR       TO WK-ADV-NBR.                   
232300                                                                  
232700*>   Header Line                                                  
233300                                                                  
233400     IF  WK-SAVE-FLAG = 'Y'                                       
233500         MOVE 'N'                TO WK-SAVE-FLAG                  
233600         MOVE WK-GL-SUBLED       TO WK-SAVE-MAJOR                 
233700         MOVE WK-GL-LED          TO WK-SAVE-MINOR.                
233900     MOVE NA2-ACCT-NBR           TO NAD-ACCT-NBR.                 
234000     PERFORM GET-J-XREF.                                          
                                                                        
234100     MOVE WK-JXRF-AREA           TO WK-AGY-JXRF-AREA.             
234200     MOVE WK-AGY-JXRF            TO HDR-AGY-NBR-X                 
234300                                    FOOT-AGY-NBR-X.               
234400                                                                  
234500     IF  SSL-ADV-PAR-NBR     = SSL-AGY-ACCT-NBR                   
234600     AND SSL-ADV-PAR-NBR NOT = SSL-ACCT-NBR                       
234700         NEXT SENTENCE                                            
234800     ELSE                                                         
234900     IF  WK-ADV-NBR NOT = ZEROS                                   
235000         MOVE WK-ADV-NBR         TO NAD-ACCT-NBR                  
235100         PERFORM GET-J-XREF                                       
                                                                        
AL-21 *        MOVE WK-JXRF-AREA       TO WK-ADV-JXRF-AREA.             
AL-21          MOVE WK-JXRF-AREA       TO WK-ADV-JXRF-AREA              
235300         MOVE WK-ADV-JXRF        TO HDR-ADV-NBR-X                 
235400                                    FOOT-ADV-NBR-X                
235500         IF  SSL-NAT-AGY = 'Y'                                    
235600             MOVE WK-ADV-JXRF-2  TO WK-REF-ACCT.                  
235700                                                                  
235800     IF  WK-GL-SUBLED = '16'                                      
235900         MOVE SSL-NAD-TEL-NBR    TO HDR-AGY-NBR-X                 
236000                                    FOOT-AGY-NBR-X.               
236100*>   Billing Address                                              
236200                                                                  
236300     MOVE NA2-NAMe-1             TO HDR-AGY-NAM1                  
236400                                    HDR-ADV-NAME.                 
236500     MOVE NA2-NAMe-2             TO HDR-AGY-NAM2                  
     ***   MOVE NA2-NAMe-3             TO HDR-AGY-NAM3                  
236600     MOVE NA2-STREET-1           TO HDR-AGY-STREET                
236700     MOVE NA2-CITY-1             TO HDR-AGY-CITY                  
236800     MOVE NA2-STATE (1)          TO HDR-AGY-STATE                 
236900     MOVE NA2-ZIP (1)            TO WK-ZIP-2                      
237000     MOVE WK-ZIP-2-5             TO HDR-AGY-ZIP-5                 
                                                                        
237100     IF  WK-ZIP-2-4 NOT = SPACES                                  
237200         MOVE '-'                TO HDR-AGY-ZIP-SL                
237300         MOVE WK-ZIP-2-4         TO HDR-AGY-ZIP-4.                
237400                                                                  
237500     IF  NA2-CNTRY (1) NOT = SPACE                                
237600         MOVE NA2-CNTRY (1)      TO HDR-AGY-CNTRY.                
237700*                                                                 
237900     MOVE ZEROS                  TO NAD-ACCT-NBR.                 
238000                                                                  
238100     IF  SSL-CATEGORY     =  LOW-VALUE                            
238200     AND WK-GL-SUBLED NOT = '16'                                  
238300         MOVE 'Miscellaneous Advertiser(s) '                      
238400                                 TO HDR-ADV-NAME                  
238500                                    FOOT-ADV-NAME                 
238300         MOVE SPACE              TO WK-SORT-NAME                  
238600     ELSE                                                         
238700     IF  SSL-AGY-ACCT-NBR NOT = SSL-ADV-PAR-NBR                   
238800         MOVE SSL-ADV-PAR-NBR    TO NAD-ACCT-NBR                  
238900     ELSE                                                         
239000     IF  SSL-ADV-PAR-NBR  NOT = SSL-ACCT-NBR                      
239100         MOVE SSL-ACCT-NBR       TO NAD-ACCT-NBR.                 
239200                                                                  
239300     IF  NAD-ACCT-NBR NOT = ZEROS                                 
239400         PERFORM GET-ADV                                          
239500         MOVE NAD-NAME-1         TO HDR-ADV-NAME                  
239600                                    FOOT-ADV-NAME                 
239600                                    WK-SORT-NAME.                 
239800     IF  SSL-NAT-AGY = 'Y'                                        
239900         MOVE HDR-ADV-NAME       TO WK-REF-NAME.                  
240000                                                                  
240100     IF  NA2-ROUTE = 'NYT     '                                   
240200         MOVE 'N'                TO HDR-1-ROUTE                   
240300                                    WK-ROUTE                      
240400     ELSE                                                         
240500     IF  NA2-ROUTE = 'UPS     '                                   
240600         MOVE 'U'                TO HDR-1-ROUTE                   
240700                                    WK-ROUTE                      
240800     ELSE                                                         
240900     IF  NA2-ROUTE = 'IMC     '                                   
241000         MOVE 'I'                TO HDR-1-ROUTE                   
241100                                    WK-ROUTE                      
241200     ELSE                                                         
241300         MOVE 'M'                TO HDR-1-ROUTE                   
241400                                    WK-ROUTE.                     
241500                                                                  
241600     IF  NA2-CNTRY (1) NOT = SPACE                                
241700         MOVE 'L'                TO HDR-1-ROUTE                   
241800                                    WK-ROUTE.                     
241900                                                                  
           IF  NA2-ACCT-NBR > 499999999                                 
233900         MOVE NA2-ACCT-NBR       TO HDR-AGY-NBR-9                 
233900         MOVE NAD-ACCT-NBR       TO HDR-ADV-NBR-9                 
           ELSE                                                         
233900         MOVE NA2-ACCT-NBR       TO HDR-ADV-NBR-9.                
                                                                        
242000     PERFORM PROCESS-HEADER-3.                                    
242100                                                                  
242200     IF  WK-PAGE-CTR = 1                                          
242300         PERFORM PUT-INVC-MSGS.                                   
242400                                                                  
242500     MOVE 'NMTI'                 TO COD3-KEY.                     
242600     MOVE WK-AGY-JXRF-4-1        TO COD3-CODE1.                   
242700     MOVE WK-AGY-JXRF-2-2        TO COD3-CODE2.                   
242800     PERFORM CALL-GIT-COD3.                                       
242900                                                                  
243000     IF  STATUS-CODE =  ZEROS                                     
243100     AND COD3-FLAG1  = 'Y'                                        
243200         MOVE 'Y'                TO WK-PRODUCE-TAPE               
243300     ELSE                                                         
243400         MOVE 'NMTI'             TO COD3-KEY                      
243500         MOVE WK-ADV-JXRF-4-1    TO COD3-CODE1                    
243600         MOVE WK-ADV-JXRF-2-2    TO COD3-CODE2                    
243700         PERFORM CALL-GIT-COD3                                    
                                                                        
243800         IF  STATUS-CODE =  ZEROS                                 
243900         AND COD3-FLAG1  = 'Y'                                    
244000             MOVE 'Y'            TO WK-PRODUCE-TAPE               
244100         ELSE                                                     
244200             MOVE 'N'            TO WK-PRODUCE-TAPE.              
244300                                                                  
244400     IF  WK-PRODUCE-TAPE = 'Y'                                    
244500         PERFORM MAG-TAPE-0-REC.                                  
244600                                                                  
      ***  IF  WK-GL-SUBLED = '11' OR '16' OR '37'                      
      ***      NEXT SENTENCE                                            
      ***  ELSE                                                         
244700***      PERFORM PROCESS-AR-HEADER.                               
244800                                                                  
244900 PROCESS-HEADER-2.                                                
245000*----------------*                                                
245100                                                                  
AL-06      DISPLAY 'PROCESS-hdr-2: ' SSL-ACCT-NBR                       
AL-06         ' ' PROCESS-GROUP-CODE                                    
AL-06         ' ' detail-data.                                          
245200     MOVE DTL-LINE               TO DTL-LINE-SAVE.                
245300     MOVE SPACES                 TO DTL-LINE.                     
AL-06      IF DETAIL-DATA = 'Y'                                         
245400        MOVE 'Continued...'      TO DTL-NET-AMT-X                 
AL-06      ELSE                                                         
AL-06         MOVE 'Ignore......'      TO DTL-NET-AMT-X.                
245500     PERFORM WRITE-DETAIL-LINE-2.                                 
245600                                                                  
245700     MOVE '5'                    TO DTL-1-CB.                     
245800     MOVE '4'                    TO DTL-1-FONT.                   
245900     MOVE DTL-LINE               TO INVOICE-REC.                  
246000     PERFORM WRITE-LINE.                                          
246100                                                                  
246200     PERFORM WRITE-FOOTER-6.                                      
246300                                                                  
AL-06      IF PROCESS-GROUP-CODE = 'Y'                                  
AL-06         MOVE 'G'             TO WK-GROUP-BUY.                     
AL-06                                                                   
246400     IF  WK-PAGE-CTR = 1                                          
246500         PERFORM PROCESS-HEADER-LIT.                              
246600                                                                  
246700     PERFORM WRITE-PAGE-TABLE.                                    
246800                                                                  
246900     PERFORM PROCESS-HEADER-3.                                    
247000                                                                  
247100     MOVE DTL-LINE-SAVE          TO DTL-LINE.                     
247200     MOVE '4'                    TO DTL-1-CB.                     
247300                                                                  
247400 PROCESS-HEADER-LIT.                                              
247500*------------------*                                              
                                                                        
247600     MOVE SPACE                  TO WK-CB.                        
247700     MOVE '1'                    TO WK-FONT.                      
247800     PERFORM WRITE-BLANK-LINE.                                    
                                                                        
247900     MOVE WK-SEP-TEXT            TO DTL-LINE.                     
248000     MOVE '1'                    TO DTL-1-FONT.                   
248100     MOVE DTL-LINE               TO INVOICE-REC.                  
                                                                        
248200     PERFORM WRITE-LINE.                                          
248300     MOVE SPACES                 TO DTL-LINE.                     
248400                                                                  
248500 PROCESS-HEADER-3.                                                
248600*----------------*                                                
248700                                                                  
248800     MOVE SPACE                  TO PAGE-TABLE.                   
248900     MOVE ZEROS                  TO PAGE-TABLE-COUNT.             
249000     MOVE 'Y'                    TO WK-FIRST-LINE.                
249100     ADD 1                       TO TTL-PAGE-CTR                  
249200                                    WK-PAGE-CTR.                  
249300     MOVE WK-PAGE-CTR            TO HDR-PAGE-NBR.                 
249400                                                                  
249500     MOVE '1'                    TO HDR-1-CB.                     
249600     MOVE '1'                    TO HDR-1-FONT.                   
249700     MOVE HDR-LINE-1             TO INVOICE-REC.                  
249800     PERFORM WRITE-LINE.                                          
249900                                                                  
250000     MOVE SPACE                  TO WK-CB.                        
250100     MOVE '1'                    TO WK-FONT.                      
250200     PERFORM WRITE-BLANK-LINE.                                    
                                                                        
250300     MOVE '1'                    TO HDR-2-FONT.                   
250400     MOVE HDR-LINE-2             TO INVOICE-REC.                  
250500     PERFORM WRITE-LINE.                                          
250600                                                                  
250700     MOVE SPACE                  TO WK-CB.                        
250800     MOVE '1'                    TO WK-FONT.                      
250900     PERFORM WRITE-BLANK-LINE.                                    
                                                                        
251000     MOVE '1'                    TO HDR-3-FONT.                   
251100     MOVE HDR-LINE-3             TO INVOICE-REC.                  
251200     PERFORM WRITE-LINE.                                          
251300                                                                  
251400     MOVE '2'                    TO HDR-4-CB.                     
251500     MOVE '1'                    TO HDR-4-FONT.                   
251600     MOVE HDR-LINE-4             TO INVOICE-REC.                  
251700     PERFORM WRITE-LINE.                                          
251800                                                                  
251900     MOVE '1'                    TO HDR-5-FONT.                   
252000     MOVE HDR-LINE-5             TO INVOICE-REC.                  
252100     PERFORM WRITE-LINE.                                          
252200                                                                  
252300     MOVE '1'                    TO HDR-6-FONT.                   
252400     MOVE HDR-LINE-6             TO INVOICE-REC.                  
252500     PERFORM WRITE-LINE.                                          
252600                                                                  
252700     MOVE '1'                    TO HDR-7-FONT.                   
252800     MOVE HDR-LINE-7             TO INVOICE-REC.                  
252900     PERFORM WRITE-LINE.                                          
CTS-02     MOVE '1'                    TO HDR-7-A-FONT.                 
CTS-02     MOVE HDR-LINE-7-A           TO INVOICE-REC.                  
CTS-02     PERFORM WRITE-LINE.                                          
253000                                                                  
250700     MOVE SPACE                  TO WK-CB.                        
250800     MOVE '1'                    TO WK-FONT.                      
250900     PERFORM WRITE-BLANK-LINE.                                    
                                                                        
253200     MOVE '1'                    TO HDR-8-FONT.                   
253300     MOVE HDR-LINE-8             TO INVOICE-REC.                  
253400     PERFORM WRITE-LINE.                                          
253500                                                                  
253100     MOVE '3'                    TO HDR-9-CB.                     
253200     MOVE '1'                    TO HDR-9-FONT.                   
253300     MOVE HDR-LINE-9             TO INVOICE-REC.                  
253400     PERFORM WRITE-LINE.                                          
253500                                                                  
253600     MOVE '4'                    TO DTL-1-CB.                     
253700     MOVE 1                      TO WK-LINE-CTR.                  
253800                                                                  
253900 PROCESS-DUMMY-PAGES.                                             
254000*-------------------*                                             
254100                                                                  
254200     PERFORM PROCESS-DUMMY-INIT.                                  
254300     PERFORM PROCESS-DUMMY-PAGE  15 TIMES.                        
254400                                                                  
254500     MOVE SPACES                 TO HDR-LINES                     
254600                                    DTL-LINE                      
254700                                    WK-FIRST-LINE.                
254800     MOVE 1                      TO PRT-SKIP.                     
254900     MOVE 1                      TO WK-PAGE-CTR                   
255000                                    WK-LINE-CTR                   
255100                                    TTL-PAGE-CTR.                 
255200                                                                  
255400 PROCESS-DUMMY-INIT.                                              
255500*------------------*                                              
255600                                                                  
255700     MOVE SPACES                 TO HDR-LINES                     
255800                                    DTL-LINE                      
255900                                    FOOTER-LINES.                 
256000     MOVE ALL 'X'                TO HDR-BILLING-FROM              
256100                                    HDR-THRU                      
256200                                    HDR-BILLING-TO                
256300                                    HDR-ADV-NAME                  
256400                                    HDR-TERMS-LIN1                
256700                                    HDR-AGY-NBR-X                 
256800                                    HDR-ADV-NBR-X                 
256900                                    HDR-BILL-MM                   
257000                                    HDR-BILL-DD                   
257100                                    HDR-BILL-YY                   
257200                                    HDR-AGY-NAM1                  
257300                                    HDR-NAM1                      
257400                                    HDR-AGY-NAM2                  
257500                                    HDR-NAM2                      
257600                                    HDR-AGY-STREET                
257700                                    HDR-STREET                    
257800                                    HDR-AGY-CITY                  
257900                                    HDR-AGY-STATE                 
258000                                    HDR-AGY-ZIP                   
258100                                    HDR-CITY                      
258200                                    HDR-STATE                     
258300                                    HDR-ZIP                       
CTS-02                                    HDR-CNTRY                     
258400                                    HDR-TEL-NBR                   
258500                                    HDR-REP-NAME.                 
258600                                                                  
258700     MOVE '/'                    TO HDR-BILL-SL1                  
258800                                    HDR-BILL-SL2.                 
258900                                                                  
259000     MOVE ZEROS                  TO HDR-PAGE-NBR.                 
259100     MOVE -999999999.99          TO HDR-AMOUNT.                   
259200                                                                  
259300     MOVE SPACES                 TO DTL-LINE.                     
259400     MOVE ALL 'X'                TO DTL-SUNDAY                    
259500                                    DTL-ISSUE                     
259600                                    DTL-REF-NBR-9                 
259700                                    DTL-DESCRIPTION               
259800                                    DTL-INFO-AREA.                
259900     MOVE 9999.99                TO DTL-AVG-RATE                  
260000     MOVE -999999.99             TO DTL-GROSS-AMT-P               
260100                                    DTL-NET-AMT-P.                
260200                                                                  
260300     MOVE -999999999.99          TO FOOT-TOT-CURR-AMT             
260400                                    FOOT-TOT-AMT.                 
260500                                                                  
260600     MOVE ALL 'X'                TO FOOT-DOCUMENT                 
260700                                    FOOT-BILLING-FROM             
260800                                    FOOT-THRU                     
260900                                    FOOT-BILLING-TO               
261200                                    FOOT-AGY-NBR-X                
261300                                    FOOT-ADV-NBR-X                
261400                                    FOOT-ADV-NAME.                
261500                                                                  
261600     MOVE LOW-VALUES             TO NA2-ROUTE                     
261700                                    NA2-TYPE.                     
261800                                                                  
261900 PROCESS-DUMMY-PAGE.                                              
262000*------------------*                                              
262100                                                                  
262200     PERFORM PROCESS-HEADER-3.                                    
262300     PERFORM PROCESS-DUMMY-DTL 5 TIMES.                           
262400                                                                  
262500     PERFORM WRITE-FOOTER-5.                                      
262600     PERFORM WRITE-FOOTER-6.                                      
262800     PERFORM WRITE-PAGE-TABLE.                                    
262900                                                                  
263000 PROCESS-DUMMY-DTL.                                               
263100*-----------------*                                               
263200                                                                  
263300     MOVE DTL-LINE               TO INVOICE-REC.                  
263400     PERFORM WRITE-LINE.                                          
263500     MOVE SPACE                  TO DTL-1-CB.                     
263600                                                                  
263800 GET-AGY-ADV.                                                     
263900*-----------*                                                     
264100     MOVE SSL-ACCT-NBR           TO NAD-ACCT-NBR.                 
264200     PERFORM GET-ADV.                                             
CTS-02     PERFORM PRINT-ADV-DETAILS.                                   
264300                                                                  
264400*>   Get parent info into parent table                            
                                                                        
264500     PERFORM GET-PARENT-INFO.                                     
264600                                                                  
264700*>   Get Bta Agy (or Adv if direcr) into NA2-REC.                 
                                                                        
264800     IF  SSL-AGY-ACCT-NBR NOT = SSL-ACCT-NBR                      
264900         MOVE SSL-AGY-ACCT-NBR   TO NA2-ACCT-NBR                  
265000         PERFORM CALL-GIT-NA2                                     
                                                                        
265100         IF STATUS-CODE NOT = ZEROS           
               move 1                  to return-code
265200            CALL 'AMSABRT' USING IN10                             
265300         ELSE                                                     
CTS-02            PERFORM PRINT-AGY-DETAILS                             
CTS-02*           NEXT SENTENCE                                         
265500     ELSE                                                         
CTS-02         MOVE 'Y'                TO WK-NO-AGY                     
265600         MOVE NAD-REC            TO NA2-REC.                      
265700                                                                  
265800*>   Get bill to address into NA2-REC                             
266000                                                                  
266200 GET-PAPER-ADDR.                                                  
266300*--------------*                                                  
266400     MOVE 'NATP'                 TO COD-KEY.                      
266500                                                                  
266600     IF  NA2-ACCT-NBR < 500000000                                 
266700         MOVE 'ADV'              TO COD-CODE1                     
266800     ELSE                                                         
266900         MOVE 'AGY'              TO COD-CODE1.                    
267000                                                                  
267100     MOVE NA2-TYPE               TO COD-CODE2.                    
267200     PERFORM CALL-GIT-COD.                                        
267300                                                                  
267400     IF  STATUS-CODE NOT = ZEROS                                  
267500         MOVE SPACES             TO COD-CODE2                     
267600         PERFORM CALL-GIT-COD                                     
                                                                        
267700         IF STATUS-CODE NOT = ZEROS         
               move 1                  to return-code
267800            CALL 'AMSABRT' USING IN06.                            
267900                                                                  
268000     MOVE COD-VALX-12            TO WK-GL-CODES.                  
268100     MOVE COD-VAL-X              TO WK-CREDIT-REPS.               
268200     MOVE COD-VALX-4             TO WK-TERMS.                     
268400     MOVE COD-VAL9-1             TO NAD-ACCT-NBR.                 
268500     PERFORM CALL-GIT-NAD.                                        
268600                                                                  
268700     IF  STATUS-CODE NOT = ZEROS                                  
268800         IF  COD-CODE2   = SPACES    
                   display 'in07 ' nad-acct-nbr ' ' status-code    
               move 1                  to return-code                   
268900             CALL 'AMSABRT' USING IN07                            
269000         ELSE                                                     
269100             MOVE SPACES         TO COD-CODE2                     
269200             PERFORM CALL-GIT-COD                                 
                                                                        
269300             IF STATUS-CODE NOT = ZEROS                           
269400                CALL 'AMSABRT' USING IN06                         
269500             ELSE                                                 
269600                MOVE COD-VALX-12 TO WK-GL-CODES                   
269700                MOVE COD-VAL-X   TO WK-CREDIT-REPS                
269800                MOVE COD-VALX-4  TO WK-TERMS                      
269900                MOVE COD-VAL9-1  TO NAD-ACCT-NBR                  
270000                PERFORM CALL-GIT-NAD                              
                   display 'in07a ' nad-acct-nbr ' ' status-code        
                                                                        
270100                IF STATUS-CODE NOT = ZEROS                        
               move 1                  to return-code
270200                   CALL 'AMSABRT' USING IN07.                     
270300                                                                  
270500 GET-INVOICE-TERMS.                                               
270600*-----------------*                                               
270700                                                                  
270800     MOVE 'NITR'                 TO COD-KEY.                      
270900     MOVE SSL-AGY-TYPE           TO WK-ACTT-CODE.                 
271000     MOVE WK-ACTT-CODE           TO COD-CODE1.                    
271100     MOVE WK-TEMP-X              TO COD-CODE2.                    
271200     PERFORM CALL-GIT-COD.                                        
271300                                                                  
271400     IF  STATUS-CODE NOT = ZEROS                                  
271500         MOVE SPACES             TO COD-CODE1                     
271600         PERFORM CALL-GIT-COD                                     
                                                                        
271700         IF  STATUS-CODE NOT = ZEROS                              
               move 1                  to return-code
271800             CALL 'AMSABRT' USING IN08.                           
271900                                                                  
272000     IF  STATUS-CODE = ZEROS                                      
272100         MOVE 'D'                TO MSG-KEY                       
272200         MOVE COD-VAL-X          TO MSG-NBR                       
272300         MOVE ZEROS              TO MSG-SEQ-NBR                   
272400         PERFORM CALL-GIT-MSG                                     
                                                                        
272500         IF  STATUS-CODE NOT = ZEROS                              
               move 1                  to return-code
272600             CALL 'AMSABRT' USING IN09.                           
272700                                                                  
272900 GET-CNT.                                                         
273000*-------*                                                         
273100                                                                  
273200     MOVE 'R'                    TO CNT-FILE-CODE.                
273300     MOVE SAVE-CNT-ACCT-NBR      TO CNT-ACCT-KEY.                 
273400     MOVE SAVE-CNT-PUB           TO CNT-PUB.                      
273500     MOVE SAVE-CNT-REF-NBR       TO CNT-REF-NBR.                  
273600     display 'cnt git '    CNT-ACCT-KEY 
				 CNT-PUB        
				 CNT-REF-NBR   


273700     PERFORM CALL-GIT-CNT.                                        
                                                                        
273800     IF  STATUS-CODE NOT = ZEROS                                  
273900         MOVE 'M'                TO CNT-FILE-CODE                 
274000         MOVE SAVE-CNT-ACCT-NBR  TO CNT-MCT-ACCT-KEY              
274100         MOVE SAVE-CNT-PUB       TO CNT-MCT-PUB                   
274200         PERFORM CALL-GIT-CNT                                     
                                                                        
               IF  STATUS-CODE NOT = ZEROS 
273600             display 'missint cnt '    CNT-ACCT-KEY 
				 CNT-PUB        
				 CNT-REF-NBR   


      **       move 1                  to return-code
274400**           CALL 'AMSABRT' USING IN17. 
                    end-if
274500                                                                  
274700 GET-CNT-2.                                                       
274800*---------*                                                       
274900                                                                  
275000     MOVE 'R'                    TO CNT-FILE-CODE.                
275100     MOVE SSL-ACCT-NBR           TO CNT-ACCT-KEY.                 
275200     MOVE SSL-PUB                TO CNT-PUB.                      
275300     MOVE SSL-REF-NBR            TO CNT-REF-NBR.                  
275400                                                                  
275500     PERFORM CALL-GIT-CNT.                                        
                                                                        
275600     IF  STATUS-CODE NOT = ZEROS                                  
275700         MOVE 'M'                TO CNT-FILE-CODE                 
275800         MOVE SSL-ACCT-NBR       TO CNT-MCT-ACCT-KEY              
275900         MOVE SSL-PUB            TO CNT-MCT-PUB                   
276000         PERFORM CALL-GIT-CNT                                     
                                                                        
276100         IF  STATUS-CODE NOT = ZEROS                              
               move 1                  to return-code
276200             CALL 'AMSABRT' USING IN32.                           
276300                                                                  
276400 PROCESS-CNT-PFM-ACCUM.                                           
276500*---------------------*                                           
276600                                                                  
276700     IF  WO-STATUS    = 'F' OR 'B' OR 'R'                         
276800         IF  DAY-NAME = 'SUN'                                     
276900             ADD 1               TO WK-PFM-TOT-SUN (1)            
277000             MOVE 1              TO WK-INDX                       
277100             PERFORM PROCESS-CNT-PFM-6-SUN                        
277700         ELSE                                                     
277800             ADD 1               TO WK-PFM-TOT-WEK (1)            
277900             MOVE 1              TO WK-INDX                       
278000             PERFORM PROCESS-CNT-PFM-6-WEK.                       
278600                                                                  
278800 PROCESS-CNT-PERFORMANCE.                                         
278900*-----------------------*                                         
279000                                                                  
279100     PERFORM GET-CNT.                                             
279200                                                                  
279300     MOVE LOW-VALUES             TO WO-KEY.                       
279400                                                                  
279500     MOVE SAVE-CNT-ACCT-NBR      TO WO-ACCT-KEY.                  
279600     MOVE SAVE-CNT-PUB           TO WO-PUB.                       
279700     MOVE SAVE-CNT-REF-NBR       TO WO-REF-NBR.                   
279800                                                                  
279900     PERFORM PROCESS-CNT-PFM.                                     
280000                                                                  
280200 PROCESS-CNT-PFM.                                                 
280300*---------------*                                                 
280400                                                                  
280500     MOVE LOW-VALUES             TO WK-PFM-AREA (1)               
280600                                    WK-PFM-AREA (2).              
280700                                                                  
280800     PERFORM CALL-SETL-WO.                                        
280900     PERFORM CALL-SGET-WO.                                        
AL-05                                                                   
AL-05      IF STATUS-CODE = 0                                           
AL-05      IF WO-PUB = 'CLAS'                                           
AL-05      IF WO-INVC-CODE = 'A' OR 'D' OR 'M' OR 'S' OR 'V'            
AL-05      IF WO-CC-NUMBER NOT > 0                                      
AL-05         PERFORM FORMAT-CC-INFO.                                   
AL-05                                                                   
281000                                                                  
281100     PERFORM PROCESS-CNT-PFM-2                                    
281200       UNTIL STATUS-CODE NOT = ZEROS                              
281300          OR WO-ISSUE        > WK-PFM-CUT-OFF.                    
281400                                                                  
281500     PERFORM CALL-ESETL-WO.                                       
281600     PERFORM CHECK-STATUS.                                        
281700                                                                  
281800     PERFORM PROCESS-CNT-PFM-PRINT.                               
282000                                                                  
282100 PROCESS-CNT-PFM-2.                                               
282200*-----------------*                                               
282300                                                                  
282400     PERFORM GET-FSI.                                             
282500                                                                  
282600     IF  COD2-FLAG1 NOT = 'I'                                     
282700     AND (WO-STATUS     = 'F' OR 'B' OR 'R')                      
282800         PERFORM PROCESS-CNT-PFM-3                                
282900     ELSE                                                         
AL-05 *        PERFORM CALL-SGET-WO.                                    
AL-05          PERFORM CALL-SGET-WO                                     
AL-05          IF STATUS-CODE = 0                                       
AL-05          IF WO-PUB = 'CLS'                                        
AL-05          IF WO-INVC-CODE = 'A' OR 'D' OR 'M' OR 'S' OR 'V'        
AL-05             IF WO-CC-NUMBER NOT > 0                               
AL-05                PERFORM FORMAT-CC-INFO.                            
AL-05                                                                   
283100                                                                  
283300 PROCESS-CNT-PFM-3.                                               
283400*-----------------*                                               
283500                                                                  
283600     MOVE WO-ISSUE               TO DAY-DATE.                     
283700     PERFORM DAY-SUB.                                             
283800     MOVE WO-ISSUE               TO DAY-DATE.                     
283900                                                                  
284000     IF  DAY-NAME = 'SUN'                                         
284100         ADD 1                   TO WK-PFM-ISS-SUN (2)            
                                                                        
284200         IF  WO-ISSUE NOT < WK-MO-FROM                            
284300         AND WO-ISSUE NOT > WK-MO-TO                              
284400             ADD 1               TO WK-PFM-ISS-SUN (1)            
284500         ELSE                                                     
284600             NEXT SENTENCE                                        
284700     ELSE                                                         
284800         ADD 1                   TO WK-PFM-ISS-WEK (2)            
                                                                        
284900         IF  WO-ISSUE NOT < WK-MO-FROM                            
285000         AND WO-ISSUE NOT > WK-MO-TO                              
285100             ADD 1               TO WK-PFM-ISS-WEK (1).           
285200                                                                  
285300     PERFORM PROCESS-CNT-PFM-4                                    
285400       UNTIL STATUS-CODE NOT = ZEROS                              
285500          OR WO-ISSUE    NOT = DAY-DATE.                          
285600                                                                  
285800 PROCESS-CNT-PFM-4.                                               
285900*-----------------*                                               
286000                                                                  
286100     PERFORM GET-FSI.                                             
286200                                                                  
286300     IF  COD2-FLAG1 NOT = 'I'                                     
286400     AND (WO-STATUS     = 'F' OR 'B' OR 'R')                      
286500         PERFORM PROCESS-CNT-PFM-5.                               
286600                                                                  
286700     PERFORM CALL-SGET-WO.                                        
286800                                                                  
287000 PROCESS-CNT-PFM-5.                                               
287100*-----------------*                                               
287200                                                                  
287300     PERFORM GET-LINEAGE.                                         
287400     MOVE WO-AD-TYPE             TO WK-PFM-IND                    
287500                                    WK-WO-AD-TYPE.                
287700     IF  DAY-NAME = 'SUN'                                         
287800         ADD 1                   TO WK-PFM-TOT-SUN (2)            
287900         MOVE 2                  TO WK-INDX                       
288000         PERFORM PROCESS-CNT-PFM-6-SUN                            
                                                                        
288100         IF  WO-ISSUE NOT < WK-MO-FROM                            
288200         AND WO-ISSUE NOT > WK-MO-TO                              
288300             ADD 1               TO WK-PFM-TOT-SUN (1)            
288400             MOVE 1              TO WK-INDX                       
288500             PERFORM PROCESS-CNT-PFM-6-SUN                        
288600         ELSE                                                     
288700             NEXT SENTENCE                                        
288800     ELSE                                                         
288900         ADD 1                   TO WK-PFM-TOT-WEK (2)            
289000         MOVE 2                  TO WK-INDX                       
289100         PERFORM PROCESS-CNT-PFM-6-WEK                            
                                                                        
289200         IF  WO-ISSUE NOT < WK-MO-FROM                            
289300         AND WO-ISSUE NOT > WK-MO-TO                              
289400             ADD 1               TO WK-PFM-TOT-WEK (1)            
289500             MOVE 1              TO WK-INDX                       
289600             PERFORM PROCESS-CNT-PFM-6-WEK.                       
289700                                                                  
289900 PROCESS-CNT-PFM-6-SUN.                                           
290000*---------------------*                                           
290100                                                                  
290200     IF  WO-PUB = WK-NYT-PUB                                      
290300     AND                                                          
290400        (WK-WO-AD-TYPE = 'LMD' OR 'LLN' OR 'LNF')                 
290500         ADD WO-RATE-QUANTITY    TO WK-PFM-LIN-SUN (WK-INDX)      
290600     ELSE                                                         
CTS-02     IF (WO-PUB     =  WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB)  
290800     OR (WK-PFM-IND = 'L')                                        
290900         ADD WK-TEMP-LINEAGE-I   TO WK-PFM-LIN-SUN (WK-INDX)      
291000     ELSE                                                         
291100         ADD WK-TEMP-LINEAGE-D   TO WK-PFM-INC-SUN (WK-INDX).     
291200                                                                  
291400 PROCESS-CNT-PFM-6-WEK.                                           
291500*---------------------*                                           
291600                                                                  
291700     IF  WO-PUB = WK-NYT-PUB                                      
291800     AND                                                          
291900        (WK-WO-AD-TYPE = 'LMD' OR 'LLN' OR 'LNF')                 
292000         ADD WO-RATE-QUANTITY    TO WK-PFM-LIN-WEK (WK-INDX)      
292100     ELSE                                                         
CTS-02     IF (WO-PUB     =  WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB)  
292300     OR (WK-PFM-IND = 'L')                                        
292400         ADD WK-TEMP-LINEAGE-I   TO WK-PFM-LIN-WEK (WK-INDX)      
292500     ELSE                                                         
292600         ADD WK-TEMP-LINEAGE-D   TO WK-PFM-INC-WEK (WK-INDX).     
292700                                                                  
292900 PROCESS-CNT-PFM-PRINT.                                           
293000*---------------------*                                           
293100      display 'PROCESS-CNT-PFM-PRINT'                                                            
293200     MOVE 'C'                    TO WK-TOTAL-TYPE.                
293300     PERFORM GET-CNT.                                             
293400                                                                  
293500     ADD 1                       TO WK-CNT-COUNT.                 
293600                                                                  
293700     COMPUTE WK-TEMP-CTR = WK-LINE-CTR + 13.                      
293800                                                                  
293900     IF  WK-TEMP-CTR > WK-PAGE-DETAILS                            
294000         PERFORM PROCESS-HEADER-2                                 
294100     ELSE                                                         
294200         MOVE SPACE              TO WK-CB                         
294300         MOVE '2'                TO WK-FONT                       
294400         PERFORM WRITE-BLANK-LINE 3 TIMES.                        
294500                                                                  
294600     MOVE 'Total Gross Amount:'  TO DTL-DESCRIPTION.              
294700                                                                  
294800     IF  WK-READY-TOTAL = 'Y'                                     
294900     OR  WK-MORE-CNT    = 'Y'                                     
295000         ADD TTL-GROSS-AMT-ADJ, TTL-GROSS-AMT                     
295100                                 GIVING MT-ADV-GROSS-AMT          
295200                                        DTL-GROSS-AMT-P           
295300         ADD TTL-NET-AMT-ADJ, TTL-NET-AMT                         
295400                                 GIVING MT-ADV-NET-AMT            
295500                                        FOOT-TOT-CURR-AMT         
295600                                        FOOT-TOT-AMT              
**********     PERFORM WRITE-DETAIL-LINE-2
           end-if.                             
295800                                                                  
295900     IF  WK-READY-TOTAL = 'Y'                                     
296000     OR  WK-MORE-CNT    = 'Y'                                     
296100         IF (TTL-NET-AMT-ADJ + TTL-NET-AMT) < ZEROS               
296200             MOVE ' Credit Balance'                               
296300                                 TO FOOT-TOT-CURR-AMT-X           
296400                                    FOOT-TOT-AMT-X                
296500         ELSE                                                     
296600         IF (TTL-NET-AMT-ADJ + TTL-NET-AMT) = ZEROS               
296700             MOVE 'Zero Balance' TO FOOT-TOT-CURR-AMT-X           
296800                                    FOOT-TOT-AMT-X.               
296900                                                                  
297100     IF  WK-GL-SUBLED NOT = '16'                                  
297200         PERFORM WRITE-CONTRACT-INFO.                             
AL-06      IF DETAIL-DATA = ' '                                         
AL-06 ***     MOVE 'Ignore......' TO      FOOT-TOT-AMT-X                
AL-06         MOVE 'Ignore.....?' TO     foot-ignore.                   
297300                                                                  
297400     PERFORM WRITE-FOOTER-5.                                      
297500     PERFORM WRITE-FOOTER-6.                                      
297600                                                                  
297700     IF  WK-PAGE-CTR = 1                                          
297800         MOVE SPACE              TO WK-CB                         
297900         MOVE '1'                TO WK-FONT                       
298000         PERFORM WRITE-BLANK-LINE                                 
                                                                        
298100         MOVE WK-SEP-TEXT        TO DTL-LINE                      
298200         MOVE '1'                TO DTL-1-FONT                    
298300         MOVE DTL-LINE           TO INVOICE-REC                   
                                                                        
298400         PERFORM WRITE-LINE                                       
298500         MOVE SPACES             TO DTL-LINE.                     
298600                                                                  
298700     PERFORM WRITE-PAGE-TABLE.                                    
298800                                                                  
298900 WRITE-CONTRACT-INFO.                                             
299000*-------------------*                                             
299800                                                                  
299900     MOVE SPACE                  TO WK-CB.                        
300000     MOVE '2'                    TO WK-FONT.                      
                                                                        
300100     PERFORM WRITE-BLANK-LINE.                                    
300200     MOVE 'Contract Information:'                                 
300300                                 TO DTL-DESCRIPTION.              
300400     PERFORM WRITE-DETAIL-LINE-2.                                 
300500                                                                  
300600     MOVE CNT-START              TO UTL-EDIT-DATE.                
300700     PERFORM EDIT-OUT-DATE.                                       
                                                                        
300800     MOVE UTL-EDIT-DATE-SL       TO DTL-CNT-START.                
300900     MOVE ' - '                  TO DTL-CNT-LIT2.                 
301000     MOVE CNT-END                TO UTL-EDIT-DATE.                
                                                                        
301100     PERFORM EDIT-OUT-DATE.                                       
301200     MOVE UTL-EDIT-DATE-SL       TO DTL-CNT-END.                  
301300     MOVE 'Ref#'                 TO DTL-CNT-LIT11.                
301400     MOVE CNT-REF-NBR            TO DTL-CNT-NBR.                  
301500                                                                  
301600     PERFORM WRITE-DETAIL-LINE-2.                                 
301700                                                                  
301800     PERFORM PROCESS-CNT-PFM-LEVEL-LIT.                           
301900                                                                  
302000     PERFORM WRITE-DETAIL-LINE-2.                                 
302100                                                                  
302200     MOVE 'Pub:'                 TO DTL-CNT-LIT3.                 
302300     MOVE 'Edtn:'                TO DTL-CNT-LIT4.                 
302400     MOVE 'Adtp:'                TO DTL-CNT-LIT5.                 
302500                                                                  
302600     MOVE CNT-SOURCE             TO WK-SOURCE.                    
302700                                                                  
302800     IF  CNT-FILE-CODE = 'R'                                      
302900         MOVE CNT-PUB            TO DTL-CNT-PUB                   
303000     ELSE                                                         
303100         MOVE CNT-MCT-PUB        TO DTL-CNT-PUB.                  
303200                                                                  
303300     MOVE DTL-CNT-PUB            TO WO-PUB.                       
303400     MOVE CNT-EDITION            TO DTL-CNT-EDTN.                 
303500     MOVE CNT-AD-TYPE            TO DTL-CNT-ADTP.                 
303600                                                                  
303700     PERFORM WRITE-DETAIL-LINE-2.                                 
303800                                                                  
303900     MOVE 'Current Month Activity:'                               
304000                                 TO DTL-DESCRIPTION.              
304100     PERFORM WRITE-DETAIL-LINE-2.                                 
304200                                                                  
304300     MOVE 'Daily  Inserts:'      TO DTL-PFM-LIT1.                 
304400                                                                  
304500     IF  WK-PFM-AREA (1) NOT = LOW-VALUES                         
304600         MOVE WK-PFM-TOT-WEK (1) TO DTL-PFM-AMT-1A                
                                                                        
304700         IF  WK-PFM-LIN-WEK (1) = ZEROS                           
304800             MOVE WK-PFM-INC-WEK (1) TO DTL-PFM-AMT-2A-I          
304900         ELSE                                                     
305000             MOVE WK-PFM-LIN-WEK (1) TO DTL-PFM-AMT-2A-L.         
305100                                                                  
305600         MOVE 'Inches:'          TO DTL-PFM-LIT2.                 
305700                                                                  
305800     PERFORM WRITE-DETAIL-LINE-2.                                 
305900                                                                  
306000     MOVE 'Sunday Inserts:'      TO DTL-PFM-LIT1.                 
                                                                        
306500         MOVE 'Inches:'          TO DTL-PFM-LIT2.                 
306600                                                                  
306700     IF  WK-PFM-AREA (1) NOT = LOW-VALUES                         
306800         MOVE WK-PFM-TOT-SUN (1) TO DTL-PFM-AMT-1A                
                                                                        
306900         IF  WK-PFM-LIN-SUN (1) = ZEROS                           
307000             MOVE WK-PFM-INC-SUN (1) TO DTL-PFM-AMT-2A-I          
307100         ELSE                                                     
307200             MOVE WK-PFM-LIN-SUN (1) TO DTL-PFM-AMT-2A-L.         
307300                                                                  
307400     PERFORM WRITE-DETAIL-LINE-2.                                 
307500                                                                  
307600     IF  WK-READY-TOTAL = 'Y'                                     
307700     OR  WK-MORE-CNT    = 'Y'                                     
307800         IF (TTL-NET-AMT-ADJ + TTL-NET-AMT) < ZEROS               
307900             MOVE ' Credit Balance'                               
308000                                 TO DTL-NET-AMT-X                 
308100         ELSE                                                     
308200         IF (TTL-NET-AMT-ADJ + TTL-NET-AMT) = ZEROS               
308300             MOVE 'Zero Balance' TO DTL-NET-AMT-X.                
308400                                                                  
308500     MOVE SPACE                  TO WK-MAG-TAPE-REC.              
308600     MOVE '3'                    TO MT-CODE-3.                    
308700                                                                  
308800     IF  WK-AGY-JXRF-2 NUMERIC                                    
308900         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-3                  
309000     ELSE                                                         
309100         MOVE SPACE              TO MT-AGY-NBR-3.                 
309200                                                                  
309300     IF  WK-ADV-JXRF-2 NUMERIC                                    
309400         MOVE WK-ADV-JXRF-2      TO MT-ADV-NBR-3                  
309500     ELSE                                                         
309600         MOVE SPACE              TO MT-ADV-NBR-3.                 
309700                                                                  
309800     MOVE PARM-INVC-DATE-P       TO MT-BILL-DATE-3.               
309900                                                                  
310000     IF  WK-READY-TOTAL = 'Y'                                     
310100     OR  WK-MORE-CNT    = 'Y'                                     
310200         ADD TTL-GROSS-AMT-ADJ, TTL-GROSS-AMT                     
310300                                 GIVING MT-ADV-GROSS-AMT          
310400         ADD TTL-NET-AMT-ADJ, TTL-NET-AMT                         
310500                                 GIVING MT-ADV-NET-AMT.           
310600                                                                  
310700     IF  WK-PRODUCE-TAPE = 'Y'                                    
310800         WRITE MAG-TAPE-REC FROM WK-MAG-TAPE-REC.                 
310900                                                                  
311000     IF  WK-READY-TOTAL = 'Y'                                     
311100     OR  WK-MORE-CNT    = 'Y'                                     
311200         IF (TTL-NET-AMT-ADJ + TTL-NET-AMT) < ZEROS               
311300         OR (TTL-NET-AMT-ADJ + TTL-NET-AMT) = ZEROS               
311400             MOVE ' Do not Remit'                                 
311500                                 TO FOOT-TOT-CURR-AMT-X           
311600                                    FOOT-TOT-AMT-X.               
311700 PROCESS-CNT-PFM-LEVEL-LIT.                                       
311800*-------------------------*                                       
311900                                                                  
312000     IF  CNT-FREQ NOT > 1                                         
312100         MOVE 'Open Level'       TO DTL-CNT-OPEN                  
312200     ELSE                                                         
312300         PERFORM PROCESS-CNT-PFM-LEVEL.                           
312400                                                                  
312500 MAG-TAPE-0-REC.                                                  
312600*--------------*                                                  
312700                                                                  
312800     PERFORM GET-CNT-2.                                           
312900                                                                  
313000     MOVE SPACE                  TO WK-MAG-TAPE-REC.              
313100     MOVE '0'                    TO MT-CODE-0.                    
313200                                                                  
313300     IF  WK-AGY-JXRF-2 NUMERIC                                    
313400         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-0                  
313500     ELSE                                                         
313600         MOVE SPACE              TO MT-AGY-NBR-0.                 
313700                                                                  
313800     IF  SSL-CATEGORY     =  LOW-VALUES                           
313900     AND WK-GL-SUBLED NOT = '16'                                  
314000         MOVE 'Miscellaneous Advertiser(s) '                      
314100                                 TO MT-ADV-NAME-0                 
314200         MOVE SPACE              TO MT-ADV-NBR-0                  
314300     ELSE                                                         
314400         MOVE NAD-NAME-1         TO MT-ADV-NAME-0                 
314500         MOVE CNT-REF-NBR        TO MT-CNT-REF-NBR                
314600         MOVE CNT-END            TO UTL-EDIT-DATE                 
314700         PERFORM EDIT-OUT-DATE                                    
                                                                        
314800         MOVE UTL-EDIT-DATE-6    TO MT-CNT-END                    
314900         PERFORM PROCESS-CNT-PFM-LEVEL-LIT                        
                                                                        
315000         MOVE DTL-CNT-OPEN       TO MT-CNT-LEVEL                  
315100         MOVE SPACE              TO DTL-CNT-OPEN                  
                                                                        
315200         IF  WK-ADV-JXRF-2 NUMERIC                                
315300             MOVE WK-ADV-JXRF-2 TO MT-ADV-NBR-0                   
315400         ELSE                                                     
315500             MOVE SPACE          TO MT-ADV-NBR-0.                 
315600                                                                  
315700     MOVE PARM-INVC-DATE-P       TO MT-BILL-DATE-1.               
315800     MOVE CNT-PUB                TO MT-CNT-PUB.                   
315801     MOVE CNT-EDITION            TO MT-CNT-EDITION.               
315802     MOVE CNT-AD-TYPE            TO MT-CNT-AD-TYPE.               
315810                                                                  
315900     IF  WK-PRODUCE-TAPE = 'Y'                                    
316000         WRITE MAG-TAPE-REC FROM WK-MAG-TAPE-REC.                 
316200                                                                  
316300 PROCESS-CNT-PFM-LEVEL.                                           
316400*---------------------*                                           
316500                                                                  
316600     MOVE CNT-AD-TYPE            TO WK-PFM-IND.                   
316700                                                                  
CTS-02     IF (WO-PUB     =  WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB)  
316900     OR (WK-PFM-IND = 'L')                                        
317000         MOVE 'Line(s)'          TO DTL-CNT-LIT9                  
317100     ELSE                                                         
317200         MOVE 'Column in.'       TO DTL-CNT-LIT7.                 
317300                                                                  
317400     MOVE CNT-FREQ               TO DTL-CNT-LEVEL                 
317500                                    WK-CNT-LEVEL.                 
317600                                                                  
317700     INSPECT WK-CNT-LVL REPLACING LEADING ZEROS BY SPACES.        
317800                                                                  
317900     IF  CNT-FREQ > 9999                                          
318000         MOVE 'NLV2'             TO COD-KEY                       
318100         MOVE WK-CNT-LVL-1       TO COD-CODE1                     
318200         MOVE WK-CNT-LVL-2       TO COD-CODE2                     
318300         PERFORM CALL-GIT-COD                                     
                                                                        
318400         IF  STATUS-CODE = ZEROS                                  
318500             MOVE COD-VAL-X      TO WK-CNT-LVL-2.                 
318600                                                                  
318700     MOVE 'NLVL'                 TO COD-KEY.                      
318800     MOVE WO-PUB                 TO COD-CODE1.                    
318900     MOVE CNT-EDITION            TO COD-CODE2.                    
319000     MOVE CNT-AD-TYPE            TO COD-CODE3.                    
319100     MOVE WK-CNT-LVL-2           TO COD-CODE4.                    
319200     PERFORM CALL-GIT-COD.                                        
319300                                                                  
319400     IF  STATUS-CODE NOT = ZEROS                                  
319500         NEXT SENTENCE                                            
319600     ELSE                                                         
319700     IF  COD-FLAG1 = 'Y'                                          
319800         MOVE COD-NAME           TO DTL-CNT-OPTION                
319900     ELSE                                                         
320500         MOVE 'or'               TO DTL-CNT-LIT8A                 
320600         MOVE COD-NAME           TO DTL-CNT-LIT8.                 
320700                                                                  
320900 PROCESS-MISC-TOTAL.                                              
321000*------------------*          
               display 'PROCESS-MISC-TOTAL'
321100     MOVE SPACES                 TO WS-MISC-GROUP-BUY.            
321200     MOVE 'M'                    TO WK-TOTAL-TYPE.                
321300     MOVE SPACE                  TO WK-MAG-TAPE-REC.              
321400     MOVE '3'                    TO MT-CODE-3.                    
321500                                                                  
321600     IF  WK-AGY-JXRF-2 NUMERIC                                    
321700         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-3                  
321800     ELSE                                                         
321900         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-3.                 
322000                                                                  
322100     MOVE PARM-INVC-DATE-P       TO MT-BILL-DATE-3.               
322200                                                                  
322300     ADD TTL-GROSS-AMT-ADJ, TTL-GROSS-AMT                         
322400                                 GIVING MT-ADV-GROSS-AMT          
322500                                        DTL-GROSS-AMT-P.          
322600     ADD TTL-NET-AMT-ADJ, TTL-NET-AMT                             
322700                                 GIVING MT-ADV-NET-AMT            
322800                                        HDR-AMOUNT                
322900                                        FOOT-TOT-CURR-AMT         
323000                                        FOOT-TOT-AMT.             
323100                                                                  
323200     IF  WK-PRODUCE-TAPE = 'Y'                                    
323300         WRITE MAG-TAPE-REC FROM WK-MAG-TAPE-REC.                 
323400                                                                  
323500     COMPUTE WK-TEMP-CTR = WK-LINE-CTR + 4.                       
323600     DISPLAY 'MISC: ' GCT-MAX-ENTRY ' '                           41030001
                    WK-TEMP-CTR ' '                                     41030101
                    WK-PAGE-DETAILS.                                    41030201
AL-06                                                                   41030301
AL-06      IF  GCT-MAX-ENTRY = 0                                        41030401
323700     IF  WK-TEMP-CTR > WK-PAGE-DETAILS                            41030500
323800         PERFORM PROCESS-HEADER-2                                 
323900     ELSE                                                         
323900     ELSE                                                         41030800
324000         MOVE SPACE              TO WK-CB                         41030900
324100         MOVE '2'                TO WK-FONT                       41031000
324200         PERFORM WRITE-BLANK-LINE.                                41031100
AL-06                                                                   41031201
AL-06      IF GCT-MAX-ENTRY > 1                                         41031301
AL-06             MOVE SSL-REC         TO HOLD-SSL-REC                  41032001
AL-06             MOVE 'G'             TO WK-GROUP-BUY                  41032101
AL-06             SET GCT-X            TO 1                             41033001
AL-06             MOVE 'Y'             TO PROCESS-GROUP-CODE            41034001
al-06             PERFORM PROCESS-HEADER-2                              41035001
AL-06             MOVE SPACES          TO DTL-HEAD-MSG                  41036001
AL-06             MOVE 'THIS IS YOUR GLOBAL BUY PAGE' TO DTL-HEAD-MSG   41037001
AL-06             display 'global-2'                                    41038001
AL-06             MOVE 'G'             TO detail-data                   41039001
AL-06             PERFORM WRITE-DETAIL-LINE                             41039101
AL-06             PERFORM SORT-GCT-TABLE                                41039201
AL-06             SET GCT-X            TO 1                             41039301
AL-06             MOVE GCT-WO-GROUP-CODE(GCT-X) TO WK-SAVE-GROUP-CODE   41039401
AL-06             PERFORM PROCESS-GB-INFO THRU                          41039501
AL-06                     PROCESS-GB-INFO-EXIT UNTIL                    41039601
AL-06                     GCT-X > GCT-MAX-ENTRY                         41039701
AL-06             MOVE ' '             TO WK-GROUP-BUY                  41039801
AL-06             MOVE 'Y' TO WS-MISC-GROUP-BUY.                        41039901
324300                                                                  
324400     MOVE 'Total Gross Amount:'  TO DTL-DESCRIPTION.              
324500     PERFORM WRITE-DETAIL-LINE-2.                                 
324600                                                                  
324700     IF  TTL-NET-AMT-ADJ + TTL-NET-AMT < ZEROS                    
324800         MOVE ' Credit Balance'  TO FOOT-TOT-CURR-AMT-X           
324900                                    FOOT-TOT-AMT-X                
325000     ELSE                                                         
325100     IF  TTL-NET-AMT-ADJ + TTL-NET-AMT = ZEROS                    
325200         MOVE 'Zero Balance'     TO FOOT-TOT-CURR-AMT-X           
325300                                    FOOT-TOT-AMT-X.               
325400                                                                  
325500     IF  (TTL-NET-AMT-ADJ + TTL-NET-AMT) < ZEROS                  
325600     OR  (TTL-NET-AMT-ADJ + TTL-NET-AMT) = ZEROS                  
325700         MOVE 'Do not Remit'    TO FOOT-TOT-CURR-AMT-X            
325800                                   FOOT-TOT-AMT-X.                
325900                                                                  
326000     IF  WK-GL-SUBLED NOT = '16'                                  
326100         MOVE 'Miscellaneous Advertiser(s) '                      
326200                                 TO FOOT-ADV-NAME                 
326300     ELSE                                                         
326400         MOVE NAD-NAME-1         TO FOOT-ADV-NAME.                
326500                                                                  
326600     PERFORM WRITE-FOOTER-5.                                      
326700     PERFORM WRITE-FOOTER-6.                                      
326800                                                                  
326900     IF  WK-PAGE-CTR = 1                                          
327000         MOVE SPACE              TO WK-CB                         
327100         MOVE '1'                TO WK-FONT                       
327200         PERFORM WRITE-BLANK-LINE                                 
                                                                        
327300         MOVE WK-SEP-TEXT        TO DTL-LINE                      
327400         MOVE '1'                TO DTL-1-FONT                    
327500         MOVE DTL-LINE           TO INVOICE-REC                   
                                                                        
327600         PERFORM WRITE-LINE                                       
327700         MOVE SPACES             TO DTL-LINE.                     
AL-06                                                                   41470501
AL-06      IF WS-MISC-GROUP-BUY NOT = 'Y'                               41471001
327900     PERFORM WRITE-PAGE-TABLE.                                    41480000
328000                                                                  
328100 PROCESS-AGY-TOTAL.                                               
328200*-----------------*         
           display 'PROCESS-AGY-TOTAL '
           display TTL-AGY-GROSS-AMT-ADJ ' ' 
                   TTL-AGY-GROSS-AMT ' '
                   TTL-AGY-NET-AMT-ADJ ' ' 
                   TTL-AGY-NET-AMT   ' '
                   TTL-TOT-AGY-CC-AMT.            
              display 'TTL-AGY-ADV-GROSS-AMT-ADJ '
                       TTL-AGY-ADV-GROSS-AMT-ADJ
              display 'TTL-AGY-ADV-GROSS-AMT '
                       TTL-AGY-ADV-GROSS-AMT    
              display 'TTL-AGY-ADV-GROSS-AMT-T '
                       TTL-AGY-ADV-GROSS-AMT-T 
              display 'TTL-AGY-ADV-NET-AMT-ADJ '   
                       TTL-AGY-ADV-NET-AMT-ADJ ' ' 
              display 'TTL-AGY-ADV-NET-AMT '  
                       TTL-AGY-ADV-NET-AMT  
              display 'TTL-AGY-ADV-GROSS-AMT-ADJ-T '
                       TTL-AGY-ADV-GROSS-AMT-ADJ-T 
              display 'TTL-AGY-ADV-NET-AMT-ADJ-NT-T '
                       TTL-AGY-ADV-NET-AMT-ADJ-NT-T   ' '
                       'WK-AGY-ADV-GROSS-TOT '
                       WK-AGY-ADV-GROSS-TOT 
                display 
                       TTL-AGY-ADV-CC-AMT   ' '
                      TTL-AGY-ADV-COM-AMT     ' '
                     TTL-AGY-ADV-TAX-AMT  ' ' 
                      TTL-AGY-ADV-D-P-AMT   ' '  
                      TTL-AGY-ADV-SPL-CHG   ' '  
                    TTL-AGY-ADV-ADJ-AMt ' '
                    TTL-AGY-ADV-CC-AMT ' ' 
           
CTS-02     INITIALIZE WK-TOT-AMOUNTS.                                   
CTS-02     INITIALIZE NEW-TECH-REC-4.                                   
CTS-02     MOVE '4'                    TO NT-CODE-4A.                   
CTS-02     IF WK-MSG-CD3 NOT = SPACES                                   
CTS-02        MOVE WK-MSG-CD3          TO NT-MSG-CD3                    
CTS-02     ELSE                                                         
CTS-02        MOVE SPACES              TO NT-MSG-CD3                    
CTS-02     END-IF                                                       
CTS-02     MOVE TTL-TOT-AGY-TAX-AMT    TO NT-TOT-AGY-TAX                
CTS-02                                    WK-TTL-ALL-TAX-AMT.           
CTS-02     MOVE TTL-TOT-AGY-COM-AMT    TO NT-TOT-AGY-COMM               
CTS-02                                    WK-TTL-ALL-COM-AMT.     
 
CTS-02     MOVE TTL-TOT-AGY-D-P-AMT    TO NT-TOT-AGY-D-P                
CTS-02                                    WK-TTL-ALL-D-P-AMT.           
CTS-02     MOVE TTL-TOT-AGY-SPL-CHG    TO NT-TOT-AGY-SPL                
CTS-02                                    WK-TTL-ALL-SPL-CHG.           
CTS-02     MOVE TTL-TOT-AGY-ADJ-AMT    TO NT-TOT-AGY-ADJ                
CTS-02                                    WK-TTL-ALL-ADJ-AMT.           
CTS-02     MOVE TTL-TOT-AGY-CC-AMT     TO NT-TOT-AGY-CC-AMT             
CTS-02                                    WK-TTL-ALL-CC-AMT.            
330400     MOVE SPACE                  TO WK-MAG-TAPE-REC.              
328400     MOVE 'A'                    TO WK-TOTAL-TYPE.                
328500                                                                  
328600     MOVE SPACES                 TO HDR-ADV-NBR-X.                
328700     MOVE 'Grand Total'          TO HDR-ADV-NAME                  
328800                                    FOOT-ADV-NAME.                
329000     ADD TTL-AGY-GROSS-AMT-ADJ, TTL-AGY-GROSS-AMT                         
329100                                 GIVING DTL-GROSS-AMT-P                   
CTS-02                                        NT-TOT-AGY-GROSS                  
CTS-02                                        WK-TTL-ALL-GRS-AMT                
329200                                        MT-AGY-GROSS-AMT.                 
329300     ADD TTL-AGY-NET-AMT-ADJ, TTL-AGY-NET-AMT                             
329400                                 GIVING HDR-AMOUNT                        
329500                                        MT-AGY-NET-AMT                    
329600                                        FOOT-TOT-CURR-AMT                 
329700                                        FOOT-TOT-AMT.                     
                                                                                
           COMPUTE NT-TOT-AGY-NET      = TTL-AGY-NET-AMT-ADJ +                  
                                         TTL-AGY-NET-AMT     -                  
                                         TTL-TOT-AGY-CC-AMT.                    
                                                                                
           COMPUTE WK-TTL-ALL-NET-AMT  = TTL-AGY-NET-AMT-ADJ +                  
                                         TTL-AGY-NET-AMT     -                  
                                         TTL-TOT-AGY-CC-AMT.                    
329800     
********   compute NT-TOT-AGY-GROSS   = 
********         TTL-AGY-GROSS-AMT-ADJ + TTL-AGY-GROSS-AMT 
********** compute NT-TOT-AGY-NET = 
*********        TTL-AGY-NET-AMT-ADJ +  TTL-AGY-NET-AMT  -
*********        WK-PPD-AMT
       
CTS-02     PERFORM ADD-ALL-TOTALS.                                      
CTS-02*                                                                 
329900     PERFORM PROCESS-HEADER-3.                                    
330000                                                                  
330100     MOVE 'Total Gross Amount:'  TO DTL-DESCRIPTION.              
330200     PERFORM WRITE-DETAIL-LINE-2.                                 
330300                                                                  
330500     MOVE '4'                    TO MT-CODE-4.                    
330600                                                                  
330700     IF  WK-AGY-JXRF-2 NUMERIC                                    
330800         MOVE WK-AGY-JXRF-2      TO MT-AGY-NBR-4                  
330900     ELSE                                                         
331000         MOVE SPACE              TO MT-AGY-NBR-4.                 
331100                                                                  
331200     MOVE PARM-INVC-DATE-P       TO MT-BILL-DATE-4.               
331300                                                                  
331400     IF  WK-PRODUCE-TAPE = 'Y'                                    
331500         WRITE MAG-TAPE-REC FROM WK-MAG-TAPE-REC.                 
331600                                                                  
331700     IF  TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT < ZEROS            
331800         MOVE ' Credit Balance'  TO FOOT-TOT-CURR-AMT-X           
331900                                    FOOT-TOT-AMT-X                
332000     ELSE                                                         
332100     IF  TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT = ZEROS            
332200         MOVE 'Zero Balance'     TO FOOT-TOT-CURR-AMT-X           
332300                                    FOOT-TOT-AMT-X.               
332400                                                                  
AL-20 **   MOVE NA2-VAT-REV-CNTRY TO WK-REV-CNTRY.                      
AL-22      MOVE NA2-VAT-BILL-CNTRY TO WK-REV-CNTRY.                     
                                                                        
332500     IF  (TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT) < ZEROS          
332600     OR  (TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT) = ZEROS          
332700         MOVE 'Do not Remit'     TO FOOT-TOT-CURR-AMT-X           
332800                                    FOOT-TOT-AMT-X.               
CTS-02*                                                                 
CTS-02     IF  (TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT                   
CTS-02                          - TTL-TOT-AGY-CC-AMT)  < ZEROS          
CTS-02     OR  (TTL-AGY-NET-AMT-ADJ + TTL-AGY-NET-AMT                   
CTS-02                          - TTL-TOT-AGY-CC-AMT)  = ZEROS          
CTS-02         MOVE 'N'                TO NT-CODE-4B                    
CTS-02         MOVE 'E'                TO NT-MAIL-FLAG                  
CTS-02         MOVE 'M1'               TO NT-MSG-CD1                    
               IF WO-PD-FLAG = 'D'                                      
CTS-02            MOVE 'M4'            TO NT-MSG-CD2                    
CTS-02            MOVE 'A2'            TO NT-MSG-CD4                    
               ELSE                                                     
                  IF WO-PD-FLAG = 'E'                                   
CTS-02               MOVE 'M2'         TO NT-MSG-CD2                    
CTS-02               MOVE 'A1'         TO NT-MSG-CD4                    
                  END-IF                                                
               END-IF                                                   
CTS-02     ELSE                                                         
CTS-02         MOVE 'Y'                TO NT-CODE-4B                    
CTS-02         MOVE 'M'                TO NT-MAIL-FLAG                  
CTS-02         MOVE 'M1'               TO NT-MSG-CD1                    
               IF WO-PD-FLAG = 'D'                                      
CTS-02            MOVE 'M4'            TO NT-MSG-CD2                    
CTS-02            MOVE 'A2'            TO NT-MSG-CD4                    
               ELSE                                                     
                  IF WO-PD-FLAG = 'E'                                   
CTS-02               MOVE 'M2'         TO NT-MSG-CD2                    
CTS-02               MOVE 'A1'         TO NT-MSG-CD4                    
                  END-IF                                                
               END-IF                                                   
CTS-02     END-IF.                                                      
CTS-02*             
AL-01**
AL-01      MOVE NA2-ROUTE(1:1)         TO NT-MAIL-FLAG.
AL-01**                                                            
AL-20**      IF PARM-INVC-TYPE = '01'                                   
AL-20 *        IF VALID-REV-CNTRY                                       
AL-20 *            MOVE 'A'             TO NT-MAIL-FLAG.                
AL-20 *                                                                 
AL-20 *     IF PARM-INVC-TYPE = '01'                                    
AL-20 *        IF WK-BARTER-AD = 'Y'                                    
AL-20 *            MOVE 'P'             TO NT-MAIL-FLAG.                
AL-20 *                                                                 
      *                                                                 
CTS-02*     IF PARM-INVC-TYPE = '02'                                    
CTS-02*        MOVE 'E'                 TO NT-MAIL-FLAG                 
CTS-02*     END-IF                                                      
      **CTS - 07/05/07 CHANGE BEGINS                                    
CTS-02*     IF PARM-INVC-TYPE = '04'                                    
CTS-02*        MOVE 'R'                 TO NT-MAIL-FLAG                 
CTS-02*     END-IF                                                      
      *CTS - 07/05/07 CHANGE ENDS                                       
AL-24 *                                                                 
AL-24 *     IF  NA2-ROUTE = 'IMC     '                                  
AL-24 *         MOVE 'I'                TO NT-MAIL-FLAG.                
AL-24 *                                                     
CTS-02     MOVE zero                   TO NT-TOT-AGY-COMM               
                                          NT-TOT-AGY-CC-AMT             

CTS-02     MOVE WK-HOLD-AGY-NBR        TO NT-AGY-NBR-3.                 
CTS-02     WRITE NEWT-REC              FROM NEW-TECH-REC-4. 
           display 'write ' NEW-TECH-REC-4.
AL-01A     ADD 1     TO WK-NT-REC-CNT.              
           add 1                       to TTL-INVC-NBR  
           
333000     PERFORM WRITE-FOOTER-5.                                      
333100     PERFORM WRITE-FOOTER-6.                                      
333200                                                                  
333300     PERFORM WRITE-PAGE-TABLE.                                    
333400                                                                  
333500 PROCESS-INVOICE-TOTAL-AR.                                        
333600*------------------------*                                        
333700*                                                                 
337000         MOVE ZEROS              TO TTL-IAR-10-AMT.               
                                                                        
333500 PROCESS-INVOICE-TOTAL-AR-2.                                      
333600*--------------------------*                                      
333700                                                                  
333800     IF  PARM-IAR-FLAG NOT = 'N'                                  
333900     AND (WK-GL-SUBLED NOT =  WK-SAVE-MAJOR                       
334000     OR  WK-GL-LED     NOT =  WK-SAVE-MINOR)                      
334100         PERFORM PROCESS-AR-TB11.                                 
334200                                                                  
334300     MOVE WK-GL-SUBLED           TO TB10-MAJOR                    
334400                                    TB11-MAJOR                    
334500                                    WK-SAVE-MAJOR.                
334600     MOVE WK-GL-LED              TO TB10-MINOR                    
334700                                    TB11-MINOR                    
334800                                    WK-SAVE-MINOR.                
334900                                                                  
335000     MOVE WK-BATCH-NBR           TO TB10-BATCH-NBR.               
335100     MOVE WK-DOCUMENT            TO TB10-ITEM-NBR.                
335200     MOVE WK-DATE                TO TB10-ITEM-DATE-X.             
                                                                        
335300     IF  WK-GL-SUBLED = '16'                                      
335400         MOVE '001'              TO TB10-TERMS-CODE-X             
335500         MOVE HDR-AGY-NBR-X      TO TB10-ACCT-NBR                 
335600     ELSE                                                         
335700         MOVE '015'              TO TB10-TERMS-CODE-X             
335800         MOVE HDR-AGY-NBR-8      TO TB10-ACCT-NBR.                
                                                                        
335900     MOVE WK-REF-FIELD           TO TB10-REF-NBR.                 
336000     MOVE TTL-IAR-10-AMT         TO TB10-ITEM-AMT.                
336100                                                                  
336200     IF  PARM-IAR-FLAG NOT = 'N'                                  
336300     AND TB10-ITEM-AMT NOT =  ZEROS                               
336400         ADD 1                   TO TTL-IAR-DETAIL                
336500         WRITE IAR-REC FROM TB10-REC                              
                                                                        
336600         IF  WK-IAR-STATUS NOT = '00'                             
336700             CALL 'AMSABRT' USING IN21.                           
336800                                                                  
336900     ADD TTL-IAR-10-AMT          TO TTL-IAR-11-AMT.               
337000     MOVE ZEROS                  TO TTL-IAR-10-AMT.               
337100                                                                  
337200     IF   PARM-IAR-FLAG NOT = 'N'                                 
337300     AND (TTL-IAR-11-AMT    >  9999999                            
337400     OR   TTL-IAR-11-AMT    < -9999999)                           
337500          PERFORM PROCESS-AR-TB11.                                
337600                                                                  
338000 PROCESS-AR-TB11.                                                 
338100*---------------*                                                 
338200                                                                  
338300     MOVE TTL-IAR-11-AMT         TO TB11-TOTAL-BATCH-AMT.         
338400     MOVE ZEROS                  TO TTL-IAR-11-AMT.               
338500     MOVE WK-DATE                TO TB11-BATCH-DATE-X.            
338600     MOVE WK-BATCH-NBR           TO TB11-BATCH-NBR.               
338700                                                                  
338800     IF  TB11-TOTAL-BATCH-AMT NOT = ZEROS                         
338900         ADD 1                   TO TTL-IAR-TOTAL                 
339000                                    WK-BATCH-NBR-9                
339100         WRITE IAR-REC FROM TB11-REC                              
                                                                        
339200         IF  WK-IAR-STATUS NOT = '00'                             
339300             CALL 'AMSABRT' USING IN21.                           
339400                                                                  
339600 PROCESS-INVOICE-TOTAL.                                           
339700*---------------------*                                           
339800                  
              display 'PROCESS-INVOICE-TOTAL'
339900     ADD TTL-GROSS-AMT           TO TTL-TTL-GROSS-AMT                     
340000     ADD TTL-GROSS-AMT-ADJ       TO TTL-TTL-GROSS-AMT-ADJ                 
340100     ADD TTL-DISC-AMT            TO TTL-TTL-DISC-AMT                      
340200     ADD TTL-DISC-AMT-ADJ        TO TTL-TTL-DISC-AMT-ADJ                  
340300     ADD TTL-SPCH-AMT            TO TTL-TTL-SPCH-AMT                      
340400     ADD TTL-SPCH-AMT-ADJ        TO TTL-TTL-SPCH-AMT-ADJ                  
340500     ADD TTL-NET-AMT             TO TTL-TTL-NET-AMT                       
340600     ADD TTL-NET-AMT-ADJ         TO TTL-TTL-NET-AMT-ADJ                   
340700                      
             display TTL-TTL-GROSS-AMT           ' ' 
                    TTL-TTL-GROSS-AMT-ADJ       ' ' 
                    TTL-TTL-DISC-AMT            ' ' 
                       TTL-TTL-DISC-AMT-ADJ        ' ' 
                    TTL-TTL-SPCH-AMT            ' ' 
                      TTL-TTL-SPCH-AMT-ADJ         ' ' 
                     TTL-TTL-NET-AMT             ' ' 
                        TTL-TTL-NET-AMT-ADJ         

340800     MOVE ZEROS                  TO TTL-GROSS-AMT                         
340900                                    TTL-GROSS-AMT-ADJ                     
341000                                    TTL-DISC-AMT                          
341100                                    TTL-DISC-AMT-ADJ                      
341200                                    TTL-SPCH-AMT                          
341300                                    TTL-SPCH-AMT-ADJ                      
341400                                    TTL-NET-AMT                           
341500                                    TTL-NET-AMT-ADJ.                      
341600                                                                  
341800 PROCESS-TOTAL-RECAP.                                             
341900*-------------------*                                             
342000      display 'PROCESS-TOTAL-RECAP'  
                display TTL-TTL-GROSS-AMT  
                display TTL-TTL-GROSS-AMT-ADJ  
                display TTL-TTL-NET-AMT   
342100     IF  PARM-IAR-FLAG  NOT = 'N'                                         
342200     AND TTL-IAR-11-AMT NOT =  ZEROS                                      
342300         PERFORM PROCESS-AR-TB11.                                         
342400                                                                          
343200     MOVE 999                    TO WK-PAGE-DETAILS.                      
343300     MOVE SPACES                 TO PRT-REC.                              
343400     PERFORM PAGE-EJECT.                                                  
343500                                                                          
343600*>   Gross Amount                                                         
343700                                                                          
343800     MOVE 'Total Gross Amount:'      TO DTL-LINE-TOT.     
343900     MOVE TTL-TTL-GROSS-AMT          TO DTL-AMT-TOT-P.                    
344000     PERFORM PRINT-DETAIL-LINE.                                           
344100                                                                          
344200     MOVE 'Total Gross Adj. Amount:' TO DTL-LINE-TOT.                     
344300     MOVE TTL-TTL-GROSS-AMT-ADJ      TO DTL-AMT-TOT-P.                    
344400     PERFORM PRINT-DETAIL-LINE.                                           
344500                         
399200     MOVE 'Total Digital Orders Gross' TO DTL-LINE.                         
DW-08      move ttl-digital-gross-amt      to dtl-amt-tot-p                     
399400     PERFORM PRINT-DETAIL-LINE.                                           

344600     MOVE ALL '-'                    TO DTL-AMT-TOT-X.                    
344700     PERFORM PRINT-DETAIL-LINE.                                           
344800                                                                          
344900     MOVE 'Final Gross Amount:'      TO DTL-LINE-TOT.  
           add ttl-digital-gross-amt       to TTL-TTL-GROSS-AMT 
345000     ADD TTL-TTL-GROSS-AMT-ADJ       TO TTL-TTL-GROSS-AMT.                
345100     MOVE TTL-TTL-GROSS-AMT          TO DTL-AMT-TOT-P.                    
345200     PERFORM PRINT-DETAIL-LINE.                                           
345300                                                                          
345400     PERFORM PRINT-BLANK-LINE        2 TIMES.                             
345500                                                                          
345600*>   Discount Amount                                                      
345700                                                                          
345800     MOVE 'Total Disc. Amount:'      TO DTL-LINE-TOT.                     
345900     MOVE TTL-TTL-DISC-AMT           TO DTL-AMT-TOT-P.                    
346000     PERFORM PRINT-DETAIL-LINE.                                           
346100                                                                          
346200     MOVE 'Total Disc. Adj. Amount:' TO DTL-LINE-TOT.                     
346300     MOVE TTL-TTL-DISC-AMT-ADJ       TO DTL-AMT-TOT-P.                    
346400     PERFORM PRINT-DETAIL-LINE.                                           
346500                                                                          
346600     MOVE ALL '-'                    TO DTL-AMT-TOT-X.                    
346700     PERFORM PRINT-DETAIL-LINE.                                           
346800                                                                          
346900     MOVE 'Final Disc. Amount:'      TO DTL-LINE-TOT.                     
347000     ADD TTL-TTL-DISC-AMT-ADJ        TO TTL-TTL-DISC-AMT.                 
347100     MOVE TTL-TTL-DISC-AMT           TO DTL-AMT-TOT-P.                    
347200     PERFORM PRINT-DETAIL-LINE.                                           
347300                                                                          
347400     PERFORM PRINT-BLANK-LINE        2 TIMES.                             
347500                                                                          
347600*>   Production Charge Amount                                             
347700                                                                          
347800     MOVE 'Total Spch. Amount:'      TO DTL-LINE-TOT.                     
347900     MOVE TTL-TTL-SPCH-AMT           TO DTL-AMT-TOT-P.                    
348000     PERFORM PRINT-DETAIL-LINE.                                           
348100                                                                          
348200     MOVE 'Total Spch. Adj. Amount:' TO DTL-LINE-TOT.                     
348300     MOVE TTL-TTL-SPCH-AMT-ADJ       TO DTL-AMT-TOT-P.                    
348400     PERFORM PRINT-DETAIL-LINE.                                           
348500                                                                          
348600     MOVE ALL '-'                    TO DTL-AMT-TOT-X.                    
348700     PERFORM PRINT-DETAIL-LINE.                                           
348800                                                                          
348900     MOVE 'Final Spch. Amount:'      TO DTL-LINE-TOT.                     
349000     ADD TTL-TTL-SPCH-AMT-ADJ        TO TTL-TTL-SPCH-AMT.                 
349100     MOVE TTL-TTL-SPCH-AMT           TO DTL-AMT-TOT-P.                    
349200     PERFORM PRINT-DETAIL-LINE.                                           
349400     PERFORM PRINT-BLANK-LINE 2 TIMES.                                    
349500                                                                          
      *CTS - 06/05/07 CHANGE BEGINS                                             
348900     MOVE 'Total Comm. Amount:'      TO DTL-LINE-TOT.                     
349000     MOVE TTL-TTL-ALL-COM-AMT        TO DTL-AMT-TOT-P.                    
349200     PERFORM PRINT-DETAIL-LINE.                                           
349400     PERFORM PRINT-BLANK-LINE. 

348900     MOVE 'Total Tax Amount:'        TO DTL-LINE-TOT.                     
349000     MOVE TTL-TTL-ALL-TAX-AMT        TO DTL-AMT-TOT-P.                    
349200     PERFORM PRINT-DETAIL-LINE.                                           
349400     PERFORM PRINT-BLANK-LINE.                                            
      *CTS - 06/05/07 CHANGE ENDS                                               
349600*>   Net Amount                                                           
349700                                                                          
349800     MOVE 'Total Net Amount:'        TO DTL-LINE-TOT.                     
349900     MOVE TTL-TTL-NET-AMT            TO DTL-AMT-TOT-P.                    
350000     PERFORM PRINT-DETAIL-LINE.                                           
399500                                                                          
399200     MOVE 'Total Reverse Pub.'       TO DTL-LINE.                         
DW-08      move     WK-TTL-CC-AMT-N        to dtl-amt-tot-p                     
399300***  MOVE TTL-NET-AMT                TO RPT-NET-AMT-P.                    
399400     PERFORM PRINT-DETAIL-LINE.                                           
399500    
350200     MOVE 'Total Net Adj. Amount:'   TO DTL-LINE-TOT.                     
350300     MOVE TTL-TTL-NET-AMT-ADJ        TO DTL-AMT-TOT-P.                    
350400     PERFORM PRINT-DETAIL-LINE.                                           
350500                                                                          
399200     MOVE 'Total Digital Orders Net' TO DTL-LINE.                         
DW-08      move ttl-digital-net-amt        to dtl-amt-tot-p                     
399400     PERFORM PRINT-DETAIL-LINE.                                           

350600     MOVE ALL '-'                    TO DTL-AMT-TOT-X.                    
350700     PERFORM PRINT-DETAIL-LINE.                                           
350800                                                                          
350900     MOVE 'Final Net Amount:'        TO DTL-LINE-TOT.                     
           add ttl-digital-net-amt         to TTL-TTL-NET-AMT
351000     ADD TTL-TTL-NET-AMT-ADJ         TO TTL-TTL-NET-AMT.                  
351100     MOVE TTL-TTL-NET-AMT            TO DTL-AMT-TOT-P.                    
351200     PERFORM PRINT-DETAIL-LINE.                                           
351300                                                                          
351400     PERFORM PRINT-BLANK-LINE 2 TIMES.                                    
351500                                                                          
********   MOVE 'Pre-Paid Credit Card Amount:' TO DTL-LINE-TOT.                 
AL-07      MOVE 'Pre-Paid PPD Amount:'         TO DTL-LINE-TOT.                 
AL-07      MOVE PPD-CC-AMT                 TO DTL-AMT-TOT-P.                    

AL-07      PERFORM PRINT-DETAIL-LINE.                                           
AL-07                                                                           
********   MOVE 'Pre-Paid PPD Amount:        ' TO DTL-LINE-TOT.                 
*******    MOVE PPD-PPD-AMT                TO DTL-AMT-TOT-P.                    
*******    PERFORM PRINT-DETAIL-LINE.                                           

AL-07      MOVE ALL '-'                    TO DTL-AMT-TOT-X.                    
AL-07      PERFORM PRINT-DETAIL-LINE.                                           
AL-07                                                                           
AL-07      MOVE 'Total Invoice Amount:  '  TO DTL-LINE-TOT.                     
AL-07      ADD  PPD-CC-AMT                 TO TTL-TTL-NET-AMT.                  
AL-07      ADD  PPD-PPD-AMT                TO TTL-TTL-NET-AMT.                  
AL-07      MOVE TTL-TTL-NET-AMT            TO DTL-AMT-TOT-P.                    
AL-07      PERFORM PRINT-DETAIL-LINE.                                           
AL-07                                                                           
AL-07      PERFORM PRINT-BLANK-LINE 2 TIMES.                                    
351600*>   Page count for invoices                                              
351700                                                                          
351800     MOVE 'Total Page Count:'        TO DTL-LINE-TOT.                     
351900     MOVE TTL-PAGE-CTR               TO DTL-AMT-TOT-P.                    
352000     PERFORM PRINT-DETAIL-LINE.                                           
352100     PERFORM PRINT-BLANK-LINE.                                            
352200                                                                          
352300*>   Total number of invoices produced                                    
352400                                                                          
352500     MOVE 'Total Invc Count:'        TO DTL-LINE-TOT.                     
352600     MOVE TTL-INVC-NBR               TO DTL-AMT-TOT-P.                    
352700     PERFORM PRINT-DETAIL-LINE.                                           
352800     PERFORM PRINT-BLANK-LINE.                                            
352900                                                                          
353000*>   Total number of WO processed                                         
353100                                                                          
353200     MOVE 'Total W.O. Count:'        TO DTL-LINE-TOT.                     
353300     MOVE TTL-WO-NBR                 TO DTL-AMT-TOT-P.                    
353400     PERFORM PRINT-DETAIL-LINE.                                           
353500     PERFORM PRINT-BLANK-LINE.                                            
353600                                                                          
353700*>   Total number of accounts processed                                   
353800                                                                          
353900     MOVE 'Total Acct Count:'        TO DTL-LINE-TOT.                     
354000     MOVE TTL-ACCT-NBR               TO DTL-AMT-TOT-P.                    
354100     PERFORM PRINT-DETAIL-LINE.                                           
354200     PERFORM PRINT-BLANK-LINE.                                            
354300                                                                          
354400*>   Total amount of units billed - Lines                                 
354500                                                                          
354600     MOVE 'Total Unit Amount - Lines:'                                    
354700                                     TO DTL-LINE-TOT.                     
354800     MOVE TTL-UNIT-AMT-LINE          TO DTL-AMT-TOT-P.                    
354900     PERFORM PRINT-DETAIL-LINE.                                           
355000     PERFORM PRINT-BLANK-LINE.                                            
355100                                                                          
355200*>   Total amount of units billed - Inches                                
355300                                                                          
355400     MOVE 'Total Unit Amount - Inches:'                                   
355500                                     TO DTL-LINE-TOT.                     
355600     MOVE TTL-UNIT-AMT-INCH          TO DTL-AMT-TOT-P.                    
355700     PERFORM PRINT-DETAIL-LINE.                                           
355800     PERFORM PRINT-BLANK-LINE.                                            
355900                                                                          
356000*>   Total amount of units billed                                         
356100                                                                          
356200     MOVE 'Total Unit Amount:'       TO DTL-LINE-TOT.                     
356300     MOVE TTL-UNIT-AMT               TO DTL-AMT-TOT-P.                    
356400     PERFORM PRINT-DETAIL-LINE.                                           
356500     PERFORM PRINT-BLANK-LINE.                                            
356600                                                                          
359100                                                                          
359200     PERFORM DUMP-TOT-TABLE                                               
359300     VARYING TOT-X FROM 1 BY 1                                            
359400       UNTIL TOT-X > TOT-TABLE-COUNT.                                     

           DISPLAY 'wk-CLAS-amt '       wk-CLAS-amt 
           DISPLAY 'wk-CST-amt  '   	wk-CST-amt  
           DISPLAY 'wk-FSIN-amt ' 	wk-FSIN-amt 
           DISPLAY 'wk-MG-amt   '   	wk-MG-amt   
           DISPLAY 'wk-MGL-amt  '  	wk-MGL-amt  
           DISPLAY 'wk-NCOM-amt ' 	wk-NCOM-amt 
           DISPLAY 'wk-NYT-amt  '  	wk-NYT-amt  
           DISPLAY 'wk-NYTL-amt ' 	wk-NYTL-amt 
           DISPLAY 'wk-TA48-amt ' 	wk-TA48-amt 
           DISPLAY 'wk-TMAG-amt ' 	wk-TMAG-amt 
           DISPLAY 'wk-TTR-amt  '  	wk-TTR-amt  
           
359500                                                                  
359600 DUMP-TOT-TABLE.                                                  
359700*---------------*                                                 
                                                                        
359800     MOVE TOT-COUNT (TOT-X)          TO WK-DISPLAY.               
359900     DISPLAY SPACE.                                               
360000     DISPLAY '==============================================='.   
360100     DISPLAY '==============================================='.   
360200     DISPLAY '==== Total Pages for Route ' TOT-ROUTE (TOT-X)      
360300             SPACE WK-DISPLAY ' ========'.                        
360400     DISPLAY '==============================================='.   
360500     DISPLAY '==============================================='.   
360700/                                                                 
360800 PAGE-EJECT.                                                      
360900*----------*                                                      
                                                                        
361000     CALL 'AMSPRNT' USING PRT-EJECT, PRT-REC.                     
361100     MOVE 2                      TO PRT-SKIP.                     
361200                                                                  
361300 PRINT-BLANK-LINE.                                                
361400*-----------------*                                               
                                                                        
361500     MOVE SPACES                 TO PRT-REC.                      
361600     CALL 'AMSPRNT' USING PRT-ONE, PRT-REC.                       
361700                                                                  
361800 PRINT-DETAIL-LINE.                                               
361900*-----------------*                                               
                                                                        
362000     CALL 'AMSPRNT' USING PRT-SKIP, DTL-LINE-TOT.                 
362100     MOVE 1                      TO PRT-SKIP.                     
362200     MOVE SPACES                 TO PRT-REC                       
362300                                    DTL-LINE-TOT.                 
362400/                                                                 
362500 WRITE-BLANK-LINE.                                                
362600*----------------*                                                
                                                                        
362700     MOVE SPACES                 TO DTL-LINE-BLANK.               
362800     MOVE WK-CB                  TO DTL-P-CB.                     
362900     MOVE WK-FONT                TO DTL-P-FONT.                   
363000     MOVE DTL-LINE-BLANK         TO INVOICE-REC.                  
363100     PERFORM WRITE-LINE.                                          
363200                                                                  
363300 WRITE-DETAIL-NONB.                                               
363400*-----------------*                                               
                                                                        
363500     IF  DTL-DESCRIPTION NOT = SPACES                             
363600         PERFORM WRITE-DETAIL-LINE.                               
363700                                                                  
363800 WRITE-DETAIL-LINE.                                               
363900*-----------------*                                               
                                                                        
364000     IF  WK-LINE-CTR > WK-PAGE-DETAILS                            
364100         PERFORM PROCESS-HEADER-2.                                
364200                                                                  
364300     PERFORM WRITE-DETAIL-LINE-2.                                 
364400                                                                  
364500 WRITE-DETAIL-LINE-2.                                             
364600*-------------------*                                             
                                                                        
364700     MOVE '2'                    TO DTL-1-FONT.                   
364800     MOVE DTL-LINE               TO INVOICE-REC.                  
364900     PERFORM WRITE-LINE.                                          
365000     MOVE SPACES                 TO DTL-LINE.                     
365100                                                                  
365200 WRITE-LINE.                                                      
365300*----------*                                                      
365400     ADD 1                       TO PAGE-TABLE-COUNT              
365500                                    WK-LINE-CTR.                  
365600                                                                  
365700     IF  PAGE-TABLE-COUNT > PAGE-TABLE-MAX                        
365800         DISPLAY 'Page Table Overflow'                            
365900     ELSE                                                         
366000         SET PAGE-X               TO PAGE-TABLE-COUNT             
366100         MOVE INVOICE-REC         TO PAGE-DATA       (PAGE-X)     
366200         MOVE NA2-TYPE            TO PAGE-ACTT       (PAGE-X)     
366300         MOVE WK-ROUTE            TO PAGE-ROUTE      (PAGE-X)     
AL-06          MOVE WK-GROUP-BUY        TO PAGE-GROUP-BUY  (PAGE-X)     
366400         MOVE WK-INVC-COUNT       TO PAGE-INVC-COUNT (PAGE-X)     
366500         MOVE WK-PAGE-CTR         TO PAGE-PAGE-NBR   (PAGE-X)     
366600         MOVE PAGE-TABLE-COUNT    TO PAGE-LINE-NBR   (PAGE-X)     
366700***      MOVE WK-SORT-NAME        TO PAGE-ACCT-NBR   (PAGE-X).    
366700         MOVE FOOT-ACCT-NBR       TO PAGE-ACCT-NBR   (PAGE-X).    
NYTMS3         MOVE NA2-EURO-TEL-NBR (11:1)                             
NYTMS3                                   TO PAGE-STMT-BREAK (PAGE-X).   
NYTMS3         IF NA2-EURO-TEL-NBR (11:1) = SPACES                      
NYTMS3            MOVE 'N'               TO PAGE-STMT-BREAK (PAGE-X).   
366800                                                                  
366900 WRITE-FOOTER-5.                                                  
367000*--------------*                                                  
                                                                        
367100     MOVE '5'                    TO FOOT-1-CB.                    
367200     MOVE '4'                    TO FOOT-1-FONT.                  
367300     MOVE FOOT-LINE-1            TO INVOICE-REC.                  
367400     PERFORM WRITE-LINE.                                          
367500                                                                  
367600 WRITE-FOOTER-6.                                                  
367700*--------------*                                                  
                                                                        
      ***  IF  WK-GL-SUBLED = '11' OR '16' OR '37'                      
               MOVE SPACE              TO FOOT-DOCUMENT  
al-01          MOVE WO-INVC-NBR        TO WS-INV-NBR-NEW
al-01          MOVE WS-INV-NBR-NEW(7:12)   TO wk-invc-tmp               
               MOVE WK-INVC-TMP-6      TO FOOT-DOCUMENT-6.              
                                                                        
367800     MOVE '6'                    TO FOOT-2-CB.                    
367900     MOVE '1'                    TO FOOT-2-FONT.                  
368000     MOVE FOOT-LINE-2            TO INVOICE-REC.                  
368100     PERFORM WRITE-LINE.                                          
368200                                                                  
368300 WRITE-PAGE-TABLE.                                                
368400*----------------*                                                
                                                                        
368500     PERFORM ADD-TOT-TABLE.                                       
368600                                                                  
368700     PERFORM WRITE-PAGE-TABLE-2                                   
368800     VARYING PAGE-X FROM 1 BY 1                                   
368900       UNTIL PAGE-X > PAGE-TABLE-COUNT.                           
369000                                                                  
369100 WRITE-PAGE-TABLE-2.                                              
369200*------------------*                                              
                                                                        
369300     IF  WK-TOTAL-TYPE = 'C'                                      
369400         PERFORM CONTRACT-TOTAL.                                  
369500                                                                  
369600     MOVE PAGE-LINE (PAGE-X)     TO INVOICE-REC.                  
369700     WRITE INVOICE-REC.                                           
369800                                                                  
369900 CONTRACT-TOTAL.                                                  
370000*--------------*                                                  
                                                                        
370100     IF  PAGE-LINE-NBR (PAGE-X) = 3                               
370200         MOVE PAGE-DATA (PAGE-X)     TO HDR-LINE-2                
370300         MOVE FOOT-TOT-AMT-X         TO HDR-AMOUNT-X              
370400         MOVE HDR-LINE-2             TO PAGE-DATA (PAGE-X)        
370500     ELSE                                                         
370600         IF  PAGE-LINE-NBR (PAGE-X) = 5                           
370700             MOVE PAGE-DATA (PAGE-X) TO HDR-LINE-3                
370800             MOVE FOOT-TOT-AMT-X     TO HDR-AMOUNT-X              
370900             MOVE HDR-LINE-3         TO PAGE-DATA (PAGE-X).       
371000                                                                  
371100 ADD-TOT-TABLE.                                                   
371200*-------------*                                                   
                                                                        
371300     SET TOT-X                   TO 1.                            
371400                                                                  
371500     SEARCH TOT-LINE                                              
371600***      AT END PERFORM ADD-TOT-NEW                               
371700       WHEN TOT-LINE  (TOT-X) = LOW-VALUES                        
371800            PERFORM ADD-TOT-NEW                                   
371900       WHEN TOT-ROUTE (TOT-X) = PAGE-ROUTE (1)                    
372000            ADD 1                TO TOT-COUNT (TOT-X).            
372100                                                                  
372200 ADD-TOT-NEW.                                                     
372300*-----------*                                                     
                                                                        
372400     ADD 1                       TO TOT-TABLE-COUNT.              
372500                                                                  
372600     IF  TOT-TABLE-COUNT > TOT-TABLE-MAX                          
372700         DISPLAY SPACE                                            
372800         DISPLAY '===================================='           
372900         DISPLAY '======= Total Table Overflow ======='           
373000         DISPLAY '===================================='           
373100     ELSE                                                         
373200         MOVE PAGE-ROUTE (1)     TO TOT-ROUTE (TOT-X)             
373300         MOVE 1                  TO TOT-COUNT (TOT-X).            
373400                                                                  
373600 RETURN-SSL-READ-WO.                                              
373700*------------------*                                              
373800                                                                  
373900     RETURN SSL-FILE                                              
374000         AT END MOVE 'EOF'       TO SSL-FILE-STATUS.              
374100                                                                  
374200     IF  SSL-FILE-STATUS NOT = 'EOF'     
               display 'return: ' SSL-ACCT-NBR ' '
                       ssl-pub ' '
                       ssl-ref-nbr ' '
                       ssl-issue ' '
                       ssl-job-nbr
374300         IF  SSL-CATEGORY    =  LOW-VALUE                         
374400             MOVE SSL-REF-NBR-X  TO SSL-PUB                       
374500             MOVE SSL-REF-NBR-2  TO SSL-REF-NBR.                  
AL-11                                                                   
374600                                                                  
374800 SORT-OUT-EXIT.                                                   
375000          EXIT.                                                   
375100/                                                                 
375200 SORT-IN SECTION.                                                 
375300*---------------*                                                 
375600                                                                  
375700     PERFORM PROCESS-PARMS.                                       

           Perform Get-Bsel-File-Names
           move zeros                  to ttl-digital-gross-amt              
                                          ttl-digital-net-amt
           
           if  bsel-entry > 0
               move 'Y'                to wk-multi-bsel   
               Perform Varying bsel-x from 1 by 1
                   Until bsel-x > bsel-entry
                       move bsel-name (bsel-x)
                                       to prm-file-name
                       display space
                       display 'prm-file-name ' 
                           prm-file-name
                       Perform open-read-prm-file 
               end-perform
               display '===== bsel loop done ====='
               move 'N'                to wk-multi-bsel   
               gO TO SORT-IN-EXIT                                                                 
           end-if
           
*********  go to exit-mod         
           
375900     OPEN INPUT SEL-FILE.                                         
376000     MOVE 'OPN'                  TO SEL-FILE-STATUS.              
376100     
376200     PERFORM READ-SEL.                                            
376300     PERFORM CREATE-SSL-REC                                       
376400       UNTIL SEL-FILE-STATUS = 'EOF'                              
376500                                                                  
376600     CLOSE SEL-FILE.                                              
376700     MOVE 'CLS'                  TO SEL-FILE-STATUS.

376900     GO TO SORT-IN-EXIT.                                          
377100 
377200 PROCESS-PARMS-ACR.     
           display 'PROCESS-PARMS-acr start'                                    
           open input acr-parm           
           move space              to wk-acr-parm-rec    
                                      parm-acr-1nfo  
           read acr-parm into wk-acr-parm-rec              
                at end move 'Y' to wk-acr-parm-eof.    

	       move 0                  to wk-parm-acr-count
		   
	       perform varying wk-x from 1 by 1
		       until wk-x > 10
			   display 'parm-acr-in (wk-x) '
			              parm-acr-in (wk-x)
			       if  parm-acr-in (wk-x) not = space
				      add 1      to wk-parm-acr-count
				      move parm-acr-in (wk-x)
                                 to parm-acr-code
                                (wk-parm-acr-count)
                      display ' adding ' parm-acr-code								   
             				 (wk-parm-acr-count)
				end-if
           end-perform	
		   
		   display 'parm acr info'  
		   display 'parm-acr-rows ' wk-parm-acr-count
           display 'parm-acr-rec   ' wk-acr-parm-rec
		   display 'parm-acr-ind   ' parm-acr-ind 
		   display 'parm-acr-1nfo ' parm-acr-1nfo
           close  acr-parm
           display 'PROCESS-PARMS-acr end'   
                                                             
377200 PROCESS-PARMS.                                                   
377300*-------------*                                                   
377400                                                                  
377500     PERFORM CALL-AMSCRD.                                         
377600                                                                  
377700     IF  STATUS-CODE = ZEROS                                      
377800         PERFORM PROCESS-PARMS-2.                                 
377900                                         
           perform PROCESS-PARMS-ACR.
		   
378100 PROCESS-PARMS-2.                                                 
378200*---------------*                                                 
378300                                                                  
378400     MOVE 'PUB'                  TO COD-KEY.                      
378500     MOVE PARM-PUB               TO COD-CODE1.                    
378600     PERFORM CALL-GIT-COD.                                        
                                                                        
378700     IF  STATUS-CODE NOT = ZEROS  
               move 1                  to return-code
378800         CALL 'AMSABRT' USING IN01.                               
378900                                                                  
379000     MOVE PARM-PFM-DATE          TO UTL-EDIT-DATE-X.              
379100     PERFORM EDIT-DATE.                                           
                                                                        
379200     MOVE UTL-EDIT-DATE          TO WK-PFM-DATE.                  
379300 
           display 'PARM-INVC-DATE-f ' PARM-INVC-DATE-f
           display 'PARM-INVC-DATE-t ' PARM-INVC-DATE-t
           display 'PARM-INVC-DATE-p ' PARM-INVC-DATE-p
           
379400     IF  PARM-ACTT-CODE NOT = SPACE AND 'S'      
               move 1                  to return-code
379500         CALL 'AMSABRT' USING IN18.                               
379600                                                                  
379700     MOVE PARM-INVC-DATE-P       TO UTL-EDIT-DATE-X.              
379800     PERFORM EDIT-DATE.                                           
                                                                        
379900     MOVE UTL-EDIT-DATE          TO WK-INVC-DATE-P.               
379700     MOVE PARM-INVC-DATE-F       TO UTL-EDIT-DATE-X.              
380000                                                                  
379800     PERFORM EDIT-DATE.  
AL-01A     MOVE UTL-EDIT-DATE          TO  CTL-REC-FROM.                
379800     PERFORM EDIT-OUT-DATE.                                       
379900     MOVE UTL-EDIT-DATE-SL       TO WK-INVC-DATE-F.               
380000                                                                  
379700     MOVE PARM-INVC-DATE-T       TO UTL-EDIT-DATE-X.              
                                                                        
379800     PERFORM EDIT-DATE.                                           
AL-01A     MOVE UTL-EDIT-DATE          TO  CTL-REC-TO.                  
379800     PERFORM EDIT-OUT-DATE.                                       
379900     MOVE UTL-EDIT-DATE-SL       TO WK-INVC-DATE-T.

           display 'WK-INVC-DATE-p ' WK-INVC-DATE-P
           display 'WK-INVC-DATE-f ' WK-INVC-DATE-f
           display 'WK-INVC-DATE-f ' WK-INVC-DATE-t
           
AL-01A     IF PARM-TYPE = SPACES or PARM-ORG = SPACES  
               move 1                  to return-code
AL-01A        CALL 'AMSABRT' USING IN18.
AL-01A     MOVE PARM-TYPE              TO WK-INVC-RUN.                  
AL-01A     MOVE PARM-ORG               TO WK-INVC-ORG.                  
380000     display 'WK-INVC-ORG ' WK-INVC-ORG ' '
                   'WK-INVC-RUN ' WK-INVC-RUN
380200 CALL-AMSCRD.                                                     
380300*-----------*                                                     
380400                                                                  
380500     CALL 'AMSCRD' USING PARM-CARD IO-PKT. 
           display parm-card
380600                                                                  
380800 CREATE-SSL-REC.                                                  
380900*--------------*                                                  
                                                                        
381000*>   Do not precess select file records that refer to             
381100*>   pending orders or orders which may have been deleted         
381200*>   by bilupd (Installation Option).                             
381300                     
381400     IF  SEL-STATUS NOT = 'P'                                     
381500         MOVE SEL-REC            TO SSL-REC                       
AL-20          PERFORM CALL-GIT-WO                                      47161001
AL-08          move wo-ad-posn         to wk-ad-posn
               if  digital-posn
               and wo-pub = 'CLAS'
                   add wo-gross-amt    to ttl-digital-gross-amt
                   add wo-invc-amt     to ttl-digital-net-amt
                   display  'skipping digital order ' wo-job-nbr ' '
                   display  'wo-gross ' wo-gross-amt ' ' 
                            'wo-invc ' wo-invc-amt ' ' 
                            'wo-disc ' WO-DISC-TOT ' ' 
                            'ttl gross ' ttl-digital-gross-amt ' ' 
                            'ttl net ' ttl-digital-net-amt ' '                             
               else
381700             PERFORM CREATE-SSL-REC-2
               end-if
           end-if.
381800                                                                  
381900     PERFORM READ-SEL.                                            
382000                                                                  
382200 CREATE-SSL-REC-2.                                                
382300*----------------*                                                
382400                                                                  
382500     MOVE 'ACR'                  TO COD-KEY.                      
382600     MOVE SSL-WO-ACR-CODE        TO COD-CODE1.                    
382700     PERFORM CALL-GIT-COD.                                        
382800                                                                  
382900     IF  STATUS-CODE =  ZEROS                                     
383000     AND COD-FLAG1   = 'N'                                        
383100         NEXT SENTENCE                                            
383200     ELSE                                                         
383300         PERFORM CREATE-SSL-REC-3                                 
383400         PERFORM REGROUP-SORT-KEY 
00             PERFORM search-acr-code 
           end-if
383600                                                                 
        search-acr-code.
           display 'search-acr-code - start'		
		   if  no-acr-parm     
               display '**no acr parm**'		   
		       Perform release-to-sort
		   else
		       move 0                      to wk-parm-x			
		       move 'N'                    to wk-parm-acr-march
		   
   	           perform varying wk-parm-x from 1 by 1
		           until wk-parm-acr-march = 'Y'			   
			       or    wk-parm-x > wk-parm-acr-count

                     if  parm-acr-code (wk-parm-x)
                             = SSL-WO-ACR-CODE
                         move 'Y'      to wk-parm-acr-march
                       end-if	
           end-perform  
			
           display SSL-WO-ACR-CODE ' '
			    'match ' wk-parm-acr-march ' '			
			             'parm-acr-ind '
				  parm-acr-ind ' ' 
				   ssl-job-nbr ' '
				   
     	   if  parm-acr-ind = 'I'
               Perform acr-include
           else
               Perform acr-exclude
		   end-if
		   
		   display 'search-acr-code - end'  
		   
       acr-include
           if  wk-parm-acr-march = 'Y'
			   display '***include by acr***'
      		   Perform release-to-sort
		   else
		       display '***exclude by acr***'
		   end-if	
				
       acr-exclude
           if  wk-parm-acr-march = 'Y'
               display '***exclude by acr***'
           else
		       display '***include by acr***'
      		   Perform release-to-sort
		   end-if
	   
       release-to-sort.			   
               display 'releese ' 
			   SSL-WO-ACR-CODE ' '
			           SSL-ACCT-NBR ' '
                       ssl-agy-acct-nbr ' ' 
                       ssl-pub ' '
                       ssl-issue ' '
                       ssl-job-nbr ' '
					   ssl-agy-type ' ' 
                       ssl-wo-acr-code ' ' 
                       ssl-edition
                   RELEASE SSL-REC. 
		  
383800 CREATE-SSL-REC-3.                                                
383900*----------------*  
         display 'CREATE-SSL-REC-3 - start'

           if  wk-multi-bsel= 'Y'
	        Perform Setup-Parm-Dates
           end-if     

384100     MOVE SSL-ISSUE              TO SSL-ISSUE-WO.                 
384200     MOVE SSL-EDITION            TO SSL-EDITION-WO.               
384300     MOVE SSL-WO-PAGE            TO SSL-PAGE-WO.                  
384400     MOVE SSL-ACCT-NBR           TO NAD-ACCT-NBR.                 
384500                                                                  
384600     PERFORM GET-ADV.                                             
384700     MOVE NAD-TYPE               TO SSL-ADV-TYPE                  
384800                                    WK-NAD-TYPE.                  

           move nad-name-1             to utl-name
           Perform create-key
           move utl-key-nam            to ssl-adv-key 
                                          ssl-agy-key
           display '1 ' nad-name-1 ' ' ssl-adv-key   
                                       ssl-agy-key
           
385000     IF  NAD-TYPE    = 'NM'                                       
385100     OR  WK-NAD-TYPE = 'T'                                        
385200         MOVE LOW-VALUE          TO SSL-CATEGORY                  
385300         MOVE SSL-REF-NBR        TO SSL-REF-NBR-2                 
385400         MOVE SSL-PUB            TO SSL-REF-NBR-X                 
385500         MOVE SSL-ISSUE-WO-X     TO SSL-PUB                       
385600         MOVE ZEROS              TO SSL-ISSUE-WO.                 
385700                                                                  
385800*>   Get parent info into parent table                            
                                                                        
385900     PERFORM GET-PARENT-INFO.                                     
386000                                                                  
386100     IF  PARENT-ACCT-NBR (2) NOT = ZEROS                          
386200         MOVE PARENT-TYPE (2)     TO SSL-ADV-PAR-TYPE             
386300         MOVE PARENT-ACCT-KEY (2) TO SSL-ADV-PAR-KEY              
386400         MOVE PARENT-ACCT-NBR (2) TO SSL-ADV-PAR-NBR              
386500     ELSE                                                         
386600     IF  PARENT-ACCT-NBR (1) NOT = ZEROS                          
386700         MOVE PARENT-TYPE (1)     TO SSL-ADV-PAR-TYPE             
386800         MOVE PARENT-ACCT-KEY (1) TO SSL-ADV-PAR-KEY              
386900         MOVE PARENT-ACCT-NBR (1) TO SSL-ADV-PAR-NBR              
387000     ELSE                                                         
387100         MOVE SSL-ADV-TYPE       TO SSL-ADV-PAR-TYPE              
387200         MOVE SSL-ADV-KEY        TO SSL-ADV-PAR-KEY               
387300         MOVE SSL-ACCT-NBR       TO SSL-ADV-PAR-NBR.              
387400                                                                  
387500     MOVE SPACE                  TO SSL-NAT-AGY.                  
                                                                        
387600     IF  SSL-AGY-ACCT-NBR = ZEROS              
387700         MOVE SSL-ADV-PAR-TYPE   TO SSL-AGY-TYPE                  
387800         MOVE SSL-ADV-PAR-KEY    TO SSL-AGY-KEY                   
387900         MOVE SSL-ADV-PAR-NBR    TO SSL-AGY-ACCT-NBR              
388000     ELSE                                                         
388100         MOVE SSL-AGY-ACCT-NBR   TO NAD-ACCT-NBR 
388200         PERFORM GET-ADV            
               move nad-name-1         to utl-name
               Perform create-key
               move utl-key-nam        to ssl-agy-key
               display '2 ' nad-name-1 ' ' ssl-adv-key   
                                       ssl-agy-key
               
388300         MOVE NAD-TYPE           TO SSL-AGY-TYPE                  
388400         MOVE 'CLSS'             TO COD-KEY                       
388500         MOVE NAD-CLASS-CODE     TO COD-CODE1                     
388600         PERFORM CALL-GIT-COD                                     
                                                                        
388700         IF  STATUS-CODE =  ZEROS                                 
388800         AND COD-FLAG4   = 'Y'                                    
388900             MOVE 'Y'            TO SSL-NAT-AGY.                  
389000                                                                  
389100     IF  PARM-ACTT-CODE = 'S'                                     
389200         MOVE SSL-AGY-TYPE       TO WK-ACTT-CODE                  
389300         MOVE WK-ACTT-CODE       TO SSL-AGY-TYPE                  
389400         MOVE SSL-ADV-PAR-TYPE   TO WK-ACTT-CODE                  
389500         MOVE WK-ACTT-CODE       TO SSL-ADV-PAR-TYPE              
389600         MOVE SSL-ADV-TYPE       TO WK-ACTT-CODE                  
389700         MOVE WK-ACTT-CODE       TO SSL-ADV-TYPE.                 
389800                                                                  
          display 'CREATE-SSL-REC-3 - end'
          
390000 REGROUP-SORT-KEY.                                                
390100*----------------*                                                
            display 'REGROUP-SORT-KEY - start'

390300*>   Setup SSL-SORT-AGY Subkey                                    
390400                                      
390500     MOVE SSL-AGY-TYPE           TO SSL-S-AGY-TYPE
390600     MOVE SSL-AGY-KEY            TO SSL-S-AGY-KEY.                
390700     MOVE SSL-AGY-ACCT-NBR       TO SSL-S-AGY-ACCT-NBR.           
390800                                                                  
390900*>   Setup SSL-SORT-ADV-PAR Subkey                                
391000                                                                  
391100     MOVE SSL-CATEGORY           TO SSL-S-CATEGORY.               
391200                                                                  
391300     IF  SSL-AGY-ACCT-NBR = ZEROS                                 
391400         MOVE 1                  TO STATUS-CODE                   
391500     ELSE                                                         
               MOVE SSL-AGY-ACCT-NBR   TO NAD-ACCT-NBR                  
               PERFORM GET-ADV                        
               MOVE 'CLSS'             TO COD-KEY                       
               MOVE NAD-CLASS-CODE     TO COD-CODE1                     
               PERFORM CALL-GIT-COD.                                    
392000                                                                  
392100     IF  STATUS-CODE      =  ZEROS                                
392200     AND COD-FLAG4        = 'J'                                   
392300     AND SSL-CATEGORY NOT =  LOW-VALUE                            
392400         MOVE SSL-ADV-JXRF-NBR   TO SSL-S-ADV-PAR-KEY             
392500     ELSE                                                         
392600         MOVE SSL-ADV-PAR-KEY    TO SSL-S-ADV-PAR-KEY.            
392700                                                                  
392800     MOVE SSL-ADV-PAR-NBR        TO SSL-S-ADV-PAR-NBR.            
392900                                                                  
393000*>   Setup SSL-SORT-ADV Subkey                                    
393100                                                                  
393200     MOVE SSL-ADV-KEY            TO SSL-S-ADV-KEY.                
393300     MOVE SSL-ACCT-NBR           TO SSL-S-ACCT-NBR.               
393400                                                                  
393500*>   Setup SSL-SORT-PUB-CNT Subkey                                
393600                                                                  
393700     MOVE SSL-PUB                TO SSL-S-PUB.                    
393800                                                                  
393900     IF  SSL-CATEGORY = LOW-VALUE                                 
394000         MOVE SSL-REF-NBR-X      TO SSL-S-AD-TYPE                 
394100         MOVE SSL-AD-TYPE        TO SSL-S-REF-NBR-X               
394200     ELSE                                                         
394300         MOVE SSL-AD-TYPE        TO SSL-S-AD-TYPE                 
394400         MOVE SSL-REF-NBR-X      TO SSL-S-REF-NBR-X.              
394500                                                                  
394600*>   Setup SSL-SORT-WO Subkey                                     
394700                                                                  
CTS-02     IF ((SSL-PUB  = WK-CLS-PUB OR WK-MAGC-PUB OR WK-IHTC-PUB)    
CTS-02     AND  SSL-CATEGORY NOT = LOW-VALUE)                           
CTS-02     OR ((SSL-REF-NBR-X = WK-CLS-PUB OR WK-MAGC-PUB OR            
CTS-02          WK-MAGC-PUB) AND  SSL-CATEGORY = LOW-VALUE)             
395200          MOVE SSL-WO-PROD-KEY   TO SSL-S-PROD-KEY                
395300         IF  SSL-CATEGORY = LOW-VALUE                             
395400             MOVE HIGH-VALUES    TO SSL-SORT-PUB-CNT              
395500         ELSE                                                     
395600             NEXT SENTENCE                                        
395700     ELSE                                                         
395800         MOVE LOW-VALUES         TO SSL-S-PROD-KEY.               
395900                                                                  
396000     MOVE SSL-ISSUE-WO-X         TO SSL-S-ISSUE-WO-X.             
396100     MOVE SSL-EDITION-WO         TO SSL-S-EDITION-WO.             
396200     MOVE SSL-PAGE-WO            TO SSL-S-PAGE-WO.                
396300     MOVE SSL-SEQ-KEY            TO SSL-S-SEQ-KEY.                
396400     MOVE SSL-ADJ-KEY            TO SSL-S-ADJ-KEY. 
           MOVE SSL-WO-GROUP-CODE      TO SSL-S-GROUP-CODE.
dwww       move sel-job-nbr            to WK-NT-JOB-NBR 
dwww       move  WK-NT-JOB-NBR-7       to ssl-job-nbr-7
AL-20      MOVE WO-INVC-NBR            TO WK-INV-NBR.                   48061001
           if  WO-CRED-MEMO not = 0
                display 'cred memo ' WO-CRED-MEMO
               move WO-CRED-MEMO       to wk-inv-nbr
           end-if
AL-****    MOVE WK-INV-NBR-6           TO SSL-S-INV-6.                  48061101
AL-20      MOVE WK-INV-NBR             TO SSL-S-INV                     48061101

********** override no  longer used jxrf so sort is by agy/adv
           move 'ADV'                  to ssl-bta-jxrf
           if  ssl-acct-nbr not = ssl-agy-acct-nbr
               move 'AGY'              to ssl-bta-jxrf
           end-if
           
           display 'regroup ' wo-job-nbr ' ' 
                              ssl-s-inv   ' ' 
           
                              ssl-s-inv-6 ' ' 
                              'adv ' SSL-ACCT-NBR ' ' 
                              'key ' SSL-ADV-KEY ' ' 
                              'agy ' SSL-AGY-ACCT-NBR ' ' 
                              'key ' SSL-Agy-KEY ' ' 
                              'jxrf ' ssl-bta-jxrf
                              
            display 'REGROUP-SORT-KEY - end'
            
398300 SORT-IN-EXIT.                                                    
398500         EXIT.                                                    
                                                        
      /                                                                 
398800 COMMON-ROUTINE SECTION.                                          
398900*----------------------*                                          
399000                                                                  
399100 GET-ADV.                                                         
399200*-------*                                                         
399300                                                                  
399400*>   ---  Get Advertiser Name and Address record                  
399500                                                                  
399600     PERFORM CALL-GIT-NAD.                                        
399700                                                                  
399800     IF  STATUS-CODE NOT = ZEROS  
               move 1                  to return-code
399900         CALL 'AMSABRT' USING IN05.                               
400000                                                                  
400200 GET-PARENT-INFO.                                                 
400300*---------------*                                                 
400400                                                                  
400500*>   ---  Expects valid account number in NAD-ACCT-NBR.           
400600*>   ---  puts immediate parent into PARENT-ENTRY (1).            
400700*>   ---  puts top most parent into PARENT-ENTRY (2).             
400800                                                                  
400900     MOVE LOW-VALUES             TO PARENT-TABLE.                 
401000                                                                  
401100     IF  NAD-ADDR-CODE = 'B'                                      
401200         PERFORM GET-PARENT-INFO-2.                               
401300                                                                  
401500 GET-PARENT-INFO-2.                                               
401600*-----------------*                                               
401700                                                                  
401800     MOVE NAD-PAR-ACCT-NBR       TO NA2-ACCT-NBR.                 
401900     SET PR-X                    TO 1.                            
402000                                                                  
402100     PERFORM GET-PARENT-INFO-3                                    
402200       UNTIL NA2-ACCT-NBR = ZEROS.                                
402300                                                                  
402500 GET-PARENT-INFO-3.                                               
402600*-----------------*                                               
402700                                                                  
402800     PERFORM CALL-GIT-NA2.                                        
402900                                                                  
403000     IF  STATUS-CODE NOT = ZEROS                                  
               move 1                  to return-code
403100         CALL 'AMSABRT' USING IN11.                               
403200                                                                  
403300     MOVE NA2-NAME-1             TO PARENT-NAME (PR-X).           
403400     MOVE NA2-TYPE               TO PARENT-TYPE (PR-X).           
403500     MOVE NA2-XRF-KEY            TO PARENT-ACCT-KEY (PR-X).       
403600     MOVE NA2-ACCT-NBR           TO PARENT-ACCT-NBR (PR-X).       
403700     MOVE NA2-PAR-ACCT-NBR       TO NA2-ACCT-NBR.                 
403800                                                                  
403900     IF  PR-X = 1                                                 
404000         SET PR-X UP BY 1.                                        
404100                                                                  
404300 GET-INVC-NBR.                                                    
404400*------------*                                                    
404500                                                                  
404600     MOVE 'NINB'                 TO COD-KEY.                      
404700     MOVE SSL-AGY-TYPE           TO WK-ACTT-CODE.                 
404800     MOVE WK-ACTT-CODE           TO COD-CODE1.                    
404900     PERFORM CALL-GIT-COD.                                        
405000                                                                  
405100     IF  STATUS-CODE NOT = ZEROS                                  
405200         MOVE SPACES             TO COD-CODE1                     
405300         PERFORM CALL-GIT-COD                                     
                                                                        
405400         IF  STATUS-CODE NOT = ZEROS                              
               move 1                  to return-code
405500             CALL 'AMSABRT' USING IN02.                           
405600                                                                  
405700     MOVE COD-VAL9-1             TO WK-INVC-TYPE.                 
405800     MOVE COD-VAL-9              TO WK-INVC-NBR.                  
405900                                                                  
406000 PUT-INVC-NBR.                                                    
406100*------------*                                                    
406200                                                                  
406300     MOVE 'NINB'                 TO COD-KEY.                      
406400     MOVE SSL-AGY-TYPE           TO WK-ACTT-CODE.                 
406500     MOVE WK-ACTT-CODE           TO COD-CODE1.                    
406600     CALL 'GETUP' USING COD-FILE, COD-REC, IO-PKT.                
406800                                                                  
406900     IF  STATUS-CODE NOT = ZEROS                                  
407000         MOVE SPACES             TO COD-CODE1                     
407100         CALL 'GETUP' USING COD-FILE, COD-REC, IO-PKT             
                                                                        
407300         IF  STATUS-CODE NOT = ZEROS                              
               move 1                  to return-code
407400             CALL 'AMSABRT' USING IN03.                           
407500                                                                  
407600     MOVE WK-INVC-NBR            TO COD-VAL-9.                    
407700                                                                  
407800     CALL 'PUT' USING COD-FILE COD-REC IO-PKT.                    
407900                                                                  
408000     IF  STATUS-CODE NOT = ZEROS                                  
               move 1                  to return-code
408100         CALL 'AMSABRT' USING IN04.                               
408200                                                                  
408400 GET-INVC-MSGS.                                                   
408500*-------------*                                                   
408600                                                                  
408700     MOVE SPACES                 TO WK-INVC-MSGS                  
408800                                    WK-MSG-KEY.                   
408900     MOVE ZEROS                  TO WK-MSG-CTR.                   
409000     SET WK-M                    TO 1.                            
409100                                                                  
409200     IF  SSL-AGY-ACCT-NBR < 500000000                             
409300         MOVE 'A'                TO WK-MSG-KEY1                   
409400     ELSE                                                         
409500         MOVE 'G'                TO WK-MSG-KEY1.                  
409600                                                                  
409700     MOVE SSL-AGY-TYPE           TO WK-MSG-KEY2.                  
409800     MOVE SPACE                  TO WK-MSG-KEY3.                  
409900                                                                  
410000     MOVE 'D'                    TO MSG-KEY.                      
410100     MOVE WK-MSG-KEY             TO MSG-NBR.                      
410200     MOVE ZEROS                  TO MSG-SEQ-NBR.                  
410300                                                                  
410400     PERFORM CALL-SETL-MSG.                                       
410500     PERFORM CALL-SGET-MSG.                                       
410600                                                                  
410700     IF  STATUS-CODE NOT = ZEROS                                  
410800     OR  MSG-NBR3    NOT = WK-MSG-PRIM                            
410900         MOVE SPACE              TO WK-MSG-KEY2-2                 
411000         MOVE WK-MSG-KEY         TO MSG-NBR                       
                                                                        
411100         PERFORM CALL-SETL-MSG                                    
411200         PERFORM CALL-SGET-MSG                                    
                                                                        
411300         IF  STATUS-CODE NOT = ZEROS                              
411400         OR  MSG-NBR3    NOT = WK-MSG-PRIM                        
411500             MOVE SPACES         TO WK-MSG-KEY2                   
411600             MOVE WK-MSG-KEY     TO MSG-NBR                       
                                                                        
411700             PERFORM CALL-SETL-MSG                                
411800             PERFORM CALL-SGET-MSG.                               
411900                                                                  
412000     PERFORM GET-INVC-MSGS-2                                      
412100       UNTIL STATUS-CODE NOT = ZEROS                              
412200          OR MSG-NBR3    NOT = WK-MSG-PRIM                        
412300          OR MSG-NBR1        = SPACE                              
412400          OR WK-M            > 5.                                 
412500                                                                  
412600     PERFORM CALL-ESETL-MSG.                                      
412700     PERFORM CHECK-STATUS.                                        
412900                                                                  
413000 GET-INVC-MSGS-2.                                                 
413100*---------------*                                                 
413200                                                                  
413300     ADD 1                       TO WK-MSG-CTR.                   
413400     MOVE MSG-TEXT               TO WK-INVC-MSG (WK-M).           
413500     SET WK-M UP                 BY 1.                            
413600                                                                  
413700     PERFORM CALL-SGET-MSG.                                       
413800                                                                  
414000 PUT-INVC-MSGS.                                                   
414100*-------------*                                                   
414200                                                                  
414300     IF  WK-MSG-CTR > ZEROS                                       
414400         PERFORM PUT-INVC-MSGS-2.                                 
414500                                                                  
414700 PUT-INVC-MSGS-2.                                                 
414800*---------------*                                                 
414900                                                                  
415000     SET WK-M                    TO 1.                            
415100                                                                  
415200     PERFORM PUT-INVC-MSGS-3                                      
415300       UNTIL WK-M > WK-MSG-CTR                                    
415400          OR WK-M > 5.                                            
415500                                                                  
415600     MOVE 'N'                    TO WK-FIRST-LINE.                
415700                                                                  
415900 PUT-INVC-MSGS-3.                                                 
416000*---------------*                                                 
416100                                                                  
416200     MOVE WK-INVC-MSG (WK-M)     TO DTL-INVC-MSG.                 
416300     SET WK-M UP                 BY 1.                            
416400                                                                  
416500*>   Detail line - Invoice message(s) (Free Form)                 
416600                                                                  
416700     PERFORM WRITE-DETAIL-LINE.                                   
416800                                                                  
417000 CALL-GIT-WO.                                                     
417100*-----------*                                                     
417200                                                                  
417300**   MOVE SSL-ACCT-NBR           TO WO-ACCT-KEY.                  
417400**   MOVE SSL-PUB                TO WO-PUB.                       
417500**   MOVE SSL-REF-NBR            TO WO-REF-NBR.                   
417600**   MOVE SSL-ISSUE              TO WO-ISSUE.                     
417700**   MOVE SSL-SEQ-KEY            TO WO-SEQ-KEY.                   
417800**   MOVE SSL-ADJ-KEY            TO WO-ADJ-NBR-KEY.               
418000**   CALL 'GIT'                  USING WO-FILE, WO-REC, IO-PKT.   
           IF wk-mega-override = 'Y'                                    
              MOVE wk-mega-JOB-NBR     TO WO-JOB-NBR                    
           ELSE                                                         
48917         MOVE SSL-JOB-NBR         TO WO-JOB-NBR.                   
418100*                                                                 
028918     CALL 'GIT' USING JNBR-WO WO-REC IO-PKT.                      
AL-06      display 'call git: '                 
AL-06           ssl-job-nbr ' '                                         
18100                                                                  
      *CTS - 02/05/07 CHANGE BEGINS                                     
418200*    IF  STATUS-CODE NOT = ZEROS   
               move 1                  to return-code
418300*        CALL 'AMSABRT' USING IN12.                               
418200     IF  STATUS-CODE NOT = ZEROS   

              MOVE  WO-PD-FLAG         TO WS-WO-PD-FLAG                 
              PERFORM EURO-TEST-PFLG                                    
              IF NOT VALID-PD-FLAG-SRCH                                 
               move 1                  to return-code              
418300           CALL 'AMSABRT' USING IN12                              
418400        END-IF                                                    
418400     END-IF.                                                      
      *CTS - 02/05/07 CHANGE ENDS   
al**       move wo-invc-nbr to invc-nbr.      
al**       CALL 'GIT' USING INBR-INVC, INVC-REC, IO-PKT.
           display 'git invc ' invc-nbr ' ' status-code
al**       IF STATUS-CODE NOT = 0                                       
                 display '++++++++++++problem ' invc-nbr
           else
                 display 'invc nbr ' invc-nbr
                          ' ' invc-net-amt
                          ' ' invc-job-nbr
                          ' ' invc-outsdg-amt.
418400                                                                  
AL-05                                                                   
AL-05      IF WO-PUB = 'CLAS'                                           
AL-05      IF WO-INVC-CODE = 'A' OR 'D' OR 'M' OR 'S' OR 'V'            
AL-05      IF WO-CC-NUMBER NOT > 0                                      
AL-05         PERFORM FORMAT-CC-INFO.                                   
AL-05                                                                   
418600 GET-J-XREF.                                                      
418700*----------*                                                      
418900     PERFORM GET-ADV.                                             
419000     PERFORM GET-J-XREF-2.                                        
419100                                                                  
419300 GET-J-XREF-2.                                                    
419400*------------*                                                    
419500                                                                  
419600     MOVE NAD-XRF-KEY            TO XRF-KEY                       
NEW-T1                                    WK-JXRF-KEY.                  
419700     PERFORM CALL-GIT-XRF.            
           display 'xrf ' xrf-key 
                   ' ' wk-jxrf-key
                   ' ' status-code
                   ' ' xrf-type
                   ' ' XRF-AREA-CODE
                   ' ' XRF-KEY-NAME
                   ' ' XRF-UNIQ 
                   ' ' xrf-name
                   ' ' xrf-comments
                   ' ' xrf-rec.                   
419800                                                                  
419900     IF  STATUS-CODE NOT = ZEROS   
               move 1                  to return-code
420000         CALL 'AMSABRT' USING IN16.                               
420100                                                                  
420200     MOVE XRF-COMMENTS           TO WK-JXRF-AREA.                 
420300                                                                  
420500 GET-COD-ISS.                                                     
420600*-----------*                                                     
420700                                                                  
420800     PERFORM CALL-GIT-COD.                                        
420900                                                                  
421000     IF  STATUS-CODE = ZEROS                                      
421100         PERFORM GET-COD-ISS-2.                                   
421200                                                                  
421400 GET-COD-ISS-2.                                                   
421500*-------------*                                                   
421600                                                                  
421700     SET COD-X                   TO 1.                            
                                                                        
421800     SEARCH COD-TABLE VARYING COD-X                               
421900         AT END                                                   
               move 1                  to return-code
422000            CALL 'AMSABRT' USING IN14                             
422100       WHEN WO-ISSUE NOT < COD-EFF-ISS (COD-X)                    
422200            NEXT SENTENCE.                                        
422300                                                                  
422400***  IF  COD-VAL (COD-X) = -999                                   
422500***      CALL 'AMSABRT'          USING IN15.                      
422600                                                                  
422800 GET-LINEAGE.                                                     
422900*-----------*                                                     
423000 
           display 'GET-LINEAGE ' 
                   wo-ad-quantity ' ' 
                   wo-size
423100     MOVE ZEROS                  TO WK-TEMP-LINEAGE-I             
423200                                    WK-TEMP-LINEAGE-D.            
423300                                                                  
423400     PERFORM GET-PUB-SIZE.                                        
423500                                                                  
423600     IF  COD-VAL-9 NOT > ZEROS                                    
423700         MOVE 1                  TO COD-VAL-9.                    
423800                                                                  
423900     IF  COD-FLAG2 = 'A'                                          
424000         IF  COD-FLAG1 = 'D'                                      
424100             DIVIDE WO-ACT-SIZE BY COD-VAL-9                      
424200                            GIVING WK-TEMP-LINEAGE-D 
**************************************** ROUNDED      
424400         ELSE                                                     
424500             DIVIDE WO-ACT-SIZE BY COD-VAL-9                      
424600                            GIVING WK-TEMP-LINEAGE-I              
424700     ELSE                                                         
424800         IF  COD-FLAG1 = 'D'                                      
424900             DIVIDE WO-SIZE BY COD-VAL-9                          
425000                        GIVING WK-TEMP-LINEAGE-D 
************************************ ROUNDED          
425200         ELSE                                                     
425300             DIVIDE WO-SIZE BY COD-VAL-9                          
425400                        GIVING WK-TEMP-LINEAGE-I.                 
425500                                                                  
425600     IF  COD-FLAG1 = 'D'                                          
425700         COMPUTE WK-TEMP-LINEAGE-I 
*****************************************ROUNDED 
                                    = WK-TEMP-LINEAGE-D * 1
425800     ELSE                                                         
425900         MOVE WK-TEMP-LINEAGE-I  TO WK-TEMP-LINEAGE-D.            
426000             
           display 'lineage-d ' WK-TEMP-LINEAGE-D
           display 'lineage-i ' WK-TEMP-LINEAGE-i           
426100 GET-FSI.                                                         
426200*-------*                                                         
426300                                                                  
426400     MOVE SPACE                  TO COD2-FLAG1.                   
426500                                                                  
426600     MOVE 'NIED'                 TO COD2-KEY.                     
426700     MOVE WO-PUB                 TO COD2-CODE1.                   
426800     MOVE WO-EDITION             TO COD2-CODE2.                   
426900     PERFORM CALL-GIT-COD2.                                       
427000                                                                  
427100     IF  STATUS-CODE NOT =  ZEROS                                 
427200     OR  COD2-FLAG1  NOT = 'I'                                    
427300         MOVE 'NISZ'             TO COD2-TYPE                     
427400         MOVE WO-AD-SIZE         TO COD2-CODE2                    
427500         MOVE WO-AD-SHAPE        TO COD2-CODE3                    
427600         PERFORM CALL-GIT-COD2                                    
                                                                        
427700         IF  STATUS-CODE = ZEROS                                  
427800             MOVE 'I'            TO COD2-FLAG1.                   
427900                                                                  
428000     IF  STATUS-CODE =  ZEROS                                     
428100     AND COD2-FLAG1  = 'I'                                        
428200         MOVE SPACE              TO COD-FLAG1                     
428300         MOVE WO-AD-QUANTITY     TO WK-TEMP-LINEAGE-I             
428400                                    WK-TEMP-LINEAGE-D.            
428500                                                                  
428600     MOVE ZEROS                  TO STATUS-CODE.                  
428700                                                                  
428900 GET-PUB-SIZE.                                                    
429000*------------*                                                    
429100                                                                  
429200     MOVE 'PBSZ'                 TO COD-KEY.                      
429300     MOVE WO-PUB                 TO COD-CODE1.                    
429400     PERFORM CALL-GIT-COD.                                        
429500                                                                  
429600     IF  STATUS-CODE NOT = ZEROS   
               move 1                  to return-code
429700         CALL 'AMSABRT' USING IN13.                               
429800                                                                  
430000 CALL-SETL-WO.                                                    
430100*------------*                                                    
430200                                                                  
430300     CALL 'SETL' USING WO-FILE, WO-REC, IO-PKT.                   
430400                                                                  
430600 CALL-SGET-WO.                                                    
430700*------------*                                                    
430800                                                                  
430900     CALL 'SGET' USING WO-FILE, WO-REC, IO-PKT.                   
431000                                                                  
431200 CALL-ESETL-WO.                                                   
431300*-------------*                                                   
431400                                                                  
431500     CALL 'ESETL' USING WO-FILE, WO-REC, IO-PKT.                  
431600                                                                  
431800 CALL-GIT-NAD.                                                    
431900*------------*                                                    
432000                                                                  
432100     CALL 'GIT' USING NAD-FILE, NAD-REC, IO-PKT.                  
432200                                                                  
432400 CALL-GIT-NA2.                                                    
432500*------------*                                                    
432600                                                                  
432700     CALL 'GIT' USING NAD-FILE, NA2-REC, IO-PKT.                  
432800                                                                  
433000 CALL-GIT-XRF.                                                    
433100*------------*                                                    
433200                                                                  
433300     CALL 'GIT' USING XRF-FILE, XRF-REC, IO-PKT.                  
433400                                                                  
433600 CALL-GIT-CNT.                                                    
433700*------------*                                                    
433800                                                                  
433900     CALL 'GIT' USING CNT-FILE, CNT-REC, IO-PKT.                  
434000                                                                  
434200 CALL-GIT-MSG.                                                    
434300*------------*                                                    
434400                                                                  
434500     CALL 'GIT' USING MSG-FILE, MSG-REC, IO-PKT.                  
434700                                                                  
434800 CALL-SETL-MSG.                                                   
434900*-------------*                                                   
435000                                                                  
435100     CALL 'SETL' USING MSG-FILE, MSG-REC, IO-PKT.                 
435200                                                                  
435400 CALL-SGET-MSG.                                                   
435500*-------------*                                                   
435600                                                                  
435700     CALL 'SGET' USING MSG-FILE, MSG-REC, IO-PKT.                 
435800                                                                  
436000 CALL-ESETL-MSG.                                                  
436100*--------------*                                                  
436200                                                                  
436300     CALL 'ESETL' USING MSG-FILE, MSG-REC, IO-PKT.                
436400                                                                  
436600 CALL-GIT-COD.                                                    
436700*------------*                                                    
436800                                                                  
436900     CALL 'GIT' USING COD-FILE, COD-REC, IO-PKT.                  
437000                                                                  
437200 CALL-GIT-COD2.                                                   
437300*-------------*                                                   
437400                                                                  
437500     CALL 'GIT' USING COD-FILE, COD2-REC, IO-PKT.                 
437600                                                                  
437800 CALL-GIT-COD3.                                                   
437900*-------------*                                                   
438000                                                                  
438100     CALL 'GIT' USING COD-FILE, COD3-REC, IO-PKT.                 
438200                                                                  
438400 CALL-SGET-COD.                                                   
438500*-------------*                                                   
438600                                                                  
438700     CALL 'SGET' USING COD-FILE, COD-REC, IO-PKT.                 
438800                                                                  
439000 CALL-SETL-COD.                                                   
439100*-------------*                                                   
439200                                                                  
439300     CALL 'SETL' USING COD-FILE, COD-REC, IO-PKT.                 
439400                                                                  
439500 PROCESS-AR-HEADER.                                               
439600*-----------------*                                               
                                                                        
439700     MOVE SPACE                  TO AR-NAD-REC.                   
439800     MOVE ZEROS                  TO WK-AR-REC-COUNT.              
439900     MOVE '1'                    TO AR-ENTITY.                    
440000     MOVE WK-GL-SUBLED           TO AR-MAJOR.                     
440100     MOVE WK-GL-LED              TO AR-MINOR.                     
440200     MOVE '01'                   TO AR-TRANS-CODE.                
                                                                        
440300     IF  WK-GL-SUBLED = '16'                                      
440400         MOVE SSL-NAD-TEL-NBR    TO AR-ACT-NO                     
440500     ELSE                                                         
440600         MOVE WK-AGY-JXRF-2      TO AR-ACT-NO.                    
440700                                                                  
440800     IF  NA2-NAME-2   = SPACE                                     
440900         MOVE '4'                TO AR-NO-RECS-X                  
441000     ELSE                                                         
441100         MOVE '5'                TO AR-NO-RECS-X.                 
441200                                                                  
441300     ADD  1                      TO WK-AR-REC-COUNT.              
441400     MOVE WK-AR-REC-COUNT        TO AR-REC-NO.                    
441500     MOVE NA2-NAME-1             TO AR-CUST-NAME.                 
441600                                                                  
441700     IF  NA2-XRF-FLAG = 'Y'                                       
441800         PERFORM GET-S-TYPE-XRF.                                  
441900                                                                  
442000     WRITE IAR-REC FROM AR-NAD-REC.                               
442100                                                                  
442200     MOVE SPACE                  TO AR-NAD-DATA.                  
442300     MOVE NA2-ZIP (1)            TO AR-ZIP-CODE.                  
442400     MOVE SSL-NAD-TEL-NBR        TO AR-TELEPHONE.                 
                                                                        
442500     IF  WK-GL-CODES = '110311'                                   
442600         MOVE '11'               TO AR-LOCK-BOX                   
442700     ELSE                                                         
442800     IF  WK-GL-CODES = '170317'                                   
442900         MOVE '17'               TO AR-LOCK-BOX                   
443000     ELSE                                                         
443100     IF  WK-GL-CODES = '370337'                                   
443200         MOVE '37'               TO AR-LOCK-BOX                   
443300     ELSE                                                         
443400     IF  WK-GL-CODES = '380338'                                   
443500         MOVE '38'               TO AR-LOCK-BOX                   
443600     ELSE                                                         
443700         MOVE '01'               TO AR-LOCK-BOX.                  
                                                                        
443800     ADD  1                      TO WK-AR-REC-COUNT.              
443900     MOVE WK-AR-REC-COUNT        TO AR-REC-NO.                    
444000     MOVE NA2-street-1           TO AR-ADDRESS.                   
444100     MOVE NA2-STATE  (1)         TO AR-STATE.                     
                                                                        
444200     WRITE IAR-REC FROM AR-NAD-REC.                               
444300                                                                  
444400     MOVE SPACE                  TO AR-NAD-DATA.                  
444500     ADD  1                      TO WK-AR-REC-COUNT.              
444600     MOVE WK-AR-REC-COUNT        TO AR-REC-NO.                    
444700*>   MOVE NAD-CITY-1             TO AR-ADDRESS2.                  
                                                                        
444800     PERFORM FORMAT-CITY-STATE.                                   
444900     MOVE WK-TERMS               TO AR-TERMS.                     
                                                                        
445000     IF  WK-GL-SUBLED = '16'                                      
445100         MOVE 'N'                TO AR-STATEMENT                  
445200     ELSE                                                         
445300         MOVE 'Y'                TO AR-STATEMENT.                 
                                                                        
445400     MOVE WK-REP-1               TO AR-CREDIT-MGR.                
445500     MOVE WK-REP-2               TO AR-CREDIT-REP.                
                                                                        
445600     WRITE IAR-REC FROM AR-NAD-REC.                               
                                                                        
445700     MOVE SPACE                  TO AR-NAD-DATA.                  
445800                                                                  
445900     IF  NA2-NAME-2   NOT = SPACE                                 
446000         ADD  1                  TO WK-AR-REC-COUNT               
446100         MOVE WK-AR-REC-COUNT    TO AR-REC-NO                     
446200         MOVE NA2-NAME-2         TO AR-CUST-NAME                  
                                                                        
446300         WRITE IAR-REC FROM AR-NAD-REC                            
                                                                        
446400         MOVE SPACE              TO AR-NAD-DATA.                  
446500                                                                  
446600     IF  WK-GL-CODES = '163000'                                   
446700         MOVE 'S'                TO AR-LANGUAGE                   
446800     ELSE                                                         
446900         MOVE 'E'                TO AR-LANGUAGE.                  
447000                                                                  
447100     MOVE 6                      TO AR-REC-NO.                    
                                                                        
447200     WRITE IAR-REC FROM AR-NAD-REC.                               
                                                                        
447300     MOVE SPACE                  TO AR-NAD-DATA.                  
447400                                                                  
447500 FORMAT-CITY-STATE.                                               
447600*-----------------*                                               
                                                                        
447700     MOVE NA2-CITY-1             TO UTL-COMP-UTL.                 
447800     MOVE SPACE                  TO WK-BLANK-FOUND.               
447900     SET UTL-UC                  TO 28.                           
448000                                                                  
448100     PERFORM FIND-LAST-NON-BLANK                                  
448200       UNTIL WK-BLANK-FOUND = 'Y'                                 
448300          OR UTL-UC         =  1.                                 
448400                                                                  
448500     SET TEMP-IX                 TO UTL-UC.                       
448700     SET UTL-UC AR-IX            TO 1.                            
448800                                                                  
448900     PERFORM MOVE-TO-AR                                           
449000       UNTIL UTL-UC > TEMP-IX.                                    
449200                                                                  
449300     MOVE ','                    TO AR-ADDRESS2-X (AR-IX).        
449400     SET AR-IX UP                BY 2.                            
449500                                                                  
449600     IF  AR-IX NOT > 30                                           
449700         MOVE NA2-STATE (1)      TO UTL-COMP-UTL                  
449800         SET UTL-UC              TO 1                             
                                                                        
449900         PERFORM MOVE-TO-AR                                       
450000           UNTIL AR-IX > 30                                       
450100              OR UTL-COMP-UTLC (UTL-UC) = SPACE.                  
450200                                                                  
450300 FIND-LAST-NON-BLANK.                                             
450400*-------------------*                                             
                                                                        
450500     IF  UTL-COMP-UTLC (UTL-UC) NOT = SPACE                       
450600         MOVE 'Y'                TO WK-BLANK-FOUND                
450700         SET UTL-UC UP           BY 1.                            
450800                                                                  
450900     SET UTL-UC DOWN             BY 1.                            
451000                                                                  
451100 MOVE-TO-AR.                                                      
451200*----------*                                                      
                                                                        
451300     MOVE UTL-COMP-UTLC (UTL-UC) TO AR-ADDRESS2-X (AR-IX).        
451400                                                                  
451500     SET UTL-UC AR-IX UP         BY 1.                            
451600                                                                  
451700 GET-S-TYPE-XRF.                                                  
451800*--------------*                                                  
451900                                                                  
452000     MOVE SPACE                  TO XRF-KEY.                      
452100     MOVE '0'                    TO XRF-AREA-CODE.                
452200     MOVE SSL-ACCT-NBR           TO XRF-ACCT-NBR.                 
452300                                                                  
452400     CALL 'SETL' USING NAD-XRF XRF-REC IO-PKT.                    
452500     CALL 'SGET' USING NAD-XRF XRF-REC IO-PKT.                    
                                                                        
452600     PERFORM GET-S-TYPE-XRF-2                                     
452700       UNTIL STATUS-CODE  NOT = ZEROS                             
452800          OR SSL-ACCT-NBR NOT = XRF-ACCT-NBR.                     
452900                                                                  
453000     CALL 'ESETL' USING NAD-XRF XRF-REC IO-PKT.                   
453100                                                                  
453200 GET-S-TYPE-XRF-2.                                                
453300*----------------*                                                
453400                                                                  
453500     IF  XRF-TYPE = 'S'                                           
453600         MOVE XRF-KEY-NAME       TO AR-SEARCH-NAME.               
453700                                                                  
453800     CALL 'SGET' USING NAD-XRF XRF-REC IO-PKT.                    
079400                                                                  
454000/                                                                 
454100 MATCH-WILDCARD.                 COPY AMZPWILD.                   
454200/                                                                 
454300 DAY-SUB.                        COPY AMZPDAY.                    
454400/                                                                 
454500 GET-CCH-REC.                    COPY AMZGCCH.                    
454600/                                                                 
454700 CALC-WO.                        COPY AMZPCALC.                   
454800/                                                                 
FXC--- GET-DATE.                       COPY AMZPGDAT.                   
      /                                                                 
454900 EDIT-DATE.                      COPY AMZPDATE.                   
455000/                                                                 
455100 SUB-ONE.                        COPY AMZLSUB1.                   
455200/                                                                 
455300 CHECK-STATUS.                   COPY AMZPCHK.                    
455500/                                                                 
455600 CHECK-SORT.                     COPY AMZPCSRT.                   
455700 GET-SH-BY-NBR.                  COPY AMZGSH.                     
       GET-xRF.                        COPY AMZGxRF.

      *************************** LAST SOURCE LINE *********************
AL-06                                                                   
AL-06  SORT-GCT-TABLE.                                                  
AL-06 *--------------                                                   
AL-06 *  sort table in group code sequence                              
AL-06 *                                                                 
AL-06      MOVE 'Y'                    TO SWAP-DONE.                    
AL-06      MOVE GCT-MAX-ENTRY          TO WS-IDX.                       
AL-06      PERFORM BUBBLE-SORT                                          
AL-06          VARYING GCT-X FROM 2 BY 1                                
AL-06              UNTIL   GCT-X  >  GCT-MAX-ENTRY                      
AL-06              OR      SWAP-DONE  =  'N'.                           
AL-06      SET GCT-X                   TO 1.                            
AL-06      PERFORM DISPLAY-GCT UNTIL GCT-X > GCT-MAX-ENTRY.             
AL-06                                                                   
AL-06  DISPLAY-GCT.                                                     
AL-06 *------------                                                     
AL-06                                                                   
AL-06      DISPLAY 'Gct sort: '                                         
AL-06           GCT-JOB-NBR(GCT-X) ' '                                  
AL-06           GCT-WO-GROUP-CODE(GCT-X).                               
AL-06      SET GCT-X UP BY 1.                                           
AL-06                                                                   
AL-06  BUBBLE-SORT.                                                     
AL-06 *------------                                                     
AL-06      MOVE 'N'                    TO SWAP-DONE.                    
AL-06      PERFORM SORT-SWAP                                            
AL-06          VARYING GCT-X2 FROM 2 BY 1                               
AL-06              UNTIL   GCT-X2  >  WS-IDX.                           
AL-06                                                                   
AL-06      SUBTRACT 1 FROM WS-IDX.                                      
AL-06                                                                   
AL-06  SORT-SWAP.                                                       
AL-06 *----------                                                       
AL-06      IF  GCT-WO-GROUP-CODE (GCT-X2) <                             
AL-06          GCT-WO-GROUP-CODE (GCT-X2 - 1)                           
AL-06          MOVE 'Y'                     TO SWAP-DONE                
AL-06          MOVE GCT-ENTRY (GCT-X2)      TO TEMP-GCT-ENTRY           
AL-06          MOVE GCT-ENTRY (GCT-X2 - 1)  TO GCT-ENTRY (GCT-X2)       
AL-06          MOVE TEMP-GCT-ENTRY          TO GCT-ENTRY (GCT-X2 - 1).  
AL-06                                                                   
AL-06                                                                   
AL-06  PROCESS-GROUP-BUY.                                               
AL-06 *---------------*           
            display 'PROCESS-GROUP-BUY'
AL-06      MOVE SPACE                  TO WK-MAG-TAPE-REC.              
AL-06      ADD 1                       TO TTL-GB-NBR.                   
AL-06                                                                   
           perform PROCESS-DETAILS

AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A  
AL-20      MOVE '07'                   TO NT-TAB2-P10                
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
*******    ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
           ADD  1                      TO WK-LINE-CNT                   
           MOVE WK-LINE-CNT            TO NT-LVL-L                      
           MOVE WK-LINE-CNT            TO NT-LVL-I         
           
           if  WK-GOLD-IND = 'Y'
               move 'SECT'                 TO COD-KEY
               MOVE WO-PUB                 to cod-code1
               MOVE WO-SECTION             to cod-code2
***********                                   NT-EDTN-LIT
               call 'GIT'                  using cod-file cod-rec io-pkt
           else
               move 'POS'                  TO COD-KEY
	       MOVE WO-PUB                 to cod-code1
	       MOVE WO-ad-posn             to cod-code2
****************                       NT-EDTN-LIT
               call 'GIT'                  using cod-file cod-rec io-pkt
               if  status-code not = 0
                   move 'EDTN'            TO COD-KEY
	           MOVE WO-PUB            to cod-code1
	           MOVE WO-edition        to cod-code2
****************                      NT-EDTN-LIT
                   call 'GIT'             using cod-file cod-rec io-pkt
               end-if
           end-if
           
           move cod-name               to NT-PUB-DTL-LIT 
           
********   move 0                      to NT-GROSS-AMT
           move space                  to NT-DATA-10
           
*********  for when they are same only print once           
           if  wo-edition not = wo-ad-posn
               WRITE NEWT-REC              FROM 
                   Function Upper-case(NEW-TECH-REC-2)   
               display NEW-TECH-REC-2
           end-if
           display 'checking web details '
                   'WK-GOLD-IND ' WK-GOLD-IND ' ' 
                   'WK-PLAT-IND ' wK-PLAT-IND ' '
                   'edtn ' edtn-table-count
                   
           if  wk-plat-ind = 'Y'        
           or  wk-gold-ind = 'Y'        
               Perform Write-Upsell-Line
                   varying wk-edtn-x from 1 by 1
                   until wk-edtn-x > edtn-table-count
           end-if

AL-06      MOVE space                 TO DTL-NET-AMT-x                  
                                         DTL-GROSS-AMT-x                
               
*********  MOVE wo-invc-amt           TO DTL-NET-AMT-P.                 
*********  MOVE wo-gross-amt          TO DTL-GROSS-AMT-P.               
           move space                  to NT-AMT-HDR-A
AL-06                                                                   
AL-06      PERFORM WRITE-DETAIL-LINE.                                   
AL-06      MOVE 'Y' TO DETAIL-DATA.                                     
AL-08               
       Write-Upsell-Line.
            display 'Write-Upsell-Line'
           display 'section ' EDTN-section (wk-edtn-x) ' ' 
                   'ad posn ' edtn-ad-posn (wk-edtn-x)                    
 
AL-20      move 0                      to NT-GROSS-AMT
           move space                  to NT-AMT-HDR-A

           move 'EDTN'                 TO COD-KEY
           MOVE EDTN-PUB  (wk-edtn-x)  to cod-code1
           MOVE EDTN-edtn (wk-edtn-x)  to cod-code2
           call 'GIT'                  using cod-file cod-rec io-pkt
           move cod-name               to NT-PUB-DTL-LIT 
           display 'write 8 ' NEW-TECH-REC-2 
           WRITE NEWT-REC              FROM 
                   Function Upper-case(NEW-TECH-REC-2).   

       Write-Mega-Line.
            display 'Write-Mega-Line'
           display 'edition ' mega-edtn (mega-x) 

AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A  
AL-20      MOVE '07'                   TO NT-TAB2-P10                
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
*******    ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
           ADD  1                      TO WK-LINE-CNT                   
           MOVE WK-LINE-CNT            TO NT-LVL-L                      
           MOVE WK-LINE-CNT            TO NT-LVL-I         

AL-20      move 0                      to NT-GROSS-AMT
                                          NT-NET-DTL-AMT
                                          NT-DISP-NET-AMT
           move space                  to NT-AMT-HDR-A

           move 'EDTN'                 TO COD-KEY
           MOVE mega-PUB  (mega-x)     to cod-code1
           MOVE mega-edtn (mega-x)     to cod-code2
           call 'GIT'                  using cod-file cod-rec io-pkt
           display cod-key ' ' status-code ' ' cod-name
           move cod-name               to NT-PUB-DTL-LIT 
           display 'write m ' NEW-TECH-REC-2
           WRITE NEWT-REC              FROM 
                  Function Upper-case(NEW-TECH-REC-2)    
       
       Add-Megaplex-Table.
AL-08                            
           add 1                       to mega-TABLE-COUNT
AL-08      IF  mega-TABLE-COUNT > mega-TABLE-MAX                        
               move 1                  to return-code
AL-08          DISPLAY SPACE                                            
AL-08          DISPLAY '========================================='      
AL-08          DISPLAY '======= mega Total Table Overflow ======='      
AL-08          DISPLAY '========================================='      
AL-08          CALL 'AMSABRT' USING IN39                                
AL-08      ELSE           
AL-08          MOVE WO-edition   TO mega-edtn    (mega-TABLE-COUNT)                    
AL-08          MOVE WO-pub       TO mega-pub     (mega-TABLE-COUNT)                    

AL-08          MOVE WO-JOB-NBR   TO mega-NBR(mega-TABLE-COUNT)                    
AL-08          MOVE WO-GROSS-AMT TO mega-GROSS-AMT(mega-TABLE-COUNT)              
AL-08          MOVE WO-BLEED-AMT TO mega-BLEED-AMT(mega-TABLE-COUNT)              
AL-08          MOVE WO-INVC-AMT  TO mega-INVC-AMT (mega-TABLE-COUNT)          
AL-08         ADD WO-adj-AMT  TO mega-adj-AMT (mega-TABLE-COUNT)    
                                WK-mega-adj-AMT
AL-08      DISPLAY 'Add-Megaplex-Table NEW: ' WO-JOB-NBR ' '
                     mega-TABLE-COUNT ' ' 
                         wo-edition ' ' 
                     wo-GROSS-AMT ' ' 
                     wo-invc-amt ' ' 
                     mega-GROSS-AMT(mega-TABLE-COUNT) ' ' 
                     mega-invc-AMT(mega-TABLE-COUNT) ' ' 
                      .                         

AL-08  ADD-GOLD-TABLE.                                                  
AL-08 *--------------*   
             display 'GOLD: ' WO-JOB-NBR ' ' wo-edition
AL-08      SET GOLD-X                   TO 1.                           
AL-08      MOVE WO-JOB-NBR              TO WK-JOB-GOLD.                 
AL-08      SEARCH GOLD-LINE                                             
AL-08        WHEN GOLD-LINE  (GOLD-X) = LOW-VALUES                      
AL-08             PERFORM ADD-GOLD-NEW                                  
AL-08        WHEN WK-JOB-GOLD-7 = GOLD-NBR-7 (GOLD-X)   
                   display 'adding $'
AL-08         ADD WO-GROSS-AMT TO GOLD-GROSS-AMT(GOLD-X)                
AL-08         ADD WO-BLEED-AMT TO GOLD-BLEED-AMT(GOLD-X)                
AL-08         ADD WO-INVC-AMT  TO GOLD-INVC-AMT (GOLD-X)
AL-08         ADD WO-adj-AMT  TO GOLD-adj-AMT (GOLD-X)           
                              WK-GOLD-adj-AMT
AL-08                                                                   
AL-08       DISPLAY 'WO-EDITION: ' WO-EDITION                           
AL-08               ' ' WO-JOB-NBR                                      
AL-08               ' ' WK-GOLD-adj-AMT                                 
AL-08  ADD-GOLD-NEW.                                                    
AL-08 *------------* 
            display 'ADD-GOLD-NEW'
AL-08      ADD 1                       TO GOLD-TABLE-COUNT.             
AL-08                                                                   
AL-08      IF  GOLD-TABLE-COUNT > GOLD-TABLE-MAX                        
               move 1                  to return-code
AL-08          DISPLAY SPACE                                            
AL-08          DISPLAY '========================================='      
AL-08          DISPLAY '======= GOLD Total Table Overflow ======='      
AL-08          DISPLAY '========================================='      
AL-08          CALL 'AMSABRT' USING IN39                                
AL-08      ELSE           
                   display 'adding new'
AL-08          MOVE WO-JOB-NBR   TO GOLD-NBR(GOLD-X)                    
AL-08          MOVE WO-GROSS-AMT TO GOLD-GROSS-AMT(GOLD-X)              
AL-08          MOVE WO-BLEED-AMT TO GOLD-BLEED-AMT(GOLD-X)              
AL-08          MOVE WO-INVC-AMT  TO GOLD-INVC-AMT (GOLD-X)          
AL-08         ADD WO-adj-AMT  TO GOLD-adj-AMT (GOLD-X)    
                                WK-GOLD-adj-AMT
AL-08      DISPLAY 'ADD GOLD NEW: ' WO-JOB-NBR ' '
                     GOLD-TABLE-COUNT.                         

AL-15                                                                   
AL-15  ADD-PLAT-TABLE.                                                  
AL-15 *--------------*                                                  
           DISPLAY 'PLAT: ' WO-JOB-NBR.                                 
AL-15      SET PLAT-X                   TO 1.                           
AL-15      MOVE WO-JOB-NBR              TO WK-JOB-PLAT.                 
AL-15      SEARCH PLAT-LINE                                             
AL-15        WHEN PLAT-LINE  (PLAT-X) = LOW-VALUES                      
AL-15             PERFORM ADD-PLAT-NEW                                  
AL-15        WHEN WK-JOB-PLAT-7 = PLAT-NBR-7 (PLAT-X)                   
AL-15         ADD WO-GROSS-AMT TO PLAT-GROSS-AMT(PLAT-X)                
AL-15         ADD WO-BLEED-AMT TO PLAT-BLEED-AMT(PLAT-X)                
AL-15         ADD WO-INVC-AMT  TO PLAT-INVC-AMT (PLAT-X)
AL-15         ADD WO-adj-AMT  TO PLAT-adj-AMT (PLAT-X)           
                                 WK-plat-adj-AMT
AL-15                                                                   
AL-15       DISPLAY 'WO-EDITION: ' WO-EDITION                           
AL-15               ' ' WO-JOB-NBR                                      
AL-15               ' ' WK-plat-adj-AMT                                 
AL-15  ADD-PLAT-NEW.                                                    
AL-15 *------------*                                                    
AL-15      DISPLAY 'ADD PLAT NEW: ' WO-JOB-NBR.                         
AL-15      ADD 1                       TO PLAT-TABLE-COUNT.             
AL-15                                                                   
AL-15      IF  PLAT-TABLE-COUNT > PLAT-TABLE-MAX                        
               move 1                  to return-code
AL-15          DISPLAY SPACE                                            
AL-15          DISPLAY '========================================='      
AL-15          DISPLAY '======= PLAT Total Table Overflow ======='      
AL-15          DISPLAY '========================================='      
AL-15          CALL 'AMSABRT' USING IN40                                
AL-15      ELSE                                                         
AL-15          MOVE WO-JOB-NBR   TO PLAT-NBR(PLAT-X)                    
AL-15          MOVE WO-GROSS-AMT TO PLAT-GROSS-AMT(PLAT-X)              
AL-15          MOVE WO-BLEED-AMT TO PLAT-BLEED-AMT(PLAT-X)              
AL-15          MOVE WO-INVC-AMT  TO PLAT-INVC-AMT (PLAT-X)            
AL-15         ADD WO-adj-AMT  TO PLAT-adj-AMT (PLAT-X)            
                                 WK-plat-adj-AMT
AL-15                                                                   
AL-15  ADD-PACKAGE-TABLE.                                               
AL-15 *-----------------* 
           
AL-15      DISPLAY 'ADD-PACKAGE-TABLE '  WO-JOB-NBR                                 
AL-15      IF  upsell-section                                       
AL-15         PERFORM ADD-GOLD-TABLE   
           else
AL-15         PERFORM ADD-PLAT-TABLE                                    
           end-if

AL-08      IF  edtn-TABLE-COUNT > edtn-TABLE-MAX                        
               move 1                  to return-code
AL-08          DISPLAY SPACE                                            
AL-08          DISPLAY '========================================='      
AL-08          DISPLAY '======= edtn Total Table Overflow ======='      
AL-08          DISPLAY '========================================='      
AL-08          CALL 'AMSABRT' USING IN39                                
AL-08      ELSE           
              set edtn-X                 to 01
              search edtn-LINE 
              when edtn-edtn (edtn-x) = wo-edition
                  next sentence
              when edtn-edtn (edtn-x) = space            
                  add 1                        to EDTN-TABLE-COUNT
                  display 'eDTN-TABLE-COUNT ' eDTN-TABLE-COUNT

                  move wo-job-nbr              to edtn-job-nbr 
                                                 (EDTN-TABLE-COUNT)
                  move wo-edition              to edtn-edtn 
                                                 (EDTN-TABLE-COUNT)
                  move wo-pub                  to edtn-pub          
                                                 (EDTN-TABLE-COUNT)
                  move wo-section              to edtn-section      
                                                 (EDTN-TABLE-COUNT)
                  move wo-ad-posn              to edtn-ad-posn      
                                                 (EDTN-TABLE-COUNT)
               end-search
           end-if
                                         
                                         
CTS-02*CTS - 02/23/07 CHANGE BEGINS                                     
CTS-02 TEST-EULU-X.                                                     
CTS-02*------------*                                                    
CTS-02     IF EULU-X > EURO-R-MAX-TBL                                   
CTS-02        DISPLAY 'EURO-REACH LOOKUP TABLE NEEDS EXPANDING'         
CTS-02        CALL 'AMSABRT' USING IN43.                                
CTS-02*                                                                 
CTS-02 TEST-EU-REACH.                                                   
CTS-02*--------------*                                                  
CTS-02     IF EU-R > EURO-AMT-MAX-TBL                                   
CTS-02        DISPLAY 'EURO-REACH AMOUNTS TABLE NEEDS EXPANDING'        
CTS-02        CALL 'AMSABRT' USING IN44.                                
CTS-02*                                                                 
CTS-02 LOAD-THE-EUROR-TABLE.                                            
CTS-02*---------------------*                                           
CTS-02     OPEN INPUT EUROR-INP                                         
CTS-02     MOVE 'N'                    TO EOF-EUROR-FLAG                
CTS-02     READ EUROR-INP                                               
CTS-02          AT END MOVE 'Y'        TO EOF-EUROR-FLAG.               
CTS-02     SET EULU-X                  TO 1                             
CTS-02     PERFORM LOAD-EUROR-ITEMS                                     
CTS-02          UNTIL   ( EULU-X > 50                                   
CTS-02          OR      EOF-EUROR-FLAG = 'Y')                           
CTS-02                                                                  
CTS-02     CLOSE EUROR-INP.                                             
CTS-02*                                                                 
CTS-02 LOAD-EUROR-ITEMS.                                                
CTS-02*-----------------*                                               
CTS-02     MOVE EUROR-REC              TO EU-PUB-EDN-LU(EULU-X)         
CTS-02     SET  EULU-X                 UP BY 1                          
CTS-02     ADD  1                      TO EURO-X-MAX                    
CTS-02     PERFORM TEST-EULU-X                                          
CTS-02     READ EUROR-INP                                               
CTS-02          AT END MOVE 'Y'        TO EOF-EUROR-FLAG.               
CTS-02*                                                                 
CTS-02 SEARCH-EURO-EDITION.                                             
CTS-02*--------------------*                                            
CTS-02     MOVE SPACES                     TO WK-MAIN-EDN.              
CTS-02     SET EULU-X                      TO 1.                        
CTS-02     SEARCH EU-PUB-EDN-LU VARYING EULU-X                          
CTS-02     AT END                                                       
CTS-02             MOVE 'G'                TO WK-EU-PG-FLG              
CTS-02     WHEN                                                         
CTS-02         EU-PUB-CD(EULU-X) = WO-PUB                               
CTS-02             MOVE EU-EDN-DTL(EULU-X) TO WK-EU-PG-FLG              
117900     END-SEARCH.                                                  
CTS-02*                                                                 
CTS-02 PROCESS-AGY-ADV-TOTAL-NEW-TECH.                                  
CTS-02*-------------------------------*    
              display 'PROCESS-AGY-ADV-TOTAL-NEW-TECH.'
              display 'TTL-AGY-ADV-GROSS-AMT-ADJ '
                       TTL-AGY-ADV-GROSS-AMT-ADJ
              display 'TTL-AGY-ADV-GROSS-AMT '
                       TTL-AGY-ADV-GROSS-AMT    
              display 'TTL-AGY-ADV-GROSS-AMT-T '
                       TTL-AGY-ADV-GROSS-AMT-T 
              display 'TTL-AGY-ADV-NET-AMT-ADJ '   
                       TTL-AGY-ADV-NET-AMT-ADJ ' ' 
              display 'TTL-AGY-ADV-NET-AMT '  
                       TTL-AGY-ADV-NET-AMT  
              display 'TTL-AGY-ADV-GROSS-AMT-ADJ-T '
                       TTL-AGY-ADV-GROSS-AMT-ADJ-T 
              display 'TTL-AGY-ADV-NET-AMT-ADJ-NT-T '
                       TTL-AGY-ADV-NET-AMT-ADJ-NT-T   ' '
                       'WK-AGY-ADV-GROSS-TOT '
                       WK-AGY-ADV-GROSS-TOT 
                display 
                       TTL-AGY-ADV-CC-AMT   ' '
                      TTL-AGY-ADV-COM-AMT     ' '
                     TTL-AGY-ADV-TAX-AMT  ' ' 
                      TTL-AGY-ADV-D-P-AMT   ' '  
                      TTL-AGY-ADV-SPL-CHG   ' '  
                    TTL-AGY-ADV-ADJ-AMt ' '
                    TTL-AGY-ADV-CC-AMT ' ' 
AL-20      ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A                    
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
AL-20      ADD  1                      TO WK-LINE-CNT                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L        
********      display 'write 9 ' NEW-TECH-REC-2 

********   WRITE NEWT-REC              FROM 
********       Function Upper-case(NEW-TECH-REC-2)              
AL-01A     ADD 1     TO WK-NT-REC-CNT              
CTS-02     INITIALIZE NEW-TECH-REC-2                                 
CTS-02     MOVE '2'                    TO NT-CODE-2A                 
CTS-02     MOVE WK-EU-PG-FLG           TO NT-CODE-2B                 
CTS-02     MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                 
           MOVE '02'                   TO NT-TAB2-P13                
           ADD  1                      TO WK-LINE-CNT                
           MOVE WK-LINE-CNT            TO NT-LVL-L                   
           MOVE 'PUB'                  TO COD-KEY                    
           MOVE WK-PUB                 TO COD-CODE1                  
           CALL 'GIT' USING COD-FILE COD-REC IO-PKT                  
           STRING 'TOTAL '                  DELIMITED BY SIZE        
                  COD-NAME                  DELIMITED BY SIZE        
                  ': '                      DELIMITED BY SIZE        
           INTO  NT-PUB-DTL-LIT                                      
********   COMPUTE NT-PUB-DTL-AMT      = TTL-AGY-ADV-GROSS-AMT-ADJ-T 
***********                              + TTL-AGY-ADV-GROSS-AMT-T   
AL-20      MOVE '07'                   TO NT-TAB2-P10                
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-I                   
*********  COMPUTE NT-GROSS-AMT        = TTL-AGY-ADV-GROSS-AMT-ADJ-T 
AL-2***    COMPUTE NT-GROSS-AMT        = TTL-AGY-ADV-GROSS-AMT-ADJ 
AL-2****                                 + TTL-AGY-ADV-GROSS-AMT   
AL-20      MOVE '08'                   TO NT-TAB2-P16                
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-O                   
AL-20 *???????????                                                   
AL-****    COMPUTE NT-NET-DTL-AMT   = TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20 **                           + TTL-AGY-ADV-NET-AMT-NT-T        
                    
           move TTL-pub-invc-AMT      to NT-NET-DTL-AMT  
           display 'TTL-pub-net-amt '  TTL-pub-invc-AMT   
           
           display 
                    'NT-NET-DTL-AMT ' NT-NET-DTL-AMT 


*********  display 'write 10 ' NEW-TECH-REC-2 
                    
***********WRITE NEWT-REC           FROM 
***********     Function Upper-case (NEW-TECH-REC-2)           
********** ADD 1     TO WK-NT-REC-CNT              
AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A                    
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
AL-20      ADD  1                      TO WK-LINE-CNT                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L  
*********     display 'write 11 ' NEW-TECH-REC-2 

********   WRITE NEWT-REC              FROM 
********         Function Upper-case(NEW-TECH-REC-2)              
AL-01A     ADD 1     TO WK-NT-REC-CNT              
AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A                    
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
********   ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
AL-20      ADD  1                      TO WK-LINE-CNT                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L                      
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-I                      
AL-20      MOVE 'TOTAL ALL PRODUCTS:'  TO NT-PUB-DTL-LIT.               
AL-20      MOVE '07'                   TO NT-TAB2-P10                   
*********  COMPUTE NT-GROSS-AMT        = WK-AGY-ADV-GROSS-TOT           
********                               + TTL-AGY-ADV-GROSS-AMT-ADJ-T    
*********                              + TTL-AGY-ADV-GROSS-AMT-T        
AL-20         COMPUTE NT-GROSS-AMT        = TTL-AGY-ADV-GROSS-AMT-ADJ 
AL-20                                       + TTL-AGY-ADV-GROSS-AMT   


AL-20 *    COMPUTE NT-GROSS-AMT        = TTL-AGY-ADV-GROSS-AMT-ADJ-T    
AL-20 *                                + TTL-AGY-ADV-GROSS-AMT-T        
AL-20      MOVE '08'                   TO NT-TAB2-P16                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-O                      
AL-20      COMPUTE NT-NET-DTL-AMT   = WK-AGY-ADV-NET-NOTAX              
AL-20                                  + TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20                                  + TTL-AGY-ADV-NET-AMT-NT-T       
AL-20 *    COMPUTE NT-NET-DTL-AMT      = TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20 *                                + TTL-AGY-ADV-NET-AMT-NT-T  
************  display 'write 12 ' NEW-TECH-REC-2 

********** WRITE NEWT-REC              FROM 
**********      Function Upper-case(NEW-TECH-REC-2)              
********** ADD 1     TO WK-NT-REC-CNT              
AL-20                                                                   
AL-20      IF  TTL-AGY-ADV-CC-AMT > 0                                   
AL-20        INITIALIZE NEW-TECH-REC-2                                  
AL-20        MOVE '2'                    TO NT-CODE-2A                  
AL-20        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                  
AL-20 **     ADD  1                      TO WK-JOB-SEQ-CNT              
AL-20        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                  
AL-20        MOVE '02'                   TO NT-TAB2-P13                 
AL-20        ADD  1                      TO WK-LINE-CNT                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-L              
*******       display 'write 13 ' NEW-TECH-REC-2 

*********    WRITE NEWT-REC              FROM 
********        Function Upper-case(NEW-TECH-REC-2)            
*********    ADD 1     TO WK-NT-REC-CNT              
AL-20        INITIALIZE NEW-TECH-REC-2                                  
AL-20        MOVE '2'                    TO NT-CODE-2A                  
AL-20        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                  
AL-20        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                  
AL-20        MOVE '02'                   TO NT-TAB2-P13                 
AL-20        ADD  1                      TO WK-LINE-CNT                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-L                    
AL-20        MOVE 'PREPAID AMOUNT:    '  TO NT-PUB-DTL-LIT              
AL-20        MOVE '08'                   TO NT-TAB2-P16                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-O        

00020        COMPUTE NT-NET-DTL-AMT      = zeros 
                                         - TTL-AGY-ADV-CC-AMT 
             display 'prepaid ' NT-NET-DTL-AMT 
AL-01A       ADD 1     TO WK-NT-REC-CNT              
              display 'write 14 ' NEW-TECH-REC-2 

AL-20        WRITE NEWT-REC              FROM 
                 Function Upper-case(NEW-TECH-REC-2)
             end-if.           
AL-20                                                                   
AL-20                                                                   
AL-20      IF TTL-AGY-ADV-TAX-AMT NOT = 0                               
AL-20        INITIALIZE NEW-TECH-REC-2                                  
AL-20        MOVE '2'                    TO NT-CODE-2A                  
AL-20        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                  
AL-20 **     ADD  1                      TO WK-JOB-SEQ-CNT              
AL-20        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                  
AL-20        MOVE '02'                   TO NT-TAB2-P13                 
AL-20        ADD  1                      TO WK-LINE-CNT                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-L       
              display 'write 16 ' NEW-TECH-REC-2 

AL-20        WRITE NEWT-REC              FROM 
                  Function Upper-case(NEW-TECH-REC-2)
AL-01A       ADD 1     TO WK-NT-REC-CNT              
AL-20        INITIALIZE NEW-TECH-REC-2                                  
AL-20        MOVE '2'                    TO NT-CODE-2A                  
AL-20        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                  
AL-20        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                  
AL-20        MOVE '02'                   TO NT-TAB2-P13                 
AL-20        ADD  1                      TO WK-LINE-CNT                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-L                    
AL-20        MOVE 'TOTAL TAX AMOUNT:  '  TO NT-PUB-DTL-LIT              
AL-20        MOVE '08'                   TO NT-TAB2-P16                 
AL-20        MOVE WK-LINE-CNT            TO NT-LVL-O                    
AL-20        COMPUTE NT-NET-DTL-AMT      = TTL-AGY-ADV-TAX-AMT          
AL-01A       ADD 1     TO WK-NT-REC-CNT    
              display 'write 17 ' NEW-TECH-REC-2 

AL-20        WRITE NEWT-REC              FROM 
                  Function Upper-case(NEW-TECH-REC-2)
             end-if.           
AL-20                                                                   
AL-20                                                                   
AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A                    
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
AL-20      ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
AL-20      ADD  1                      TO WK-LINE-CNT                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L                      
********   WRITE NEWT-REC              FROM 
*********      Function Upper-case(NEW-TECH-REC-2)    
********      display 'write 18 ' NEW-TECH-REC-2

           ADD 1     TO WK-NT-REC-CNT              
AL-20      INITIALIZE NEW-TECH-REC-2.                                   
AL-20      MOVE '2'                    TO NT-CODE-2A                    
AL-20      MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
AL-20 **   ADD  1                      TO WK-JOB-SEQ-CNT                
AL-20      MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20      MOVE '02'                   TO NT-TAB2-P13                   
AL-20      ADD  1                      TO WK-LINE-CNT                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-L                      
AL-20      MOVE 'INVOICE AMOUNT:    '  TO NT-PUB-DTL-LIT.               
AL-20      MOVE '08'                   TO NT-TAB2-P16                   
AL-20      MOVE WK-LINE-CNT            TO NT-LVL-O                      
AL-20      COMPUTE NT-NET-DTL-AMT   = WK-AGY-ADV-NET-NOTAX              
AL-20                               + TTL-AGY-ADV-NET-AMT-ADJ-NT-T      
AL-20                               + TTL-AGY-ADV-NET-AMT-NT-T          
AL-20                               + TTL-AGY-ADV-TAX-AMT               
AL-20                               - TTL-AGY-ADV-CC-AMT                

              display 'write 19 ' NEW-TECH-REC-2 
           WRITE NEWT-REC              FROM 
                Function Upper-case(NEW-TECH-REC-2)              
           ADD 1     TO WK-NT-REC-CNT              
AL-20                                                                   
CTS-02     INITIALIZE NEW-TECH-REC-3                                    
CTS-02     MOVE ZEROS                  TO WK-JOB-SEQ-CNT.               
CTS-02*                                                                 
CTS-02     MOVE '3'                    TO NT-CODE-3A.                   
*********  MOVE WK-CURR-VAL1           TO NT-TOT-CURR-VAL.              
CTS-02     MOVE 'USD'                  TO NT-TOT-CURR-VAL.              

CTS-02     MOVE SPACES                 TO WK-CURR-VAL1.                 
CTS-02*                                                                 
CTS-02     MOVE WK-HOLD-ADV-NBR        TO NT-ADV-NBR-2.                 
CTS-02     MOVE WK-HOLD-AGY-NBR        TO NT-AGY-NBR-2.                 
CTS-02     MOVE TTL-AGY-ADV-CC-AMT     TO NT-PPD-AMT.                   
           MOVE TTL-AGY-ADV-COM-AMT    TO NT-COMM-AMT.                  

CTS-02     MOVE TTL-AGY-ADV-TAX-AMT    TO NT-TAX-AMT.                   
CTS-02     MOVE TTL-AGY-ADV-D-P-AMT    TO NT-TOT-D-P-AMT.               
CTS-02     MOVE TTL-AGY-ADV-SPL-CHG    TO NT-TOT-SPL-AMT.               
CTS-02     MOVE TTL-AGY-ADV-ADJ-AMT    TO NT-TOT-ADJ-AMT.               
CTS-02*    
CTS-02     ADD TTL-AGY-ADV-GROSS-AMT-ADJ, TTL-AGY-ADV-GROSS-AMT         
CTS-02                                 GIVING NT-TOT-GROSS-AMT.         
CTS-02                                                                  
CTS-02     ADD TTL-AGY-ADV-NET-AMT-ADJ, TTL-AGY-ADV-NET-AMT             
CTS-02                                 GIVING NT-TOT-NET-AMT.           
CTS-02*                                             
AL-20      COMPUTE NT-TOT-GROSS-AMT    = TTL-AGY-ADV-GROSS-AMT-ADJ 
AL-20                                  + TTL-AGY-ADV-GROSS-AMT   
AL-20      COMPUTE NT-TOT-NET-AMT      = TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
AL-20                                  + TTL-AGY-ADV-NET-AMT-NT-T 
                                       - TTL-AGY-ADV-CC-AMT
CTS-02     MOVE zero                   TO NT-COMM-AMT
                                          NT-PPD-AMT.                  

*********  move TTL-AGY-ADV-NET-AMT to NT-TOT-NET-AMT

           move TTL-AGY-ADV-GROSS-AMT to NT-TOT-GROSS-AMT 
           compute  NT-TOT-NET-AMT = TTL-AGY-ADV-NET-AMT 
                                   - TTL-AGY-ADV-CC-AMT

CTS-02     WRITE NEWT-REC FROM 
               Function Upper-case(NEW-TECH-REC-3).   

            display 'write 0 ' NEW-TECH-REC-3
AL-01A     ADD 1     TO WK-NT-REC-CNT.              
CTS-02*                             
              MOVE ZEROES             TO TTL-AGY-ADV-NET-AMT-ADJ-NT-T   
                                         TTL-AGY-ADV-NET-AMT-NT-T       
                                         WK-AGY-ADV-GROSS-TOT           
                                         WK-AGY-ADV-NET-NOTAX           
              MOVE ZEROES             TO TTL-AGY-ADV-GROSS-AMT-ADJ-T    
                                         TTL-AGY-ADV-GROSS-AMT-T.       

CTS-02 PROCESS-EURO-DETAILS.                                            
CTS-02*---------------------*                                           
CTS-02     IF EURO-FIRST-TIME = 'Y'                                     
CTS-02        MOVE SPACES                 TO EURO-FIRST-TIME            
CTS-02        INITIALIZE NEW-TECH-REC-2                                 
CTS-02        PERFORM PROCESS-MONTH-YEAR                                
CTS-02        MOVE '2'                    TO NT-CODE-2A                 
CTS-02        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                 
CTS-02        MOVE '01'                   TO NT-TAB2-P1                 
CTS-02        MOVE '02'                   TO NT-TAB2-P4                 
CTS-02        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                 
CTS-02        MOVE ZEROS                  TO WK-LINE-CNT                
CTS-02        ADD  1                      TO WK-LINE-CNT                
CTS-02        MOVE WK-LINE-CNT            TO NT-LVL-A                   
CTS-02                                       NT-LVL-D                   
  
              MOVE WO-INVC-NBR        TO WS-INV-NBR-NEW
           if  WO-CRED-MEMO not = 0
               move WO-CRED-MEMO       to WS-INV-NBR-NEW
           end-if
AL-01         MOVE WS-INV-NBR-NEW(7:12) TO NT-INV-NBR

              STRING WK-EU-MONTH    DELIMITED BY SPACE                  
                     SPACE          DELIMITED BY SIZE                   
                     WK-EU-YEAR     DELIMITED BY SPACE                  
              INTO   NT-EURO-DESC-R                                     
CTS-02        WRITE NEWT-REC              FROM 
                 Function Upper-case(NEW-TECH-REC-2)      
              display 'write 20 ' NEW-TECH-REC-2 

AL-01A        ADD 1     TO WK-NT-REC-CNT              
CTS-02*                                                                 
CTS-02        IF WK-EU-PG-FLG = 'E'                                     
CTS-02           INITIALIZE NEW-TECH-REC-2                              
CTS-02           MOVE '2'                 TO NT-CODE-2A                 
CTS-02           MOVE '02'                TO NT-TAB2-P4                 
CTS-02           MOVE WK-EU-PG-FLG        TO NT-CODE-2B                 
CTS-02           MOVE WK-JOB-SEQ-CNT      TO NT-JOB-SEQ                 
CTS-02           ADD  1                   TO WK-LINE-CNT                
CTS-02           MOVE WK-LINE-CNT         TO NT-LVL-D                   
dw-29 *          STRING 'EURO REACH '  DELIMITED BY SIZE                
dw-29            move 'PUB'               TO COD3-KEY                   
dw-29            move WO-PUB              TO COD3-CODE1                 
dw-29            CALL 'GIT'            USING COD-FILE COD3-REC IO-PKT   
dw-29            STRING COD3-NAME      DELIMITED BY SIZE                
                        WK-EU-YEAR     DELIMITED BY SPACE               
                 INTO   NT-EURO-DESC-R                                  
CTS-02           WRITE NEWT-REC           FROM 
                    Function Upper-case(NEW-TECH-REC-2) 
              display 'write 22 ' NEW-TECH-REC-2 

AL-01A           ADD 1     TO WK-NT-REC-CNT              
CTS-02*                                                                 
CTS-02           MOVE SPACE               TO NT-EURO-DESC-A             
CTS-02           ADD  1                   TO WK-LINE-CNT                
CTS-02           MOVE WK-LINE-CNT         TO NT-LVL-D                   
CTS-02           MOVE 'PUBLICATION IN:'   TO NT-EURO-DESC               
CTS-02           WRITE NEWT-REC           FROM 
                     Function Upper-case(NEW-TECH-REC-2)  
              display 'write 23 ' NEW-TECH-REC-2 

AL-01A           ADD 1     TO WK-NT-REC-CNT              
CTS-02        END-IF.                                                   
CTS-02*                                                                 
CTS-02 EURO-TOTALS-PARA.                                                
CTS-02*-----------------*                                               
CTS-02     MOVE SPACES                 TO WK-EURO-PROCESS               
      *CTS - 06/05/07 CHANGE BEGINS                                     
004133     ADD  1                      TO WK-TMP-LINE-CNT               
CTS-02*    MOVE NT-LVL-D               TO NT-LVL-F1                     
AL-20 * TAKE OUT                                                        
CTS-02*    MOVE WK-TMP-LINE-CNT        TO NT-LVL-F1                     
AL-20 * TAKE OUT                                                        
      *CTS - 06/05/07 CHANGE ENDS                                       
CTS-02     MOVE WK-TMP-LINE-CNT        TO NT-LVL-F2                     
CTS-02                                    NT-LVL-G                      
CTS-02                                    NT-LVL-H                      
CTS-02                                    NT-LVL-I                      
CTS-02                                    WK-LINE-CNT                   
CTS-02     MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
CTS-02     MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
AL-20 *take out                                                         
CTS-02*    MOVE '03'                   TO NT-TAB2-P6.                   
AL-20 *take out                                                         
CTS-02     MOVE '04'                   TO NT-TAB2-P7.                   
CTS-02     MOVE '05'                   TO NT-TAB2-P8.                   
CTS-02     MOVE '06'                   TO NT-TAB2-P9.                   
CTS-02     MOVE '07'                   TO NT-TAB2-P10.                  
AL-20 *take out                                                         
CTS-02*    MOVE WK-HOLD-D6             TO NT-DATA-6.                    
AL-20 *take out                                                         
CTS-02     MOVE WK-HOLD-D7             TO NT-DATA-7.                    
CTS-02     MOVE WK-HOLD-D8             TO NT-DATA-8.                    
CTS-02     MOVE WK-HOLD-D9             TO NT-DATA-9.                    
CTS-02     MOVE WK-HOLD-GROSS-AMT      TO NT-GROSS-AMT                  
CTS-02                                    NT-DISP-GRS-AMT.              
AL-20      MOVE '08'                   TO NT-TAB2-P16.                  
AL-20      MOVE WK-TMP-LINE-CNT        TO NT-LVL-O.                     
AL-20      MOVE WK-HOLD-NET-AMT-NOTAX  TO NT-NET-DTL-AMT.               
CTS-02     MOVE WK-HOLD-NET-AMT        TO NT-DISP-NET-AMT.              
CTS-02     MOVE WK-HOLD-TAX-AMT        TO NT-DISP-TAX-AMT.              
CTS-02     MOVE WK-HOLD-COM-AMT        TO NT-DISP-COM-AMT.              
CTS-02*                                                                 
*********  IF WK-WO-EDN-MAIN = 'INTL' OR 'ASIA' OR 'ATL'                
**********    PERFORM PROCESS-MAIN-EDITION                              
      *CTS - 06/05/07 CHANGE BEGINS                                     
*********     ADD  1                TO WK-LINE-CNT                      
      *CTS - 06/05/07 CHANGE ENDS                                       
**********    MOVE WK-LINE-CNT      TO NT-LVL-K                         
*********     MOVE '02'             TO NT-TAB2-P12                      
*********  END-IF.                                                      

CTS-02     MOVE SPACES              TO WK-WO-EDN-MAIN.                  
CTS-02     MOVE SPACES              TO WK-MAIN-EDN-A.                   
CTS-02*                                                                 
CTS-02     IF WK-HOLD-D5 NOT = SPACES                                   
CTS-02        ADD  1                   TO WK-LINE-CNT                   
CTS-02        MOVE WK-LINE-CNT         TO NT-LVL-E                      
CTS-02        MOVE '02'                TO NT-TAB2-P5                    
CTS-02        MOVE WK-HOLD-D5          TO NT-DATA-5                     
CTS-02     END-IF.                                                      
CTS-02     WRITE NEWT-REC              FROM 
               Function Upper-case(NEW-TECH-REC-2).
              display 'write 24 ' NEW-TECH-REC-2 

AL-01A     ADD 1     TO WK-NT-REC-CNT.              
CTS-02     INITIALIZE WK-HOLD-DATA.                                     
CTS-02*                                                                 
CTS-02 PROCESS-MONTH-YEAR.                                              
CTS-02*-------------------*                                             
CTS-02     EVALUATE WK-MM-1                                             
CTS-02        WHEN 1                                                    
CTS-02            MOVE 'JANUARY'       TO WK-EU-MONTH                   
CTS-02        WHEN 2                                                    
CTS-02            MOVE 'FEBRUARY'      TO WK-EU-MONTH                   
CTS-02        WHEN 3                                                    
CTS-02            MOVE 'MARCH'         TO WK-EU-MONTH                   
CTS-02        WHEN 4                                                    
CTS-02            MOVE 'APRIL'         TO WK-EU-MONTH                   
CTS-02        WHEN 5                                                    
CTS-02            MOVE 'MAY'           TO WK-EU-MONTH                   
CTS-02        WHEN 6                                                    
CTS-02            MOVE 'JUNE'          TO WK-EU-MONTH                   
CTS-02        WHEN 7                                                    
CTS-02            MOVE 'JULY'          TO WK-EU-MONTH                   
CTS-02        WHEN 8                                                    
CTS-02            MOVE 'AUGUST'        TO WK-EU-MONTH                   
CTS-02        WHEN 9                                                    
CTS-02            MOVE 'SEPTEMBER'     TO WK-EU-MONTH                   
CTS-02        WHEN 10                                                   
CTS-02            MOVE 'OCTOBER'       TO WK-EU-MONTH                   
CTS-02        WHEN 11                                                   
CTS-02            MOVE 'NOVEMBER'      TO WK-EU-MONTH                   
CTS-02        WHEN 12                                                   
CTS-02            MOVE 'DECEMBER'      TO WK-EU-MONTH                   
CTS-02     END-EVALUATE                                                 
CTS-02*                                                                 
CTS-02     IF WK-YY-1 < 83                                              
CTS-02        MOVE '20'                TO WK-EU-YY1                     
CTS-02        MOVE WK-YY-1             TO WK-EU-YY2                     
CTS-02     ELSE                                                         
CTS-02        MOVE '19'                TO WK-EU-YY1                     
CTS-02        MOVE WK-YY-1             TO WK-EU-YY2                     
CTS-02     END-IF.                                                      
CTS-02*                                                                 
CTS-02 PROCESS-MONTH-YEAR-FUNDS.                                        
CTS-02*-------------------------*                                       
CTS-02     EVALUATE NT-BILL-MM-1                                        
CTS-02        WHEN 1                                                    
CTS-02            MOVE 'JANUARY'       TO WK-EU-MONTH                   
CTS-02        WHEN 2                                                    
CTS-02            MOVE 'FEBRUARY'      TO WK-EU-MONTH                   
CTS-02        WHEN 3                                                    
CTS-02            MOVE 'MARCH'         TO WK-EU-MONTH                   
CTS-02        WHEN 4                                                    
CTS-02            MOVE 'APRIL'         TO WK-EU-MONTH                   
CTS-02        WHEN 5                                                    
CTS-02            MOVE 'MAY'           TO WK-EU-MONTH                   
CTS-02        WHEN 6                                                    
CTS-02            MOVE 'JUNE'          TO WK-EU-MONTH                   
CTS-02        WHEN 7                                                    
CTS-02            MOVE 'JULY'          TO WK-EU-MONTH                   
CTS-02        WHEN 8                                                    
CTS-02            MOVE 'AUGUST'        TO WK-EU-MONTH                   
CTS-02        WHEN 9                                                    
CTS-02            MOVE 'SEPTEMBER'     TO WK-EU-MONTH                   
CTS-02        WHEN 10                                                   
CTS-02            MOVE 'OCTOBER'       TO WK-EU-MONTH                   
CTS-02        WHEN 11                                                   
CTS-02            MOVE 'NOVEMBER'      TO WK-EU-MONTH                   
CTS-02        WHEN 12                                                   
CTS-02            MOVE 'DECEMBER'      TO WK-EU-MONTH                   
CTS-02     END-EVALUATE                                                 
CTS-02*                                                                 
CTS-02     STRING NT-BILL-YY-1         DELIMITED BY SIZE                
CTS-02            NT-BILL-YY-2         DELIMITED BY SIZE                
CTS-02     INTO   WK-EU-YEAR.                                           
CTS-02*                                                                 
CTS-02 PROCESS-ALL-TOTALS.                                              
CTS-02*-------------------*                                             
CTS-02     INITIALIZE NEW-TECH-REC-5.                                   
CTS-02     MOVE '5'                    TO NT-CODE-5A.                   
CTS-02     subtract  ttl-digital-gross-amt   
                from TTL-TTL-GROSS-AMT
CTS-02     subtract ttl-digital-net-amt 
                from TTL-TTL-net-AMT
                    
CTS-02     MOVE TTL-TTL-GROSS-AMT      TO NT-TOT-IHT-GROSS              
CTS-02     MOVE TTL-TTL-NET-AMT        TO NT-TOT-IHT-NET.               
CTS-02     MOVE TTL-TTL-ALL-TAX-AMT    TO NT-TOT-IHT-TAX.               
           MOVE TTL-TTL-ALL-COM-AMT    TO NT-TOT-IHT-COMM.              

CTS-02     MOVE TTL-TTL-ALL-D-P-AMT    TO NT-TOT-IHT-D-P.               
CTS-02     MOVE TTL-TTL-ALL-SPL-CHG    TO NT-TOT-IHT-SPL.               
CTS-02     MOVE TTL-TTL-GROSS-AMT-ADJ  TO NT-TOT-IHT-ADJ.               
CTS-02     MOVE PPD-CC-AMT             TO NT-TOT-IHT-CC.                
CTS-02*                                                 
********   MOVE zero                   TO NT-TOT-IHT-COMM           
********                                  NT-TOT-IHT-CC
                                          
CTS-02     WRITE NEWT-REC              FROM NEW-TECH-REC-5.             
AL-01A     ADD 1     TO WK-NT-REC-CNT.              
CTS-02*                                                                 
CTS-02 ADD-ALL-TOTALS.                                                  
CTS-02*---------------*                                                 
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-GRS-AMT = TTL-TTL-ALL-GRS-AMT +          
CTS-02                                   WK-TTL-ALL-GRS-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-NET-AMT = TTL-TTL-ALL-NET-AMT +          
CTS-02                                   WK-TTL-ALL-NET-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-COM-AMT = TTL-TTL-ALL-COM-AMT +          
CTS-02                                   WK-TTL-ALL-COM-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-TAX-AMT = TTL-TTL-ALL-TAX-AMT +          
CTS-02                                   WK-TTL-ALL-TAX-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-D-P-AMT = TTL-TTL-ALL-D-P-AMT +          
CTS-02                                   WK-TTL-ALL-D-P-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-SPL-CHG = TTL-TTL-ALL-SPL-CHG +          
CTS-02                                   WK-TTL-ALL-SPL-CHG.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-ADJ-AMT = TTL-TTL-ALL-ADJ-AMT +          
CTS-02                                   WK-TTL-ALL-ADJ-AMT.            
CTS-02*                                                                 
CTS-02     COMPUTE TTL-TTL-ALL-CC-AMT  = TTL-TTL-ALL-CC-AMT +           
CTS-02                                   WK-TTL-ALL-CC-AMT.             
CTS-02*                                                                 
CTS-02 PROCESS-CREDIT-CARD-DETAILS.                                     
341900*----------------------------*                                    
193900*>   Detail line C                                                
194000                                                                  
194100     PERFORM WRITE-DETAIL-LINE.                                   
           PERFORM FORMAT-WO-INFO                                       
           PERFORM WRITE-DETAIL-NONB.                                   
-2005-                                                                  
CTS-02     MOVE ZEROS                       TO WK-TEMP-MMYY.            
08/30-     Move WO-INVC-CODE (1:1)          to Ws-Invc-Code.            
-2001-                                                                  
      *CTS - 06/05/07 CHANGE BEGINS                                     
09/07-***  If   WO-REVISED-FLAG = 'B' or 'I'                            
09/07-***       Next Sentence                                           
09/07-***  Else                                                         
      *CTS - 06/05/07 CHANGE ENDS                                       
-2005-     If   CrCard                                                  
08/30-     And  WO-CC-AUTH-CODE = Space                                 
09/07-          Move WO-MULTI-APP           to CCW-CC-WO-MULTI-APP      
09/07-          Move WO-INVC-CODE           to CCW-CC-TYPE              
09/07-          Move WO-CC-NUMBER           to CCW-CC-NBR               
09/07-          Move WO-CC-AUTH-CODE        to CCW-CC-AUTH-CODE         
09/07-          Move WO-CC-EXP-DATE         to CCW-CC-EXP-DATE          
09/07-          Move WO-CC-AUTH-AMT         to CCW-CC-AUTH-AMT          
09/07-          Move WO-INVC-AMT            to CCW-CC-INVC-AMT          
09/07-          Move WO-INVC-NBR            to CCW-CC-INVC-NBR          
09/07-          Move WO-JOB-NBR             to CCW-CC-JOB-NBR           
09/07-          Move WO-ACCT-KEY            to CCW-CC-ACCT-NBR          
09/07-          Move SSL-INVC-DATE          to CCW-CC-INVC-DATE         
09/07-                                                                  
09/07-          Write CCW-REC From CCW-SEL-REC                          
08/30-     Else                                                         
fxc---     If  (CrCard or NytdCc)                                       
DW-08      And  WO-INVC-AMT > Zeros                                     
09/07-          Move WO-MULTI-APP           to CCW-CC-WO-MULTI-APP         
09/07-          Move WO-INVC-CODE           to CCW-CC-TYPE                 
09/07-          Move WO-CC-NUMBER           to CCW-CC-NBR                  
09/07-          Move WO-CC-AUTH-CODE        to CCW-CC-AUTH-CODE            
09/07-          Move WO-CC-EXP-DATE         to CCW-CC-EXP-DATE             
09/07-          Move WO-CC-AUTH-AMT         to CCW-CC-AUTH-AMT             
09/07-          Move WO-INVC-AMT            to CCW-CC-INVC-AMT             
09/07-          Move WO-INVC-NBR            to CCW-CC-INVC-NBR             
09/07-          Move WO-JOB-NBR             to CCW-CC-JOB-NBR              
09/07-          Move WO-ACCT-KEY            to CCW-CC-ACCT-NBR             
09/07-          Move SSL-INVC-DATE          to CCW-CC-INVC-DATE            
09/07-                                                                     
09/07-          Write CCW-REC From CCW-SEL-REC                             
09/07-                                                                     
09/07-          Compute WO-CC-AUTH-AMT = Zeros      - WO-INVC-AMT          
AL-07 ****      Compute PPD-CC-AMT     = PPD-CC-AMT + WO-INVC-AMT          
09/07-                                                                     
09/07-          Add WO-CC-AUTH-AMT          to DTL-NET-AMT                 
09/07-****                                     TTL-NET-AMT                 
TEST-3*                                        TTL-AGY-ADV-NET-AMT         
CTS-02**                                       TTL-AGY-ADV-CC-AMT          
***********                                    TTL-AGY-NET-AMT             
09/07-                                         TTL-IAR-10-AMT              
09/07-                                                                     
09/07-         Move 'Payment - Thank You -' to DTL-DESCRIPTION             
CTS-02         MOVE 'PAID BY '              TO WK-PAY-L1                   
CTS-02         MOVE WO-CC-NUMBER            TO WK-CC-DETAILS-R             
CTS-02         MOVE 'NO '                   TO WK-PAY-LIT1-1               
CTS-02         MOVE 'XXXX-XXXX-XXXX-'       TO WK-PAY-VAL1-1               
CTS-02         MOVE WK-CC-13-16             TO WK-PAY-VAL1-2               
CTS-02*                                                                    
CTS-02         MOVE 'EXP'                   TO WK-PAY-L3                   
CTS-02         MOVE WO-CC-EXP-DATE          TO WK-TEMP-MMYY                
CTS-02         MOVE WK-T1-MM                TO WK-PAY-VAL2-1               
CTS-02         MOVE '/'                     TO WK-PAY-L4                   
CTS-02         MOVE WK-T1-YY                TO WK-PAY-VAL2-2               
CTS-02         MOVE 'CODE'                  TO WK-PAY-L5                   
09/07-                                                                     
09/07-         Evaluate WO-INVC-CODE (1:1)                                 
09/07-             When 'A'     Move 'Amex' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'AMEX CARD'  TO WK-PAY-L2             
09/07-             When 'D'     Move 'DC  ' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'DC CARD'    TO WK-PAY-L2             
09/07-             When 'M'     Move 'MC  ' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'MC CARD'    TO WK-PAY-L2             
09/07-             When 'S'     Move 'Disc' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'DISC CARD'  TO WK-PAY-L2             
09/07-             When 'V'     Move 'Visa' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'VISA CARD'  TO WK-PAY-L2             
DW-08              When 'N'     Move 'NYTD' to DTL-DESCRIPTION (23:4)      
CTS-02                          MOVE 'NYTD CARD'  TO WK-PAY-L2             
CTS-02*            When 'E'     Move 'EURO' to DTL-DESCRIPTION (23:4)      
CTS-02*                         MOVE 'EUROCARD :' TO WK-PAY-L2             
09/07-         End-Evaluate                                                
09/07-                                                                     
CTS-02         IF WK-PAY-MODE1 NOT = SPACES             AND                
CTS-02            WK-PAY-MODE2 NOT = SPACES             AND                
CTS-02            WK-PAY-MODE1 NOT = WK-SAVE-PAY-MODE1  AND                
CTS-02            WK-PAY-MODE2 NOT = WK-SAVE-PAY-MODE2                     
CTS-02              MOVE WK-PAY-MODE1       TO WK-SAVE-PAY-MODE1           
CTS-02              MOVE WK-PAY-MODE2       TO WK-SAVE-PAY-MODE2           
CTS-02         END-IF                                                      
CTS-02*                                                                    
09/07-         Move WO-CC-AUTH-AMT          to DTL-NET-AMT-P               
CTS-02*                                        NT-CC-PAID-AMT              
09/07-         Perform WRITE-DETAIL-LINE.                                  
CTS-02*                                                                    
CTS-02 CHECK-AND-LOAD-DISC-AMT.                                            
CTS-02*------------------------*                                           
CTS-02        WRITE NEWT-REC       FROM 
                  Function Upper-case(NEW-TECH-REC-2)
              display 'write 25 ' NEW-TECH-REC-2 

AL-01A        ADD 1     TO WK-NT-REC-CNT              
CTS-02        INITIALIZE NEW-TECH-REC-2                                    
CTS-02*                                                                    

CTS-02 CHECK-AND-LOAD-SPL-AMT.                                             
CTS-02*-----------------------*                                            
CTS-02     IF WK-EU-PG-FLG = 'E'                                           
CTS-02        MOVE NT-AD-DESC-VAL-R1 TO EU-DISC-SPL-ADJ-DESC(EU-R)         
CTS-02        MOVE DTL-GROSS-AMT     TO EU-DISC-SPL-ADJ-AMT (EU-R)         
CTS-02        SET EU-R               UP BY 1                               
CTS-02        ADD 1                  TO EURO-AMT-MAX                       
CTS-02        PERFORM TEST-EU-REACH                                        
CTS-02        INITIALIZE NT-DISC-AMT-A                                     
CTS-02        INITIALIZE NT-AMT-HDR-A                                      
CTS-02        INITIALIZE NT-SPL-CHG-A                                      
CTS-02        MOVE ZEROS           TO NT-DISP-NET-AMT                      
CTS-02        MOVE SPACES          TO NT-TAB2-P3                           
CTS-02                                NT-TAB2-P10                          
CTS-02                                NT-LVL-C                             
CTS-02                                NT-LVL-I                             
CTS-02     ELSE                                                            
CTS-02        WRITE NEWT-REC       FROM 
                  Function Upper-case(NEW-TECH-REC-2) 
              display 'write 26 ' NEW-TECH-REC-2 

AL-01A        ADD 1     TO WK-NT-REC-CNT              
CTS-02        INITIALIZE NEW-TECH-REC-2                                    
CTS-02     END-IF.                                                         
CTS-02*                                                                    
CTS-02 CHECK-AND-LOAD-ADJ-AMT.                                             
CTS-02*-----------------------*                                            
CTS-02     IF WK-EU-PG-FLG = 'E'                                           
CTS-02        MOVE NT-ADJ-CHG-DESC   TO EU-DISC-SPL-ADJ-DESC(EU-R)         
CTS-02        MOVE DTL-GROSS-AMT     TO EU-DISC-SPL-ADJ-AMT (EU-R)         
AL-20         MOVE DTL-NET-NOTAX   TO EU-DISC-SPL-ADJ-NOTAX(EU-R)          
CTS-02        SET EU-R               UP BY 1                               
CTS-02        ADD 1                  TO EURO-AMT-MAX                       
CTS-02        PERFORM TEST-EU-REACH                                        
CTS-02        INITIALIZE NT-DISC-AMT-A                                     
CTS-02        INITIALIZE NT-AMT-HDR-A                                      
CTS-02        INITIALIZE NT-DISP-ADJ-AMT                                   
AL-20         INITIALIZE NT-NET-DTL-AMT                                    
AL-20         INITIALIZE NT-NET-DETL-LINE                                  
CTS-02        MOVE ZEROS           TO NT-DISP-NET-AMT                      
CTS-02        MOVE SPACES          TO NT-TAB2-P3                           
CTS-02                                NT-TAB2-P10                          
AL-20                                 NT-TAB2-P16                          
CTS-02                                NT-LVL-C                             
CTS-02                                NT-LVL-I                             
AL-20                                 NT-LVL-O                             
CTS-02     ELSE                                                            
CTS-02        WRITE NEWT-REC       FROM 
                  Function Upper-case(NEW-TECH-REC-2)      
              display 'write 23 ' NEW-TECH-REC-2 

AL-01A        ADD 1     TO WK-NT-REC-CNT              
AL-20         ADD  1                  TO WK-LINE-CNT                       
AL-20         MOVE WK-LINE-CNT        TO NT-LVL-C                          
AL-20                                    NT-LVL-I                          
AL-20                                    NT-LVL-O                          
AL-20         MOVE '02'               TO NT-TAB2-P3                        
AL-20         MOVE '07'               TO NT-TAB2-P10                       
AL-20         MOVE '08'               TO NT-TAB2-P16                       
AL-20         MOVE SPACES             TO NT-DATA-3                         
AL-20         MOVE 'Adj Gross/Net: '  TO NT-ADJ-DESC                       
AL-20 ***     MOVE WO-INVC-AMT        TO NT-ADJ-AMT                        
AL-20 ***     MOVE WO-INVC-AMT        TO NT-NET-DTL-AMT                    
AL-20 ***     COMPUTE NT-NET-DTL-AMT = WO-INVC-AMT + WO-TOT-SALES-TAX      
AL-20         COMPUTE NT-NET-DTL-AMT = WO-INVC-AMT - WO-TOT-SALES-TAX      
AL-20 ***     COMPUTE NT-ADJ-AMT  = WO-INVC-AMT + WO-AGY-COMM +            
AL-20         COMPUTE NT-ADJ-AMT  = WO-INVC-AMT + WO-AGY-COMM -            
AL-20                               WO-TOT-SALES-TAX                       
AL-20         INITIALIZE NT-DISP-ADJ-AMT                                   
AL-20         INITIALIZE NT-DISP-NET-AMT         
              display 'write 24 ' NEW-TECH-REC-2 

CTS-02        WRITE NEWT-REC       FROM 
                  Function Upper-case(NEW-TECH-REC-2)                     
AL-01A        ADD 1     TO WK-NT-REC-CNT              
CTS-02     END-IF.                                                         
CTS-02*                                                                    
CTS-02 UNLOAD-AMTS-TABLE.                                                  
CTS-02*------------------*                                                 
CTS-02     PERFORM VARYING EU-R FROM 1 BY 1                                
CTS-02         UNTIL EU-R > EURO-AMT-MAX                                   
CTS-02         IF EU-DISC-SPL-ADJ-DESC(EU-R) NOT = SPACES AND              
CTS-02            EU-DISC-SPL-ADJ-AMT (EU-R) NOT = ZEROS                   
CTS-02            INITIALIZE NEW-TECH-REC-2                                
CTS-02            MOVE '2'               TO NT-CODE-2A                     
CTS-02            MOVE '02'              TO NT-TAB2-P3                     
CTS-02            MOVE '07'              TO NT-TAB2-P10                    
AL-20             MOVE '08'              TO NT-TAB2-P16                    
      *CTS - 06/05/07 CHANGE BEGINS                                        
CTS-02            ADD  2                 TO WK-LINE-CNT                    
      *CTS - 06/05/07 CHANGE ENDS                                          
CTS-02            MOVE WK-LINE-CNT       TO NT-LVL-C                       
CTS-02                                      NT-LVL-I                       
AL-20                                       NT-LVL-O                       
CTS-02            MOVE WK-EU-PG-FLG      TO NT-CODE-2B                     
CTS-02            MOVE WK-JOB-SEQ-CNT    TO NT-JOB-SEQ                     
CTS-02            MOVE EU-DISC-SPL-ADJ-DESC(EU-R) TO NT-DATA-3             
CTS-02            MOVE EU-DISC-SPL-ADJ-AMT (EU-R) TO NT-EURO-AMT           
CTS-02                                               NT-DISP-NET-AMT       
CTS-02            MOVE EU-DISC-SPL-ADJ-NOTAX (EU-R) TO NT-NET-DTL-AMT      
CTS-02            WRITE NEWT-REC         FROM 
                      Function Upper-case(NEW-TECH-REC-2)   
              display 'write 25 ' NEW-TECH-REC-2 

AL-01A            ADD 1     TO WK-NT-REC-CNT              
CTS-02         END-IF                                                      
CTS-02     END-PERFORM.                                                    
CTS-02*                                                                    
146500     IF WK-EU-PG-FLG = 'E'                                           
CTS-02        INITIALIZE NEW-TECH-REC-2                                    
CTS-02        MOVE '2'                    TO NT-CODE-2A                    
CTS-02        MOVE WK-EU-PG-FLG           TO NT-CODE-2B                    
CTS-02        MOVE WK-JOB-SEQ-CNT         TO NT-JOB-SEQ                    
CTS-02        MOVE '02'                   TO NT-TAB2-P14                   
CTS-02        ADD  1                      TO WK-LINE-CNT                   
CTS-02        MOVE WK-LINE-CNT            TO NT-LVL-M                      
CTS-02        MOVE 'REF NBR: '            TO NT-JOB-NBR-LIT                
CTS-02        MOVE WK-EU-JOB-NBR          TO NT-JOB-NBR                    
AL-28         IF WK-PO-NBR   NOT = SPACES                                  
AL-28            MOVE 'PO NBR:  '         TO NT-PO-NBR-LIT                 
AL-28            MOVE WK-PO-NBR           TO NT-PO-NBR                     
AL-28         END-IF                                                       
CTS-02        WRITE NEWT-REC       FROM 
                   Function Upper-case(NEW-TECH-REC-2) 
              display 'write 26 ' NEW-TECH-REC-2 

AL-01A        ADD 1     TO WK-NT-REC-CNT              
CTS-02        INITIALIZE NEW-TECH-REC-2                                    
           END-IF.                                                         
CTS-02*                                                                    
CTS-02 PRINT-ADV-DETAILS.                                                  
CTS-02*------------------*                                                 
CTS-02     MOVE NAD-NAMe-1         TO NT-HDR-ADV-NAME                      
CTS-02                                WK-TEMP-ADV-NAME                     
CTS-02     MOVE NAD-NAMe-2         TO NT-HDR-ADV-NAM2                      
      ***  MOVE NAD-NAMe-3         TO NT-HDR-ADV-NAM3                      
CTS-02     MOVE NAD-STREET-1       TO NT-HDR-ADV-STREET                    
CTS-02     MOVE NAD-CITY-1         TO NT-HDR-ADV-CITY                      
CTS-02     MOVE NAD-STATE (1)      TO NT-HDR-ADV-STATE                     
CTS-02     MOVE NAD-ZIP (1)        TO WK-ZIP-2                             
CTS-02     MOVE WK-ZIP-2-5         TO NT-HDR-ADV-ZIP-5                     
CTS-02                                                                     
CTS-02     IF WK-ZIP-2-4 NOT = SPACES                                      
CTS-02        MOVE '-'             TO NT-HDR-ADV-ZIP-SL                    
CTS-02        MOVE WK-ZIP-2-4      TO NT-HDR-ADV-ZIP-4                     
CTS-02     END-IF                                                          
CTS-02                                                                     
CTS-02     IF NAD-CNTRY (1) NOT = SPACES                                   
CTS-02        MOVE NAD-CNTRY (1)   TO NT-HDR-ADV-CNTRY                     
CTS-02     ELSE                                                            
CTS-02        MOVE 'USA'           TO NT-HDR-ADV-CNTRY                     
CTS-02     END-IF                                                          
CTS-02                                                                     
CTS-02**   IF NAD-VAT-ID > SPACES                                          
AL-21      IF NAD-VAT-BILL-CNTRY > SPACES                                  
AL-21        OR  NAD-VAT-ID > SPACES                                       
              MOVE 'CNTY'                 TO COD-KEY                       
              MOVE NAD-VAT-BILL-CNTRY     TO COD-CODE1                     
              CALL 'GIT'   USING  COD-FILE, COD-REC, IO-PKT                
              IF STATUS-CODE = 0                                           
                 MOVE COD-NAME       TO NT-HDR-ADV-CNTRY                   
                 IF NAD-VAT-ID > SPACES                                    
                  IF COD-FLAG1 NOT = '2'                                   
                   MOVE NAD-VAT-ID          TO NT-VAT-ADV-ID               
                   MOVE '2'                 TO NT-VAT-FLG                  
                   MOVE 'CLIENT VAT CODE:'  TO NT-VAT-LIT                  
AL-21              MOVE 'M3'                TO WK-MSG-CD3.                 
                                                                           
CTS-02*                                                                    
CTS-02*    MOVE NAD-ACCT-NBR           TO NAD-ACCT-NBR.                    
CTS-02*****PERFORM GET-J-XREF.                                             
AL-20      PERFORM GET-AGY-XRF.                                            
CTS-02     MOVE WK-JXRF-AREA           TO WK-ADV-KEY.                      
AL-01** 
AL-01      MOVE NAD-ACCT-NBR           TO NT-HDR-ADV-NBR-X                 
AL-01                                     WK-HOLD-ADV-NBR.
AL-01**                    
      *     IF WK-ADV-FILLER = '='                                         
CTS-02*        MOVE WK-ADV-KEY-1           TO NT-HDR-ADV-NBR-X             
CTS-02*                                       WK-HOLD-ADV-NBR              
      *     ELSE                                                           
CTS-02*        MOVE WK-ADV-KEY             TO NT-HDR-ADV-NBR-X             
CTS-02*                                       WK-HOLD-ADV-NBR              
      *     END-IF.                                                        
      *    display 'adv:' nad-acct-nbr                                     
      *            ' ' wk-adv-key                                          
      *            ' ' NAD-XRF-KEY                                         
      *            ' ' NT-HDR-ADV-NBR-X.                                   
CTS-02*                                                                    
CTS-02 PRINT-AGY-DETAILS.                                                  
CTS-02*------------------*                                                 
CTS-02*>   Billing Address                                                 
CTS-02                                                                     
CTS-02     MOVE NA2-NAMe-1             TO NT-HDR-AGY-NAME.                 
CTS-02     MOVE NA2-NAMe-2             TO NT-HDR-AGY-NAM2.                 
    ****   MOVE NA2-NAMe-3             TO NT-HDR-AGY-NAM3.                 
CTS-02     MOVE NA2-STREET-1           TO NT-HDR-AGY-STREET.               
CTS-02     MOVE NA2-CITY-1             TO NT-HDR-AGY-CITY.                 
CTS-02     MOVE NA2-STATE (1)          TO NT-HDR-AGY-STATE.                
CTS-02     MOVE NA2-ZIP (1)            TO WK-ZIP-2                         
CTS-02     MOVE WK-ZIP-2-5             TO NT-HDR-AGY-ZIP-5.                
CTS-02                                                                     
CTS-02     IF  WK-ZIP-2-4 NOT = SPACES                                     
CTS-02         MOVE '-'                TO NT-HDR-AGY-ZIP-SL                
CTS-02         MOVE WK-ZIP-2-4         TO NT-HDR-AGY-ZIP-4.                
CTS-02                                                                     
CTS-02     IF  NA2-CNTRY (1) NOT = SPACE                                   
CTS-02         MOVE NA2-CNTRY (1)      TO NT-HDR-AGY-CNTRY                 
CTS-02     ELSE                                                            
CTS-02        MOVE 'USA'               TO NT-HDR-AGY-CNTRY                 
CTS-02     END-IF.                                                         
CTS-02*                                                                    
TEST-2*    MOVE SPACES                 TO WK-MSG-CD3.                      
                                                                           
CTS-02**   IF NA2-VAT-ID > SPACES                                          
AL-21      IF NA2-VAT-BILL-CNTRY > SPACES                                  
AL-21        OR  NA2-VAT-ID > SPACES                                       
AL-21        MOVE 'CNTY'                 TO COD-KEY                        
AL-21        MOVE NA2-VAT-BILL-CNTRY     TO COD-CODE1                      
AL-21        CALL 'GIT'   USING  COD-FILE, COD-REC, IO-PKT                 
AL-21        IF STATUS-CODE = 0                                            
AL-21           MOVE COD-NAME            TO NT-HDR-AGY-CNTRY               
AL-21           IF NA2-VAT-ID > SPACES                                     
AL-21             IF COD-FLAG1 NOT = '2'                                   
AL-21                MOVE NA2-VAT-ID          TO NT-VAT-AGY-ID             
AL-21                MOVE '1'                 TO NT-VAT-FLG                
AL-21                MOVE 'CLIENT VAT CODE:'  TO NT-VAT-LIT                
AL-21                MOVE 'M3'                TO WK-MSG-CD3.               
                                                                           
                                                                           
CTS-02     MOVE NA2-ACCT-NBR           TO NAD-ACCT-NBR.                    
CTS-02**   PERFORM GET-J-XREF.                                             
AL-20      PERFORM GET-AGY-XRF.                                            
CTS-02     MOVE WK-JXRF-AREA           TO WK-AGY-KEY.                      
AL-01**
AL-01      MOVE NA2-ACCT-NBR           TO NT-HDR-AGY-NBR-X 
AL-01                                     WK-HOLD-AGY-NBR.
AL-01**
      *    IF WK-AGY-FILLER = '='                                          
CTS-02*        MOVE WK-AGY-KEY-1           TO NT-HDR-AGY-NBR-X             
CTS-02*                                       WK-HOLD-AGY-NBR              
      *     ELSE                                                           
CTS-02*        MOVE WK-AGY-KEY             TO NT-HDR-AGY-NBR-X             
CTS-02*                                       WK-HOLD-AGY-NBR              
      *     END-IF.                                                        
      *    display 'agy:' nad-acct-nbr                                     
      *            ' ' wk-agy-key                                          
      *            ' ' NAD-XRF-KEY                                         
      *            ' ' NT-HDR-Agy-NBR-X.                                   
TEST-2*                                                                    
TEST-2 GET-IHT-LAST-INVC-NBR.                                              
TEST-2*----------------------*                                             
TEST-2                                                                     
TEST-2     MOVE '$IHT'                 TO COD-KEY.                         
TEST-2     CALL 'GIT'   USING  CTL-FILE, COD-REC, IO-PKT.                  
TEST-2     PERFORM CHECK-STATUS.                                           
TEST-2                                                                     
TEST-2     MOVE COD-VAL-9              TO LAST-INVC-NBR.    
           display 'last inv: ' last-invc-nbr.
TEST-2                                                                     
TEST-2 PUT-IHT-LAST-INVC-NBR.                                              
TEST-2*----------------------*                                             
TEST-2                                                                     
TEST-2     MOVE '$IHT'                 TO COD-KEY.                         
TEST-2     CALL 'GETUP' USING  CTL-FILE COD-REC IO-PKT.                    
TEST-2     PERFORM CHECK-STATUS.                                           
TEST-2                                                                     
TEST-2     MOVE LAST-INVC-NBR          TO COD-VAL-9.                       
TEST-2     CALL 'PUT'   USING  CTL-FILE COD-REC IO-PKT.                    
TEST-2     PERFORM CHECK-STATUS.                                           
TEST-2                                                                     
TEST-2 PUT-IHT-NYT-INVC-NBR.                                               
TEST-2*---------------------*                                              
TEST-2                                                                     
TEST-2     MOVE 'xxx1'                 TO COD-KEY.                         
TEST-2     MOVE LAST-INVC-NBR(1:4)     TO COD-CODE1.                       
TEST-2     MOVE LAST-INVC-NBR(5:4)     TO COD-CODE2.  
TEST-2     MOVE LAST-INVC-NBR(9:1)     TO COD-CODE3.  
TEST-2*     MOVE LAST-INVC-NBR(9:1)     TO COD-CODE2.  
TEST-2*     MOVE WK-NT-INV-NBR-6        TO COD-CHAR8(1). 
           MOVE WK-NT-INV-NBR-6        TO COD-CHAR12(1). 
dw-30      move wk-today-date          to cod-log-date           
           display 'iht 1 ' cod-key 
                   ' ' cod-CHAR12(1)
                   ' ' LAST-INVC-NBR.
TEST-2                                                                     
TEST-2     CALL 'INSURT'  USING COD-FILE COD-REC IO-PKT.                   
TEST-2     PERFORM CHECK-STATUS. 
TEST-2                                                                     
TEST-2 PUT-NYT-IHT-INVC-NBR.                                               
TEST-2*---------------------*                                              
TEST-2                                                                     
TEST-2     MOVE 'xxx2'                 TO COD-KEY.                         
TEST-2     MOVE WK-NT-INV-NBR-6(1:4)   TO COD-CODE1.                       
TEST-2     MOVE WK-NT-INV-NBR-6(5:4)   TO COD-CODE2. 
           move wk-nt-inv-nbr-6(9:1)   to cod-code3.
TEST-2*     MOVE WK-NT-INV-NBR-6(9:1)   TO COD-CODE2.       
TEST-2     MOVE LAST-INVC-NBR          TO COD-CHAR12(1).         
dw-30      move wk-today-date          to cod-log-date           
TEST-2     display 'iht 2 ' cod-key 
                   ' ' cod-char12(1)
                   ' ' WK-NT-INV-NBR-6.
TEST-2     CALL 'INSURT' USING COD-FILE COD-REC IO-PKT.                    
TEST-2     PERFORM CHECK-STATUS.                                           
CTS-02*                                                                    
CTS-02 INITIALIZE-EURO-AMTS.                                               
CTS-02*---------------------*                                              
CTS-02     PERFORM VARYING EU-R                                            
CTS-02         FROM 1 BY 1 UNTIL EU-R > EURO-AMT-MAX                       
CTS-02         INITIALIZE  EU-AMTS-UNLOAD(EU-R)                            
CTS-02     END-PERFORM.                                                    
CTS-02*                                                                    
CTS-02 PROCESS-MAIN-EDITION.                                               
CTS-02*---------------------*                                              
CTS-02     MOVE SPACES                 TO WK-END-EDN.                      
CTS-02     MOVE ZEROS                  TO WK-M2.                           
CTS-02     SET NT-M1                   TO 1.                               
CTS-02     MOVE '('                    TO NT-MAIN-EDN-VAL(NT-M1).          
CTS-02     PERFORM VARYING WK-M1                                           
CTS-02         FROM 1 BY 1 UNTIL WK-M1 > 22                                
CTS-02                     OR WK-END-EDN = 'Y'                             
CTS-02         ADD 1                   TO WK-M2                            
CTS-02         IF WK-HOLD-EDN-DESC (WK-M1) NOT = SPACE                     
CTS-02            SET NT-M1            UP BY 1                             
CTS-02            MOVE WK-HOLD-EDN-DESC (WK-M1)                            
CTS-02                                 TO NT-MAIN-EDN-VAL(NT-M1)           
CTS-02         ELSE                                                        
CTS-02            IF WK-M2 NOT = 22                                        
CTS-02               ADD 1                TO WK-M2                         
CTS-02            END-IF                                                   
CTS-02            IF WK-HOLD-EDN-DESC (WK-M2) = SPACE                      
CTS-02               SET NT-M1            UP BY 1                          
CTS-02               MOVE ')'             TO NT-MAIN-EDN-VAL(NT-M1)        
CTS-02               MOVE 'Y'             TO WK-END-EDN                    
CTS-02            END-IF                                                   
CTS-02            ADD -1                  TO WK-M2                         
CTS-02         END-IF                                                      
CTS-02     END-PERFORM.                                                    
CTS-02*                                                                    
      *CTS - 02/23/07 CHANGE ENDS                                          
      *CTS - 02/05/07 CHANGE BEGINS                                        
      ********************************************************             
      *   special call to set flag to check whether this is a*             
      *   valid paid flag to process for this run            *             
      *   DATE: 02/05/07                                     *             
      ********************************************************             
       COPY AMZPEURS.                                                      
      *CTS - 02/05/07 CHANGE ENDS                                          
AL-20                                                                      
AL-20 ***************************************************                  
AL-20 *         GET J-XREF                                                 
AL-20 ***************************************************                  
AL-20  GET-AGY-XRF.                                                        
AL-20 *----------                                                          
AL-20      PERFORM GET-ADV.                                                
AL-20      MOVE LOW-VALUES                TO XRF-REC.                      
AL-20      MOVE NAD-ACCT-NBR              TO XRF-ACCT-NBR.                 
AL-20      IF NAD-ACCT-NBR > 499999999                                     
AL-20         MOVE '5'                    TO XRF-AREA-CODE                 
AL-20      ELSE                                                            
AL-20         MOVE '0'                    TO XRF-AREA-CODE.                
AL-20                                                                      
AL-20      CALL 'SETL'   USING NAD-XRF, XRF-REC, IO-PKT                    
AL-20      PERFORM GET-AGY-XRF-1 UNTIL                                     
AL-20              STATUS-CODE > ZEROS                                     
AL-20              OR XRF-TYPE = 'J'.                                      
AL-20                                                                      
AL-20      IF STATUS-CODE = ZEROS                                          
AL-20         MOVE XRF-NAME               TO WK-JXRF-KEY                   
AL-20         MOVE XRF-NAME               TO WK-JXRF-AREA                  
AL-20      ELSE                                                            
AL-20         MOVE NAD-XRF-KEY            TO WK-JXRF-KEY                   
AL-20         MOVE NAD-XRF-KEY            TO WK-JXRF-AREA.                 
AL-20      CALL 'ESETL'  USING NAD-XRF, XRF-REC, IO-PKT.                   
AL-20                                                                      
AL-20 ***************************************************                  
AL-20 *         GET XRF                                                    
AL-20 ***************************************************                  
AL-20  GET-AGY-XRF-1.                                                      
AL-20 *-------------*                                                      
AL-20      CALL 'SGET' USING NAD-XRF XRF-REC IO-PKT.                       
AL-20      IF NAD-ACCT-NBR NOT = XRF-ACCT-NBR                              
AL-20         MOVE 99 TO STATUS-CODE.                                      
AL-20                                                                      
       Load-FTP-Parms.                                     
           open input ftp-parm                             
           read ftp-parm into wk-ftp-parm-rec              
                at end move 'Y' to wk-ftp-parm-eof.        
                                                           
           Perform Load-FTP-Parms-2                        
               until wk-ftp-parm-eof = 'Y'                 
                                                           
           close ftp-parm.                                 
                                                           
           if  WK-INV-EU-FTP  = space                      
           or  WK-INV-EU-FTP1 = space                      
           or  WK-INV-EU-FTP2 = space                      
           or  WK-INV-eu-FTP3 = space                      
           or  WK-INV-DO-FTP  = space                      
           or  WK-INV-DO-FTP1 = space                      
           or  WK-INV-DO-FTP2 = space                      
           or  WK-INV-DO-FTP3 = space                      
               display '=== ftp parm info missing ==='     
               display wk-ftp-parm-rec.                    
                                                           
           display 'ftp info ' wk-ftp-info.                
                                                           
       Load-FTP-Parms-2.       
           display wk-ftp-parm-rec.
           if  parm-ftp-tag   = 'EUFTP '                   
               move parm-ftp-info      to WK-INV-EU-FTP    
           else                                            
           if  parm-ftp-tag   = 'EUFTP1'                   
               move parm-ftp-info      to WK-INV-EU-FTP1   
           else                                            
           if  parm-ftp-tag   = 'EUFTP2'                   
               move parm-ftp-info      to WK-INV-EU-FTP2   
           else                                            
           if  parm-ftp-tag   = 'EUFTP3'                   
               move parm-ftp-info      to WK-INV-eu-FTP3       
           else                                                
           if  parm-ftp-tag   = 'DOFTP '                       
               move parm-ftp-info      to WK-INV-DO-FTP        
           else                                                
           if  parm-ftp-tag   = 'DOFTP1'                       
               move parm-ftp-info      to WK-INV-DO-FTP1       
           else                                                
           if  parm-ftp-tag   = 'DOFTP2'                       
               move parm-ftp-info      to WK-INV-DO-FTP2       
           else                                                
           if  parm-ftp-tag   = 'DOFTP3'                       
               move parm-ftp-info      to WK-INV-DO-FTP3       
           else                                                
               display '=== ftp parm tag missing ==='          
               display wk-ftp-parm-rec.                        
                                                               
           read ftp-parm into wk-ftp-parm-rec                  
                at end move 'Y' to wk-ftp-parm-eof.            
                                                               
       Get-Bsel-File-Names.
           move space                  to bsel-file-names
********   STRING 
********   'DIR D:\Admarc\AdmQA\tmp\BSEL > %AMTMP%\TEST.TXT! '
********                         DELIMITED BY '!'
********                         INTO CMD-LINE.
           
           MOVE 'AMTMP'                TO ENV-NAME.                     NWI001
           MOVE SPACES                 TO ENV-VALUE.                    NWI001 
           MOVE 100                    TO ENV-VALUE-MAX.                NWI001 
           CALL 'GITVAR'  USING  ENV-NAME                               NWI001 
                                 ENV-VALUE                              NWI001 
                                 ENV-VALUE-MAX                          NWI001 
                                 ENV-VALUE-LEN.                         NWI001 

           DISPLAY 'ENVNAME: ' ENV-NAME.
	   DISPLAY 'ENVVAL : ' ENV-VALUE.
	   
           string 'dir ' delimited by size
                   env-value delimited by space
                  '\BSEL\'  delimited by size
                  ' > %AMTMP%\dir-bsel.txt ' delimited by size
               into CMD-LINE
           display 'cmd-line ' cmd-line 

               
           MOVE 35               TO FUNC.
           CALL "SYSCALL" USING COMMAND-1.
           MOVE SPACES           TO CMD-LINE.

	   
           OPEN INPUT DIR-FILE. 

           MOVE 'N'                    TO EOF-SW. 

           READ DIR-FILE 
             AT END
                MOVE 'Y'               TO EOF-SW. 

           PERFORM Load-Bsel-Names
             UNTIL EOF-SW = 'Y'. 

           CLOSE DIR-FILE. 
           
           display 'bsel-entry ' bsel-entry
           display 'bsel-file-names ' bsel-file-names
           
       Load-Bsel-Names.
      *----------------------

           DISPLAY 'INPUT IS >' 
           display DIR-REC. 
********   dir-rec never changes 

    ****  does not change  
           IF DIR-REC(40:6) = 'bilsel'
	      PERFORM Load-Bsel-Names-2
	   end-if
	      
           READ DIR-FILE 
             AT END
                MOVE 'Y'               TO EOF-SW. 

       Load-Bsel-Names-2
           MOVE SPACE                  TO PRM-FILE-NAME, prm-file-rec.
           STRING  ENV-VALUE                 DELIMITED BY SPACE          
                   "\BSEL\"                       DELIMITED BY SIZE     
     **** does not change  
                   DIR-REC(40:50)             DELIMITED BY SIZE
                                          INTO PRM-FILE-NAME.
           display 'fname================= ' prm-file-name.
           
           add 1                       to bsel-entry
           if  bsel-entry > 500
               display '********* bsel overflow ***********'
           else
              move prm-file-name     to bsel-name (bsel-entry)
              display 'adding bsel name='
                         bsel-name (bsel-entry)
           end-if
           
       open-read-prm-file.      
       
           display 'open-read-prm-file'
           display 'bsel-x ' bsel-x
           OPEN INPUT PRM-FILE.  
           
           IF  PRM-FILE-STATUS NOT = 0
               display 'file open failure ' PRM-FILE-STATUS
	   ELSE
	       PERFORM READ-sel 
376300         PERFORM CREATE-SSL-REC                                       
376400            UNTIL PRM-FILE-STATUS not = 0  
	       CLOSE PRM-FILE
	   end-if
	   
       Setup-Parm-Dates.
           display 'Setup-Parm-Dates.'
           display 'prm-file-name ' 
           display prm-file-name ' ' 
                                    prm-file-name (58:4)
                                    prm-file-name (56:2)
           move space                  to ssl-parm-dates
     ***   
     *** prm-file-name d:\admarc\AdmDev\tmp\BSEL\bilsel_121890786_20150629_20150705.fil                
           unstring prm-file-name delimited by '_'
               into wk-prm-name
                    wk-acct-nbr  
                    wk-prm-start
                    wk-prm-end
           
           display 'wk-prm-start ' wk-prm-start ' '
                   'wk-prm-end ' wk-prm-end 
           

           move  wk-prm-start (5:4)    to PARM-INVC-DATE-F (1:4)                   
           move  wk-prm-start (3:2)    to PARM-INVC-DATE-F (5:2)                   
           
           MOVE PARM-INVC-DATE-f       TO UTL-EDIT-DATE-X
           display 'parm from '  PARM-INVC-DATE-f
           PERFORM EDIT-DATE
           PERFORM EDIT-OUT-DATE
           MOVE UTL-EDIT-DATE-SL       TO WK-INVC-DATE-F
                                          ssl-PARM-INVC-DATE-f

           move  wk-prm-end   (5:4)    to PARM-INVC-DATE-t (1:4)                   
           move  wk-prm-end   (3:2)    to PARM-INVC-DATE-t (5:2)                   
           
           MOVE PARM-INVC-DATE-t       TO UTL-EDIT-DATE-X 
           display 'parm to   '  PARM-INVC-DATE-t
                                          
           PERFORM EDIT-DATE
           PERFORM EDIT-OUT-DATE
           MOVE UTL-EDIT-DATE-SL       TO WK-INVC-DATE-t
                                          ssl-PARM-INVC-DATE-t

379700     MOVE PARM-INVC-DATE-t       TO PARM-INVC-DATE-p
           move PARM-INVC-DATE-p       to UTL-EDIT-DATE-X
                                          ssl-PARM-INVC-DATE-p
379800     PERFORM EDIT-DATE.                                           
379900     MOVE UTL-EDIT-DATE          TO WK-INVC-DATE-P
                                          ssl-PARM-INVC-DATE-p.               
                                          
           display 'ssl-PARM-INVC-DATE-F ' ssl-PARM-INVC-DATE-F ' '
           display  'ssl-PARM-INVC-DATE-T ' ssl-PARM-INVC-DATE-t ' ' 
           display  'ssl-PARM-INVC-DATE-p ' ssl-PARM-INVC-DATE-p ' '
           display  'ssl-parm-dates ' ssl-parm-dates
           
       READ-PRM.
      *-----------
           READ PRM-FILE into sel-rec
	        AT END
		   MOVE 99 TO PRM-FILE-STATUS.
		   
           IF PRM-FILE-STATUS = 0
	      display 'rec ='  PRM-FILE-REC
	      display 'sel3='  sel3-REC
	   ELSE
	      DISPLAY 'FILE STATUS ' PRM-FILE-STATUS.
	       

397600 READ-SEL.                                                        
397700*--------* 
           if  wk-multi-bsel = 'Y'
               READ PRM-FILE into sel-rec
	            AT END MOVE 99     TO PRM-FILE-STATUS
               display 'PRM-FILE-STATUS ' PRM-FILE-STATUS
	   else    
397900         READ SEL-FILE                                                
398000           AT END MOVE 'EOF'     TO SEL-FILE-STATUS
           end-if
           
398100      display 'READ-SEL'
             
             display sel-ACCT-NBR ' '
                       sel-agy-acct-nbr ' ' 
                       sel-pub ' '
                       sel-issue ' '
                       sel-job-nbr
                       
       