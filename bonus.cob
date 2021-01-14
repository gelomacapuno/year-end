       IDENTIFICATION DIVISION.
       PROGRAM-ID. bonus.
       AUTHOR. Angelo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRecords ASSIGN TO "Customer.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS FS.
       DATA DIVISION.
       FILE SECTION.
       FD  CRecords.
       01  CustomerData.
           02 ACCNUM   PIC 9(4).
           02 FullName.
               03 FNAME    PIC X(10).
               03 LNAME    PIC X(10).
           02 BAL      PIC 9(7).
       WORKING-STORAGE SECTION.
      *TO MAKE SOME DELAYS IN PROCESSING
       01  GETCH            PIC X.
      *FOR FILE HANDLING AND READING
       01  WSCustomer.
           02 WSACCNUM   PIC 9(4).
           02 WSFullName.
               03 WSFNAME    PIC X(10).
               03 WSLNAME    PIC X(10).
           02 WSBAL      PIC 9(7).
      *FOR CREATING AN ACCOUNT / PARA-CREATE
       01  WS-CURRENT.
           02 CACCNUM   PIC 9(4).
           02 CFullName.
               03 CFNAME    PIC X(10).
               03 CLNAME    PIC X(10).
           02 CBAL      PIC 9(7).
      *ONLY IN NEW LINE
       01 WS-BLANK     PIC X(25) VALUE SPACES.
      *FOR PARA-USER
       01  WSUSER          PIC A.
           88 USER-A      VALUE 'A', 'a'.
           88 USER-B      VALUE 'B', 'b'.
           88 USER-C      VALUE 'C', 'c'.
           88 USER-D      VALUE 'D', 'd'.
      *FOR PARA-MENU
       01  WS-MENU         PIC A.
           88 A            VALUE 'A', 'a'.
           88 B            VALUE 'B', 'b'.
       01 QUIT         PIC 9  VALUE 0.
      *TO INDICATE IF EOF
       01  WSEOF     PIC A(1) VALUE 'N'.
      *FOR LOGIN FORM
       01  LOGIN     PIC 9  VALUE 0.
       01  SIGNIN   PIC 9(4).
      *FOR FILE STATUS  
       01  FS       PIC X(2).
      *INDICATOR FOR ACCOUNT NUMBER IN PARA-CREATE
       01  EXISTS  PIC 9  VALUE 0.
      *FOR WITHDRAWAL TRANSACTION 
       01  WS-WITHDRAW   PIC 9(7).
      *FOR DEPOSIT TRANSACTION 
       01  WS-DEPOSIT    PIC 9(7).	
       PROCEDURE DIVISION.
       MAIN.
           PERFORM PARA-MENU WITH TEST BEFORE UNTIL QUIT = 1.
           STOP RUN.
       PARA-MENU.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************************'.
           DISPLAY "*      Banque d'or BANK TRANSACTION SYSTEM       *".
           DISPLAY '*         [A]. LOGIN TO YOUR ACCOUNT             *'. 
           DISPLAY '*           [B]. CREATE AN ACCOUNT               *'.
           DISPLAY '*                [ANY]. EXIT                     *'.
           DISPLAY '**************************************************'.
           DISPLAY ' CHOOSE AN OPERATION: ' WITH NO ADVANCING.  
           
           ACCEPT WS-MENU.
           IF A
               PERFORM PARA-LOGIN
           ELSE IF B
               PERFORM PARA-CREATE
           ELSE 
               ADD 1 TO QUIT
           END-IF.

       PARA-CREATE.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY 'ENTER ACCOUNT NUMBER: ' WITH NO ADVANCING.
           ACCEPT CACCNUM.
           OPEN INPUT CRecords.
           IF FS = "35"
               CLOSE CRecords
               OPEN OUTPUT CRecords
               CLOSE CRecords
           ELSE 
               PERFORM UNTIL WSEOF = 'Y'
                   READ CRecords INTO WSCustomer
                       AT END MOVE 'Y' TO WSEOF
                       NOT AT END 
                           IF CACCNUM = WSACCNUM 
                           ADD 1 TO EXISTS
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CRecords     
           END-IF.
           IF EXISTS = 1    
               DISPLAY 'ACCOUNT NUMBER IS NOT AVAILABLE'
               PERFORM PARA-STOP
           ELSE
               OPEN EXTEND CRecords
                   MOVE CACCNUM TO ACCNUM
                   DISPLAY 'ENTER FIRST NAME: ' WITH NO ADVANCING
                   ACCEPT CFNAME
                   MOVE CFNAME TO FNAME
                   DISPLAY 'ENTER LAST NAME: ' WITH NO ADVANCING 
                   ACCEPT CLNAME
                   MOVE CLNAME TO LNAME
                   DISPLAY 'ENTER BALANCE: ' WITH NO ADVANCING
                   ACCEPT CBAL
                   MOVE CBAL TO BAL
                   WRITE CustomerData
                   END-WRITE
               CLOSE CRecords
               DISPLAY WS-BLANK
           DISPLAY '**************************************************'
           DISPLAY '*      WELCOME TO OUR BANKING SYSTEM!            *'
           DISPLAY '**************************************************'
           DISPLAY '*     ACCOUNT NUMBER: ' CACCNUM 
           '                       *'
           DISPLAY '*     ACCOUNT NAME: ' CFNAME CLNAME    '         *'
           DISPLAY '*     CURRENT BALANCE: ' CBAL'                   *'
           DISPLAY '**************************************************'
               DISPLAY 'PLEASE ENTER TO CONTINUE...' WITH NO ADVANCING
               ACCEPT GETCH
           END-IF.
           STOP RUN.   
       
       PARA-LOGIN.
           MOVE 0 TO LOGIN.
           MOVE 0 TO SIGNIN.
           DISPLAY WS-BLANK.
           DISPLAY 'ACCOUNT NUMBER: ' WITH NO ADVANCING.
           ACCEPT SIGNIN. 
           OPEN INPUT CRecords.
               PERFORM UNTIL WSEOF = 'Y'
               READ CRecords INTO WSCustomer
                       AT END MOVE 'Y' TO WSEOF
                       NOT AT END IF WSACCNUM = SIGNIN 
                           MOVE 1 TO LOGIN
                           MOVE WSCustomer TO WS-CURRENT
                           END-IF
                       END-READ
                END-PERFORM.
           CLOSE CRecords.
           MOVE 'N' TO WSEOF.
           IF LOGIN = 1
               PERFORM PARA-USER
           ELSE 
               DISPLAY 'LOGIN FAILED.'
               PERFORM PARA-STOP
           END-IF.

       PARA-USER.
           DISPLAY '**************************************************'.
           DISPLAY '*             WELCOME, ' CFNAME '!               *'.
           DISPLAY '**************************************************'.
           DISPLAY '*             [A]. CHECK BALANCE                 *'.
           DISPLAY '*                [B]. DEPOSIT                    *'.
           DISPLAY '*               [C]. WITHDRAW                    *'.
           DISPLAY '*                [D]. LOGOUT                     *'.
           DISPLAY '**************************************************'.
           DISPLAY 'PLEASE SELECT AN OPERATION:' WITH NO ADVANCING.
           ACCEPT WSUSER.
           IF USER-A
               PERFORM PARA-BALANCE
           ELSE IF USER-B
               PERFORM PARA-DEPOSIT
           ELSE IF USER-C
               PERFORM PARA-WITHDRAW
           ELSE 
               CONTINUE
           END-IF.
       PARA-BALANCE.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************************'.
           DISPLAY '* ACCOUNT NUMBER: ' CACCNUM 
           '                           *'.
           DISPLAY '* YOUR LATEST BALANCE IS ' CBAL 
           '                 *'.
           DISPLAY '**************************************************'.
           PERFORM PARA-STOP.

       PARA-DEPOSIT.
           OPEN I-O CRecords.
               PERFORM UNTIL WSEOF = 'Y'
               READ CRecords INTO WSCustomer
                AT END MOVE 'Y' TO WSEOF
                NOT AT END IF WSACCNUM = SIGNIN 
                           DISPLAY 'ENTER DEPOSIT AMOUNT: ' 
                           WITH NO ADVANCING 
                           ACCEPT WS-DEPOSIT
                           ADD WS-DEPOSIT TO WSBAL GIVING WSBAL
                           
                           MOVE WSCustomer TO CustomerData
                           REWRITE CustomerData
                           END-REWRITE
           DISPLAY '**************************************************'
           DISPLAY '*       NEW ACCOUNT BALANCE: ' WSBAL 
           '             *'
           DISPLAY '**************************************************'
                       END-IF
               END-READ
               END-PERFORM.
           CLOSE CRecords.

           PERFORM PARA-STOP.
       PARA-WITHDRAW.
           OPEN I-O CRecords.
               PERFORM UNTIL WSEOF = 'Y'
               READ CRecords INTO WSCustomer
                AT END MOVE 'Y' TO WSEOF
                NOT AT END IF WSACCNUM = SIGNIN 
                   DISPLAY 'ENTER WITHDRAWAL AMOUNT: ' 
                   WITH NO ADVANCING 
                   ACCEPT WS-WITHDRAW
                       IF WS-WITHDRAW > WSBAL 
                           DISPLAY 'INSUFFICIENT BALANCE.'
                       ELSE 
                           SUBTRACT WS-WITHDRAW FROM WSBAL GIVING WSBAL
                           MOVE WSCustomer TO CustomerData
                           REWRITE CustomerData
                           END-REWRITE
           DISPLAY '**************************************************'
           DISPLAY '*       NEW ACCOUNT BALANCE: ' WSBAL '           *'
           DISPLAY '**************************************************'
                       END-IF
                   END-IF
               END-READ
               END-PERFORM.
           CLOSE CRecords.
           PERFORM PARA-STOP.

       PARA-STOP.
           DISPLAY 'PLEASE ENTER TO CONTINUE...' WITH NO ADVANCING.
           ACCEPT GETCH.
           ADD 1 TO QUIT.
           PERFORM MAIN. 
      *END LINE OF THE PROGRAM.
