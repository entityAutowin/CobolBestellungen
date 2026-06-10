       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-DATA.
       AUTHOR. FIEDLER-OBERZIER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BESTELL-FILE ASSIGN TO "data/bestellungen.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BESTELL-STATUS.
           SELECT KUNDEN-FILE ASSIGN TO "data/kunden.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KUNDEN-STATUS.
           SELECT PRODUKT-FILE ASSIGN TO "data/produkte.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PRODUKT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BESTELL-FILE.
       01  BUF-BESTELLUNG       PIC X(18).
       FD  KUNDEN-FILE.
       01  BUF-KUNDENSTAMM      PIC X(28).
       FD  PRODUKT-FILE.
       01  BUF-PRODUKTSTAMM     PIC X(34).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS-FIELDS.
           05  WS-BESTELL-STATUS    PIC X(02).
           05  WS-KUNDEN-STATUS     PIC X(02).
           05  WS-PRODUKT-STATUS    PIC X(02).
       01  WS-EOF-FLAGS.
           05  WS-EOF-BESTELL       PIC X(01) VALUE "N".
               88  EOF-BESTELL               VALUE "Y".
           05  WS-EOF-KUNDEN        PIC X(01) VALUE "N".
               88  EOF-KUNDEN                VALUE "Y".
           05  WS-EOF-PRODUKT       PIC X(01) VALUE "N".
               88  EOF-PRODUKT               VALUE "Y".

       LINKAGE SECTION.
       01  LK-BESTELLUNGEN.
           05  LK-BES-REC           PIC X(17) OCCURS 100 TIMES.
       01  LK-KUNDENSTAMM.
           05  LK-KUN-REC           PIC X(27) OCCURS 100 TIMES.
       01  LK-PRODUKTSTAMM.
           05  LK-PRO-REC           PIC X(33) OCCURS 100 TIMES.

       01  LK-IDX-BESTELL           PIC 9(03) COMP.
       01  LK-IDX-KUNDEN            PIC 9(03) COMP.
       01  LK-IDX-PRODUKT           PIC 9(03) COMP.

       PROCEDURE DIVISION USING LK-BESTELLUNGEN
                                 LK-KUNDENSTAMM
                                 LK-PRODUKTSTAMM
                                 LK-IDX-BESTELL
                                 LK-IDX-KUNDEN
                                 LK-IDX-PRODUKT.

       100-READ-BESTELLUNGEN.
           OPEN INPUT BESTELL-FILE
           IF WS-BESTELL-STATUS NOT = "00"
               DISPLAY "ERROR OPENING BESTELL-FILE: " 
                       WS-BESTELL-STATUS
               PERFORM 999-ERROR-EXIT
           END-IF

           INITIALIZE LK-IDX-BESTELL
           MOVE "N" TO WS-EOF-BESTELL
           
           PERFORM UNTIL EOF-BESTELL OR LK-IDX-BESTELL >= 100
               READ BESTELL-FILE
                   AT END
                       SET EOF-BESTELL TO TRUE
                   NOT AT END
                       IF WS-BESTELL-STATUS = "00" OR "04"
                          ADD 1 TO LK-IDX-BESTELL
                          MOVE BUF-BESTELLUNG TO 
                               LK-BES-REC (LK-IDX-BESTELL)
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE BESTELL-FILE.

       200-READ-KUNDENSTAMM.
           OPEN INPUT KUNDEN-FILE
           IF WS-KUNDEN-STATUS NOT = "00"
               DISPLAY "ERROR OPENING KUNDEN-FILE: " 
                       WS-KUNDEN-STATUS
               PERFORM 999-ERROR-EXIT
           END-IF

           INITIALIZE LK-IDX-KUNDEN
           MOVE "N" TO WS-EOF-KUNDEN
           
           PERFORM UNTIL EOF-KUNDEN OR LK-IDX-KUNDEN >= 100
               READ KUNDEN-FILE
                   AT END
                       SET EOF-KUNDEN TO TRUE
                   NOT AT END
                       IF WS-KUNDEN-STATUS = "00" OR "04"
                          ADD 1 TO LK-IDX-KUNDEN
                          MOVE BUF-KUNDENSTAMM TO 
                               LK-KUN-REC (LK-IDX-KUNDEN)
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE KUNDEN-FILE.

       300-READ-PRODUKTSTAMM.
           OPEN INPUT PRODUKT-FILE
           IF WS-PRODUKT-STATUS NOT = "00"
               DISPLAY "ERROR OPENING PRODUKT-FILE: " 
                       WS-PRODUKT-STATUS
               PERFORM 999-ERROR-EXIT
           END-IF

           INITIALIZE LK-IDX-PRODUKT
           MOVE "N" TO WS-EOF-PRODUKT
           
           PERFORM UNTIL EOF-PRODUKT OR LK-IDX-PRODUKT >= 100
               READ PRODUKT-FILE
                   AT END
                       SET EOF-PRODUKT TO TRUE
                   NOT AT END
                       IF WS-PRODUKT-STATUS = "00" OR "04"
                          ADD 1 TO LK-IDX-PRODUKT
                          MOVE BUF-PRODUKTSTAMM TO 
                               LK-PRO-REC (LK-IDX-PRODUKT)
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE PRODUKT-FILE
           GOBACK.

       999-ERROR-EXIT.
           DISPLAY "Program terminating due to file error."
           STOP RUN.
