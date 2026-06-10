IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-SPERRE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEHLER-FILE ASSIGN TO "data/fehler_sperre.dat"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FEHLER-FILE.
       01 FEHLER-REC               PIC X(17).

       LINKAGE SECTION.
           COPY bestellung.
           COPY kunde.
       01 LK-KUNDEN-IDX            PIC 9(04).
       01 LK-HAT-FEHLER            PIC X(02).

       PROCEDURE DIVISION USING BES-BESTELLUNG-REC 
                                KUN-KUNDENSTAMM-REC
                                LK-KUNDEN-IDX
                                LK-HAT-FEHLER.
       MAIN-LOGIC.
           MOVE "NEIN" TO LK-HAT-FEHLER.

           *> Prüfung des Status an der indizierten Stelle
           IF KUN-STATUS(LK-KUNDEN-IDX) = "S"
               MOVE "JA" TO LK-HAT-FEHLER
               
               OPEN EXTEND FEHLER-FILE
               WRITE FEHLER-REC FROM BES-BESTELLUNG-REC
               CLOSE FEHLER-FILE
           END-IF.
           
           GOBACK.