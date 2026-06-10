IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-PRODUKT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEHLER-FILE ASSIGN TO "data/fehler_produkt.dat"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FEHLER-FILE.
       01 FEHLER-REC               PIC X(17).

       LINKAGE SECTION.
           COPY bestellung.
           COPY produkt.
       01 LK-PRODUKT-IDX           PIC 9(04).
       01 LK-HAT-FEHLER            PIC X(02).

       PROCEDURE DIVISION USING BES-BESTELLUNG-REC 
                                PRO-PRODUKTSTAMM-REC
                                LK-PRODUKT-IDX
                                LK-HAT-FEHLER.
       MAIN-LOGIC.
           MOVE "NEIN" TO LK-HAT-FEHLER.

           IF BES-PRODUKTNUMMER NOT = PRO-PRODUKTNUMMER(LK-PRODUKT-IDX)
               MOVE "JA" TO LK-HAT-FEHLER
               
               OPEN EXTEND FEHLER-FILE
               WRITE FEHLER-REC FROM BES-BESTELLUNG-REC
               CLOSE FEHLER-FILE
           END-IF.
           
           GOBACK.