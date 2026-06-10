IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-SPERRE.
       AUTHOR.     UNSERE-NAMEN.

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
       *> Empfang der flachen Bestelldaten
       01 LK-AKTUELL-BESTELLUNG.
          05 LK-BES-KUNDENNUMMER   PIC 9(06).
          05 LK-BES-PRODUKTNUMMER  PIC 9(06).
          05 LK-BES-MENGE          PIC 9(05).

       *> Kunden-Tabelle über das verlangte Pfad-Copybook
       01 LK-KUNDEN-TABELLE.
          05 LK-KUNDEN-ANZAHL      PIC 9(03).
          05 LK-KUNDE-ELEMENT      OCCURS 100 TIMES INDEXED BY KUN-IDX.
             COPY "copybooks/kundenstamm" REPLACING 01 BY 10.

       01 LK-KUNDEN-IDX            PIC 9(04).
       01 LK-HAT-FEHLER            PIC X(02).

       PROCEDURE DIVISION USING LK-AKTUELL-BESTELLUNG 
                                LK-KUNDEN-TABELLE
                                LK-KUNDEN-IDX
                                LK-HAT-FEHLER.
       MAIN-LOGIC.
           MOVE "NEIN" TO LK-HAT-FEHLER.

           *> Zugriff auf das Statusfeld des gefundenen Kunden in der Tabelle
           IF KUN-STATUS(LK-KUNDEN-IDX) = "S"
               MOVE "JA" TO LK-HAT-FEHLER
               OPEN EXTEND FEHLER-FILE
               WRITE FEHLER-REC FROM LK-AKTUELL-BESTELLUNG
               CLOSE FEHLER-FILE
           END-IF.
           
           GOBACK.