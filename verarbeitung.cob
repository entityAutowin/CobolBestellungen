       IDENTIFICATION DIVISION.
       PROGRAM-ID. VERARBEITUNG.
       AUTHOR.     UNSERE-NAMEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECHNUNG-FILE ASSIGN TO "data/rechnungen.dat"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD RECHNUNG-FILE.
       01 RECH-OUTPUT-REC          PIC X(43).

       WORKING-STORAGE SECTION.
       01 WS-RECHNUNG-REC.
          05 RECH-KUNDENNUMMER     PIC 9(06).
          05 RECH-PRODUKTNUMMER    PIC 9(06).
          05 RECH-MENGE            PIC 9(05).
          05 RECH-EINZELPREIS      PIC S9(05)V99.
          05 RECH-GESAMTPREIS      PIC S9(07)V99.

       01 WS-GEFUNDEN-IDX          PIC 9(04).
       01 WS-KUNDEN-IDX            PIC 9(04).
       01 WS-PRODUKT-IDX           PIC 9(04).
       01 WS-FEHLER-KUNDE          PIC X(02).
       01 WS-FEHLER-SPERRE         PIC X(02).
       01 WS-FEHLER-PRODUKT        PIC X(02).

       LINKAGE SECTION.
       01 LK-KUNDEN-TABELLE.
          05 LK-KUNDEN-ANZAHL      PIC 9(03) COMP.
          05 LK-KUNDE-ELEMENT      OCCURS 100 TIMES INDEXED BY KUN-IDX.
             COPY "copybooks/kundenstamm".

       01 LK-PRODUKT-TABELLE.
          05 LK-PRODUKT-ANZAHL     PIC 9(03) COMP.
          05 LK-PRODUKT-ELEMENT    OCCURS 100 TIMES INDEXED BY PRO-IDX.
             COPY "copybooks/produktstamm".

       *> Struktur für die flache Übergabe der aktuellen Bestellung
       01 LK-AKTUELL-BESTELLUNG.
          05 LK-BES-KUNDENNUMMER   PIC 9(06).
          05 LK-BES-PRODUKTNUMMER  PIC 9(06).
          05 LK-BES-MENGE          PIC 9(05).

       PROCEDURE DIVISION USING LK-KUNDEN-TABELLE 
                                LK-PRODUKT-TABELLE 
                                LK-AKTUELL-BESTELLUNG.
       MAIN-PROCESS.
           MOVE "NEIN" TO WS-FEHLER-KUNDE
           MOVE "NEIN" TO WS-FEHLER-SPERRE
           MOVE "NEIN" TO WS-FEHLER-PRODUKT

           *> 1. KUNDE PRÜFEN
           PERFORM SUCHE-KUNDE
           MOVE WS-GEFUNDEN-IDX TO WS-KUNDEN-IDX
           
           CALL "CHECK-KUNDE" USING LK-AKTUELL-BESTELLUNG 
                                    LK-KUNDEN-TABELLE
                                    WS-KUNDEN-IDX
                                    WS-FEHLER-KUNDE
           
           IF WS-FEHLER-KUNDE = "NEIN"
               CALL "CHECK-SPERRE" USING LK-AKTUELL-BESTELLUNG 
                                         LK-KUNDEN-TABELLE
                                         WS-KUNDEN-IDX
                                         WS-FEHLER-SPERRE
           ELSE
               MOVE "JA" TO WS-FEHLER-SPERRE
           END-IF.

           *> 2. PRODUKT PRÜFEN
           PERFORM SUCHE-PRODUKT
           MOVE WS-GEFUNDEN-IDX TO WS-PRODUKT-IDX

           CALL "CHECK-PRODUKT" USING LK-AKTUELL-BESTELLUNG 
                                      LK-PRODUKT-TABELLE
                                      WS-PRODUKT-IDX
                                      WS-FEHLER-PRODUKT.

           *> 3. RECHNUNG SCHREIBEN
           IF WS-FEHLER-KUNDE = "NEIN" AND 
              WS-FEHLER-SPERRE = "NEIN" AND 
              WS-FEHLER-PRODUKT = "NEIN"
              
              MOVE LK-BES-KUNDENNUMMER  TO RECH-KUNDENNUMMER
              MOVE LK-BES-PRODUKTNUMMER TO RECH-PRODUKTNUMMER
              MOVE LK-BES-MENGE         TO RECH-MENGE
              
              MOVE PRO-PREIS(WS-PRODUKT-IDX) TO RECH-EINZELPREIS
              
              MULTIPLY LK-BES-MENGE BY PRO-PREIS(WS-PRODUKT-IDX)
                  GIVING RECH-GESAMTPREIS
              
              OPEN EXTEND RECHNUNG-FILE
              WRITE RECH-OUTPUT-REC FROM WS-RECHNUNG-REC
              CLOSE RECHNUNG-FILE
           END-IF.

           GOBACK.

       SUCHE-KUNDE.
           MOVE 0 TO WS-GEFUNDEN-IDX.
           PERFORM VARYING KUN-IDX FROM 1 BY 1 
                     UNTIL KUN-IDX > LK-KUNDEN-ANZAHL 
                        OR WS-GEFUNDEN-IDX > 0
               IF KUN-KUNDENNUMMER(KUN-IDX) = LK-BES-KUNDENNUMMER
                   SET WS-GEFUNDEN-IDX TO KUN-IDX
               END-IF
           END-PERFORM.
           IF WS-GEFUNDEN-IDX = 0
               MOVE 999 TO WS-GEFUNDEN-IDX
           END-IF.

       SUCHE-PRODUKT.
           MOVE 0 TO WS-GEFUNDEN-IDX.
           PERFORM VARYING PRO-IDX FROM 1 BY 1 
                     UNTIL PRO-IDX > LK-PRODUKT-ANZAHL 
                        OR WS-GEFUNDEN-IDX > 0
               IF PRO-PRODUKTNUMMER(PRO-IDX) = LK-BES-PRODUKTNUMMER
                   SET WS-GEFUNDEN-IDX TO PRO-IDX
               END-IF
           END-PERFORM.
           IF WS-GEFUNDEN-IDX = 0
               MOVE 999 TO WS-GEFUNDEN-IDX
           END-IF.
           