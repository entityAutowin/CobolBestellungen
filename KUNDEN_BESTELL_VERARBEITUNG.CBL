       IDENTIFICATION DIVISION.
       PROGRAM-ID. BESTELLVERARBEITUNG.
       AUTHOR. FIEDLER-OBERZIER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KUNDENSTAMM ASSIGN TO "kunden.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRODUKTSTAMM ASSIGN TO "produkte.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BESTELLUNGEN ASSIGN TO "bestellungen.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT RECHNUNGEN ASSIGN TO "rechnungen.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FEHLER-KUNDE ASSIGN TO "fehler_kunde.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FEHLER-PRODUKT ASSIGN TO "fehler_produkt.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FEHLER-SPERRE ASSIGN TO "fehler_sperre.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  KUNDENSTAMM.
       01  KUNDEN-REC.
           05  K-KUNDENNUMMER      PIC 9(6).
           05  K-NAME              PIC X(20).
           05  K-STATUS            PIC X.
               88  K-AKTIV         VALUE 'A'.
               88  K-GESPERRT      VALUE 'S'.

       FD  PRODUKTSTAMM.
       01  PRODUKT-REC.
           05  P-PRODUKTNUMMER     PIC 9(6).
           05  P-BEZEICHNUNG       PIC X(20).
           05  P-PREIS-VOR-KOMMA   PIC 9(5).
           05  FILLER              PIC X.
           05  P-PREIS-NACH-KOMMA  PIC 9(2).

       FD  BESTELLUNGEN.
       01  BESTELL-REC.
           05  B-KUNDENNUMMER      PIC 9(6).
           05  B-PRODUKTNUMMER     PIC 9(6).
           05  B-MENGE             PIC 9(5).

       FD  RECHNUNGEN.
       01  RECHNUNG-REC.
           05  R-KUNDENNUMMER      PIC 9(6).
           05  R-PRODUKTNUMMER     PIC 9(6).
           05  R-MENGE             PIC 9(5).
           05  R-EINZELPREIS       PIC 9(5).99.
           05  R-GESAMTPREIS       PIC 9(7).99.

       FD  FEHLER-KUNDE.
       01  FK-REC                  PIC X(17).

       FD  FEHLER-PRODUKT.
       01  FP-REC                  PIC X(17).

       FD  FEHLER-SPERRE.
       01  FS-REC                  PIC X(17).

       WORKING-STORAGE SECTION.
       01  EOF-FLAGS.
           05  EOF-KUNDEN          PIC X  VALUE 'N'.
               88  STAMM-K-ENDE    VALUE 'Y'.
           05  EOF-PRODUKTE        PIC X  VALUE 'N'.
               88  STAMM-P-ENDE    VALUE 'Y'.
           05  EOF-BESTELL         PIC X  VALUE 'N'.
               88  BESTELL-ENDE    VALUE 'Y'.

       01  WS-RECHNUNG-WORK.
           05  WS-PREIS-NUM        PIC 9(5)V99  VALUE ZERO.
           05  WS-GESAMTPREIS      PIC 9(7)V99  VALUE ZERO.

       01  STATISTIK-ZAEHLER.
           05  Z-BESTELLUNGEN      PIC 9(4) VALUE ZERO.
           05  Z-RECHNUNGEN        PIC 9(4) VALUE ZERO.
           05  Z-ERR-KUNDE         PIC 9(4) VALUE ZERO.
           05  Z-ERR-PRODUKT       PIC 9(4) VALUE ZERO.
           05  Z-ERR-SPERRE        PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.
       HAUPT-PROGRAMM.
           PERFORM INITIALISIERUNG
           
           PERFORM LESE-NAECHSTE-BESTELLUNG
           PERFORM VERARBEITUNG UNTIL BESTELL-ENDE
           
           PERFORM ABGESCHLOSSEN
           STOP RUN.

       INITIALISIERUNG.
           OPEN INPUT  BESTELLUNGEN
           OPEN OUTPUT RECHNUNGEN
                       FEHLER-KUNDE
                       FEHLER-PRODUKT
                       FEHLER-SPERRE.

       VERARBEITUNG.
           IF BESTELL-REC = SPACES OR B-KUNDENNUMMER = ZERO
               PERFORM LESE-NAECHSTE-BESTELLUNG
               EXIT PARAGRAPH
           END-IF.

           ADD 1 TO Z-BESTELLUNGEN.

           OPEN INPUT KUNDENSTAMM
           MOVE 'N' TO EOF-KUNDEN
           PERFORM LESE-NAECHSTEN-KUNDEN

           PERFORM UNTIL STAMM-K-ENDE OR K-KUNDENNUMMER = B-KUNDENNUMMER
               PERFORM LESE-NAECHSTEN-KUNDEN
           END-PERFORM.

           IF STAMM-K-ENDE
               MOVE BESTELL-REC TO FK-REC
               WRITE FK-REC
               ADD 1 TO Z-ERR-KUNDE
           ELSE
               IF K-GESPERRT
                   MOVE BESTELL-REC TO FS-REC
                   WRITE FS-REC
                   ADD 1 TO Z-ERR-SPERRE
               ELSE
                   PERFORM PRUEFE-UND-VERARBEITE-PRODUKT
               END-IF
           END-IF.

           CLOSE KUNDENSTAMM.
           PERFORM LESE-NAECHSTE-BESTELLUNG.

       PRUEFE-UND-VERARBEITE-PRODUKT.
           OPEN INPUT PRODUKTSTAMM
           MOVE 'N' TO EOF-PRODUKTE
           PERFORM LESE-NAECHSTES-PRODUKT

           PERFORM UNTIL STAMM-P-ENDE OR P-PRODUKTNUMMER = B-PRODUKTNUMMER
               PERFORM LESE-NAECHSTES-PRODUKT
           END-PERFORM.

           IF STAMM-P-ENDE
               MOVE BESTELL-REC TO FP-REC
               WRITE FP-REC
               ADD 1 TO Z-ERR-PRODUKT
           ELSE
               INITIALIZE RECHNUNG-REC
               MOVE B-KUNDENNUMMER  TO R-KUNDENNUMMER
               MOVE B-PRODUKTNUMMER TO R-PRODUKTNUMMER
               MOVE B-MENGE         TO R-MENGE
               
               COMPUTE WS-PREIS-NUM = P-PREIS-VOR-KOMMA + 
                                      (P-PREIS-NACH-KOMMA / 100)
               
               MULTIPLY B-MENGE BY WS-PREIS-NUM GIVING WS-GESAMTPREIS
               
               MOVE WS-PREIS-NUM   TO R-EINZELPREIS
               MOVE WS-GESAMTPREIS TO R-GESAMTPREIS
               
               WRITE RECHNUNG-REC
               ADD 1 TO Z-RECHNUNGEN
           END-IF.

           CLOSE PRODUKTSTAMM.

       LESE-NAECHSTEN-KUNDEN.
           READ KUNDENSTAMM
               AT END MOVE 'Y' TO EOF-KUNDEN
           END-READ.

       LESE-NAECHSTES-PRODUKT.
           READ PRODUKTSTAMM
               AT END MOVE 'Y' TO EOF-PRODUKTE
           END-READ.

       LESE-NAECHSTE-BESTELLUNG.
           READ BESTELLUNGEN
               AT END MOVE 'Y' TO EOF-BESTELL
           END-READ.

       ABGESCHLOSSEN.
           CLOSE BESTELLUNGEN
                 RECHNUNGEN
                 FEHLER-KUNDE
                 FEHLER-PRODUKT
                 FEHLER-SPERRE

           DISPLAY "======================================="
           DISPLAY "      COBOL STATISTICS REPORT          "
           DISPLAY "======================================="
           DISPLAY "Anzahl verarbeiteter Bestellungen: " Z-BESTELLUNGEN
           DISPLAY "Anzahl gueltiger Rechnungen      : " Z-RECHNUNGEN
           DISPLAY "Fehler - Kunde nicht gefunden    : " Z-ERR-KUNDE
           DISPLAY "Fehler - Produkt nicht gefunden  : " Z-ERR-PRODUKT
           DISPLAY "Fehler - Kunde gesperrt          : " Z-ERR-SPERRE
           DISPLAY "=======================================".
