       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       AUTHOR. FIEDLER-OBERZIER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BESTELL-STRUKTUR.
           05  IDX-BESTELL          PIC 9(03) COMP VALUE 0.
           05  BES-BESTELLUNG-REC   OCCURS 100 TIMES.
               COPY "copybooks/bestellungen".

       01  KUNDEN-STRUKTUR.
           05  IDX-KUNDEN           PIC 9(03) COMP VALUE 0.
           05  KUN-KUNDENSTAMM-REC  OCCURS 100 TIMES.
               COPY "copybooks/kundenstamm".

       01  PRODUKT-STRUKTUR.
           05  IDX-PRODUKT          PIC 9(03) COMP VALUE 0.
           05  PRO-PRODUKTSTAMM-REC OCCURS 100 TIMES.
               COPY "copybooks/produktstamm".

       01  WS-LOOP-IDX              PIC 9(03) COMP.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL "READ-DATA" USING 
               BES-BESTELLUNG-REC(1)
               KUN-KUNDENSTAMM-REC(1)
               PRO-PRODUKTSTAMM-REC(1)
               IDX-BESTELL
               IDX-KUNDEN
               IDX-PRODUKT.

           DISPLAY "Data loaded successfully."
           DISPLAY "Orders read:   " IDX-BESTELL
           DISPLAY "Customers read:" IDX-KUNDEN
           DISPLAY "Products read: " IDX-PRODUKT
           
           IF IDX-BESTELL > 0
               PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1
                         UNTIL WS-LOOP-IDX > IDX-BESTELL
                   CALL "VERARBEITUNG" USING
                       KUNDEN-STRUKTUR
                       PRODUKT-STRUKTUR
                       BES-BESTELLUNG-REC(WS-LOOP-IDX)
               END-PERFORM
               DISPLAY "Processing completed."
           ELSE
               DISPLAY "No orders to process."
           END-IF.

           STOP RUN.
