       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       AUTHOR. FIEDLER-OBERZIER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "copybooks/bestellungen".
       COPY "copybooks/kundenstamm".
       COPY "copybooks/produktstamm".

       01  WS-COUNTERS.
           05  IDX-BESTELL          PIC 9(03) COMP VALUE 0.
           05  IDX-KUNDEN           PIC 9(03) COMP VALUE 0.
           05  IDX-PRODUKT          PIC 9(03) COMP VALUE 0.

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
           
           STOP RUN.
