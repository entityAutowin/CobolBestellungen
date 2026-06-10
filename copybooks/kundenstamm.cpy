      *> Copybook für den Kundenstamm (Länge: 27 Zeichen)
       01 KUN-KUNDENSTAMM-REC occurs 100 times.
          05 KUN-KUNDENNUMMER      PIC 9(06).
          05 KUN-NAME              PIC X(20).
          05 KUN-STATUS            PIC X(01).
             88 KUN-STATUS-AKTIV     VALUE "A".
             88 KUN-STATUS-GESPERRT  VALUE "S".
             