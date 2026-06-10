      *> Copybook für den Kundenstamm (Länge: 28 Zeichen)
          10 KUN-KUNDENNUMMER      PIC 9(06).
          10 KUN-NAME              PIC X(20).
          10 KUN-STATUS            PIC X(01).
             88 KUN-STATUS-AKTIV     VALUE "A".
             88 KUN-STATUS-GESPERRT  VALUE "S".
          10 KUN-FILLER            PIC X(01).
