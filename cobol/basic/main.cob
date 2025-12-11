       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILLIONROWCHALLENGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEASUREMENT-FILE ASSIGN TO "measurements.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "outputs.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MEASUREMENT-FILE.
       01  MEASUREMENT-RECORD    PIC X(256).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD         PIC X(10000).

       WORKING-STORAGE SECTION.
       01  WS-CITY-TABLE.
           05  WS-CITY-ENTRY OCCURS 50000 TIMES.
               10  WS-CITY-NAME      PIC X(256).
               10  WS-MIN-VAL        PIC 9(10).
               10  WS-MAX-VAL        PIC 9(10).
               10  WS-TOTAL-VAL      PIC 9(18).
               10  WS-COUNT-VAL      PIC 9(10).

       01  WS-ARRAY-SIZE         PIC 9(10) VALUE 0.
       01  WS-CURRENT-LINE       PIC X(256).
       01  WS-CITY-NAME-INPUT    PIC X(256).
       01  WS-MEASUREMENT-STR    PIC X(20).
       01  WS-MEASUREMENT        PIC 9(10).

       01  WS-EOF-FLAG           VALUE "N" PIC X(1).
       01  WS-SEMICOLON-POS      PIC 9(3).
       01  WS-LINE-LENGTH        PIC 9(3).

       01  WS-SEARCH-IDX         PIC 9(10).
       01  WS-FOUND              PIC 9 VALUE 0.
       01  WS-I                  PIC 9(10).
       01  WS-J                  PIC 9(10).
       01  WS-SORT-IDX           PIC 9(10).

       01  WS-TEMP-CITY          PIC X(256).
       01  WS-TEMP-MIN           PIC 9(10).
       01  WS-TEMP-MAX           PIC 9(10).
       01  WS-TEMP-TOTAL         PIC 9(18).
       01  WS-TEMP-COUNT         PIC 9(10).

       01  WS-AVG                PIC 9(10).
       01  WS-AVG-STR            PIC X(20).
       01  WS-MIN-STR            PIC X(20).
       01  WS-MAX-STR            PIC X(20).
       01  WS-TOTAL-STR          PIC X(20).
       01  WS-COUNT-STR          PIC X(20).

       01  WS-OUTPUT-LINE        PIC X(1024).
       01  WS-RESULT-OUTPUT      PIC X(100000).

       01  WS-START-TIME         PIC 9(18).
       01  WS-END-TIME           PIC 9(18).
       01  WS-ELAPSED-MS         PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM PROCESS-FILE.
           PERFORM SORT-CITIES.
           PERFORM BUILD-OUTPUT.
           PERFORM WRITE-OUTPUT.
           DISPLAY "Done".
           STOP RUN.

       PROCESS-FILE.
           OPEN INPUT MEASUREMENT-FILE.

           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ MEASUREMENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       MOVE MEASUREMENT-RECORD TO WS-CURRENT-LINE
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.

           CLOSE MEASUREMENT-FILE.

       PROCESS-LINE.
           MOVE 0 TO WS-LINE-LENGTH.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 256 OR WS-CURRENT-LINE(WS-I:1) = SPACE
               MOVE WS-I TO WS-LINE-LENGTH
           END-PERFORM.

           IF WS-LINE-LENGTH = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE 0 TO WS-SEMICOLON-POS.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LINE-LENGTH
               IF WS-CURRENT-LINE(WS-I:1) = ";"
                   MOVE WS-I TO WS-SEMICOLON-POS
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           IF WS-SEMICOLON-POS = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CURRENT-LINE(1:WS-SEMICOLON-POS - 1)
               TO WS-CITY-NAME-INPUT.

           MOVE WS-CURRENT-LINE(WS-SEMICOLON-POS + 1:)
               TO WS-MEASUREMENT-STR.

           MOVE FUNCTION NUMVAL(WS-MEASUREMENT-STR)
               TO WS-MEASUREMENT.

           PERFORM UPDATE-STATISTICS.

       UPDATE-STATISTICS.
           MOVE 0 TO WS-FOUND.

           PERFORM VARYING WS-SEARCH-IDX FROM 1 BY 1
               UNTIL WS-SEARCH-IDX > WS-ARRAY-SIZE
                   OR WS-FOUND = 1
               IF WS-CITY-NAME(WS-SEARCH-IDX) = WS-CITY-NAME-INPUT
                   MOVE 1 TO WS-FOUND
                   IF WS-MEASUREMENT < WS-MIN-VAL(WS-SEARCH-IDX)
                       MOVE WS-MEASUREMENT
                           TO WS-MIN-VAL(WS-SEARCH-IDX)
                   END-IF
                   IF WS-MEASUREMENT > WS-MAX-VAL(WS-SEARCH-IDX)
                       MOVE WS-MEASUREMENT
                           TO WS-MAX-VAL(WS-SEARCH-IDX)
                   END-IF
                   ADD WS-MEASUREMENT
                       TO WS-TOTAL-VAL(WS-SEARCH-IDX)
                   ADD 1 TO WS-COUNT-VAL(WS-SEARCH-IDX)
               END-IF
           END-PERFORM.

           IF WS-FOUND = 0
               ADD 1 TO WS-ARRAY-SIZE
               IF WS-ARRAY-SIZE <= 50000
                   MOVE WS-CITY-NAME-INPUT
                       TO WS-CITY-NAME(WS-ARRAY-SIZE)
                   MOVE WS-MEASUREMENT
                       TO WS-MIN-VAL(WS-ARRAY-SIZE)
                   MOVE WS-MEASUREMENT
                       TO WS-MAX-VAL(WS-ARRAY-SIZE)
                   MOVE WS-MEASUREMENT
                       TO WS-TOTAL-VAL(WS-ARRAY-SIZE)
                   MOVE 1 TO WS-COUNT-VAL(WS-ARRAY-SIZE)
               ELSE
                   DISPLAY "ERROR: Array size exceeded"
                   STOP RUN
               END-IF
           END-IF.

       SORT-CITIES.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I >= WS-ARRAY-SIZE
               PERFORM VARYING WS-J FROM WS-I BY 1
                   UNTIL WS-J > WS-ARRAY-SIZE
                   IF WS-CITY-NAME(WS-I) > WS-CITY-NAME(WS-J)
                       MOVE WS-CITY-NAME(WS-I) TO WS-TEMP-CITY
                       MOVE WS-MIN-VAL(WS-I) TO WS-TEMP-MIN
                       MOVE WS-MAX-VAL(WS-I) TO WS-TEMP-MAX
                       MOVE WS-TOTAL-VAL(WS-I) TO WS-TEMP-TOTAL
                       MOVE WS-COUNT-VAL(WS-I) TO WS-TEMP-COUNT

                       MOVE WS-CITY-NAME(WS-J) TO WS-CITY-NAME(WS-I)
                       MOVE WS-MIN-VAL(WS-J) TO WS-MIN-VAL(WS-I)
                       MOVE WS-MAX-VAL(WS-J) TO WS-MAX-VAL(WS-I)
                       MOVE WS-TOTAL-VAL(WS-J) TO WS-TOTAL-VAL(WS-I)
                       MOVE WS-COUNT-VAL(WS-J) TO WS-COUNT-VAL(WS-I)

                       MOVE WS-TEMP-CITY TO WS-CITY-NAME(WS-J)
                       MOVE WS-TEMP-MIN TO WS-MIN-VAL(WS-J)
                       MOVE WS-TEMP-MAX TO WS-MAX-VAL(WS-J)
                       MOVE WS-TEMP-TOTAL TO WS-TOTAL-VAL(WS-J)
                       MOVE WS-TEMP-COUNT TO WS-COUNT-VAL(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       BUILD-OUTPUT.
           MOVE SPACES TO WS-RESULT-OUTPUT.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ARRAY-SIZE
               DIVIDE WS-TOTAL-VAL(WS-I) BY WS-COUNT-VAL(WS-I)
                   GIVING WS-AVG

               MOVE WS-MIN-VAL(WS-I) TO WS-MIN-STR
               MOVE WS-MAX-VAL(WS-I) TO WS-MAX-STR
               MOVE WS-AVG TO WS-AVG-STR
               MOVE WS-TOTAL-VAL(WS-I) TO WS-TOTAL-STR
               MOVE WS-COUNT-VAL(WS-I) TO WS-COUNT-STR

               STRING
                   WS-CITY-NAME(WS-I) DELIMITED BY SPACE
                   "=" DELIMITED BY SIZE
                   WS-MIN-STR DELIMITED BY SPACE
                   ";" DELIMITED BY SIZE
                   WS-MAX-STR DELIMITED BY SPACE
                   ";" DELIMITED BY SIZE
                   WS-AVG-STR DELIMITED BY SPACE
                   "(" DELIMITED BY SIZE
                   WS-TOTAL-STR DELIMITED BY SPACE
                   "/" DELIMITED BY SIZE
                   WS-COUNT-STR DELIMITED BY SPACE
                   ")" DELIMITED BY SIZE
                   INTO WS-OUTPUT-LINE
               END-STRING

               STRING
                   WS-RESULT-OUTPUT DELIMITED BY SPACE
                   WS-OUTPUT-LINE DELIMITED BY SPACE
                   X"0A" DELIMITED BY SIZE
                   INTO WS-RESULT-OUTPUT
               END-STRING
           END-PERFORM.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE WS-RESULT-OUTPUT TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           CLOSE OUTPUT-FILE.
