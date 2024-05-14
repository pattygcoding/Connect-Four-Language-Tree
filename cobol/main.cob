       IDENTIFICATION DIVISION.
       PROGRAM-ID. main.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BOARD.
           05  ROW PIC X(7) OCCURS 6 TIMES.
       01  PLAYER-X PIC X VALUE 'X'.
       01  PLAYER-O PIC X VALUE 'O'.
       01  CURRENT-PLAYER PIC X.
       01  INPUT-COLUMN PIC 9.
       01  VALID-MOVE PIC X VALUE 'N'.
       01  WINNER PIC X VALUE ' '.
       01  BOARD-FULL PIC X VALUE 'N'.
       01  I PIC 9.
       01  J PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-BOARD
           MOVE PLAYER-X TO CURRENT-PLAYER
           PERFORM UNTIL WINNER NOT = ' ' OR BOARD-FULL = 'Y'
               PERFORM DISPLAY-BOARD
               PERFORM GET-PLAYER-MOVE
               PERFORM MAKE-MOVE
               PERFORM CHECK-FOR-WINNER
               IF WINNER = ' ' THEN
                   PERFORM SWITCH-PLAYER
               END-IF
               PERFORM CHECK-BOARD-FULL
           END-PERFORM
           PERFORM DISPLAY-BOARD
           IF WINNER NOT = ' ' THEN
               DISPLAY "Player " CURRENT-PLAYER " wins!"
           ELSE
               DISPLAY "The game is a draw."
           END-IF
           STOP RUN.

       INITIALIZE-BOARD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   MOVE ' ' TO BOARD (I:J)
               END-PERFORM
           END-PERFORM.

       DISPLAY-BOARD.
           DISPLAY " 1 2 3 4 5 6 7"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               DISPLAY "---------------"
               DISPLAY BOARD (I:1) " | " BOARD (I:2) " | " 
                       BOARD (I:3) " | " BOARD (I:4) " | " 
                       BOARD (I:5) " | " BOARD (I:6) " | " 
                       BOARD (I:7)
           END-PERFORM
           DISPLAY "---------------".

       GET-PLAYER-MOVE.
           MOVE 'N' TO VALID-MOVE
           PERFORM UNTIL VALID-MOVE = 'Y'
               DISPLAY "Player " CURRENT-PLAYER ", choose a column (1-7):"
               ACCEPT INPUT-COLUMN
               IF INPUT-COLUMN >= 1 AND INPUT-COLUMN <= 7 THEN
                   PERFORM CHECK-COLUMN-FULL
                   IF VALID-MOVE = 'N' THEN
                       DISPLAY "Column is full. Try another column."
                   END-IF
               ELSE
                   DISPLAY "Invalid column. Enter a number between 1 and 7."
               END-IF
           END-PERFORM.

       CHECK-COLUMN-FULL.
           MOVE 'Y' TO VALID-MOVE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               IF BOARD (I:INPUT-COLUMN) = ' ' THEN
                   MOVE 'N' TO VALID-MOVE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       MAKE-MOVE.
           PERFORM VARYING I FROM 6 BY -1 UNTIL I < 1
               IF BOARD (I:INPUT-COLUMN) = ' ' THEN
                   MOVE CURRENT-PLAYER TO BOARD (I:INPUT-COLUMN)
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SWITCH-PLAYER.
           IF CURRENT-PLAYER = PLAYER-X THEN
               MOVE PLAYER-O TO CURRENT-PLAYER
           ELSE
               MOVE PLAYER-X TO CURRENT-PLAYER
           END-IF.

       CHECK-FOR-WINNER.
           PERFORM CHECK-HORIZONTAL
           PERFORM CHECK-VERTICAL
           PERFORM CHECK-DIAGONAL.

       CHECK-HORIZONTAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                   IF BOARD (I:J) = CURRENT-PLAYER AND
                      BOARD (I:J + 1) = CURRENT-PLAYER AND
                      BOARD (I:J + 2) = CURRENT-PLAYER AND
                      BOARD (I:J + 3) = CURRENT-PLAYER THEN
                       MOVE CURRENT-PLAYER TO WINNER
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-VERTICAL.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                   IF BOARD (I:J) = CURRENT-PLAYER AND
                      BOARD (I + 1:J) = CURRENT-PLAYER AND
                      BOARD (I + 2:J) = CURRENT-PLAYER AND
                      BOARD (I + 3:J) = CURRENT-PLAYER THEN
                       MOVE CURRENT-PLAYER TO WINNER
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-DIAGONAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                   IF BOARD (I:J) = CURRENT-PLAYER AND
                      BOARD (I + 1:J + 1) = CURRENT-PLAYER AND
                      BOARD (I + 2:J + 2) = CURRENT-PLAYER AND
                      BOARD (I + 3:J + 3) = CURRENT-PLAYER THEN
                       MOVE CURRENT-PLAYER TO WINNER
                   END-IF
               END-PERFORM
           END-PERFORM
           PERFORM VARYING I FROM 4 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                   IF BOARD (I:J) = CURRENT-PLAYER AND
                      BOARD (I - 1:J + 1) = CURRENT-PLAYER AND
                      BOARD (I - 2:J + 2) = CURRENT-PLAYER AND
                      BOARD (I - 3:J + 3) = CURRENT-PLAYER THEN
                       MOVE CURRENT-PLAYER TO WINNER
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-BOARD-FULL.
           MOVE 'Y' TO BOARD-FULL
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   IF BOARD (I:J) = ' ' THEN
                       MOVE 'N' TO BOARD-FULL
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF BOARD-FULL = 'N' THEN
                   EXIT PERFORM
               END-IF
           END-PERFORM.
