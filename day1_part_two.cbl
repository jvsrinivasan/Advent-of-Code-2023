IDENTIFICATION DIVISION.
PROGRAM-ID. CALIBRATION.
AUTHOR. SRINIVASAN-JV.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INFILE ASSIGN TO '/uploads/Dec1a2023.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
    FD INFILE.
    01 INPUT-REC.
        05 W-INPUT              PIC X(80).
    WORKING-STORAGE SECTION.
    01 WS-WORK-VARIABLES.
        05 WS-CALIB-COUNTER     PIC 9(6) VALUE 0.
        05 WS-EOF               PIC A.
        05 WS-IS-NUMERIC-FLAG   PIC A VALUE ' '.
        05 WS-IS-LETTER-FLAG    PIC A VALUE ' '.
        05 WS-INPUT             PIC X(80).
        05 WS-REVERSE-INPUT     PIC X(80).
        05 WS-COUNT             PIC 9(2) VALUE 0.
        05 WS-ACTUAL-LENGTH     PIC 9(2) VALUE 0.
        05 WS-I                 PIC 9(3) VALUE 0.
PROCEDURE DIVISION.
    OPEN INPUT INFILE.
    PERFORM UNTIL WS-EOF = 'Y'
        READ INFILE INTO WS-INPUT
        AT END MOVE 'Y' TO WS-EOF
        NOT AT END
        PERFORM 100-CALC-RTN
        END-READ
    END-PERFORM.
    CLOSE INFILE.
    DISPLAY 'TOTAL CALIBRATION VALUE: ' WS-CALIB-COUNTER.
    STOP RUN.

    100-CALC-RTN.
        INSPECT FUNCTION REVERSE(WS-INPUT) TALLYING WS-COUNT FOR LEADING SPACE
        SUBTRACT WS-COUNT FROM FUNCTION LENGTH(WS-INPUT) GIVING WS-ACTUAL-LENGTH
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACTUAL-LENGTH OR WS-IS-NUMERIC-FLAG = 'Y' OR WS-IS-LETTER-FLAG = 'Y'
        EVALUATE WS-INPUT(WS-I:1)
            WHEN '1'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '2'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '3'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '4'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '5'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '6'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '7'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '8'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '9'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (FUNCTION NUMVAL(WS-INPUT(WS-I:1)) * 10)
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN 'o'
                IF WS-INPUT(WS-I:3) = 'one'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (1 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 't'
                IF WS-INPUT(WS-I:3) = 'two'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (2 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                ELSE
                    IF WS-INPUT(WS-I:5) = 'three'
                        COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (3 * 10)
                        MOVE 'Y' TO WS-IS-LETTER-FLAG  
                    END-IF
                END-IF
            WHEN 'f'
                IF WS-INPUT(WS-I:4) = 'four'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (4 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                ELSE
                    IF WS-INPUT(WS-I:4) = 'five'
                        COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (5 * 10)
                        MOVE 'Y' TO WS-IS-LETTER-FLAG
                    END-IF
                END-IF
            WHEN 's'
                IF WS-INPUT(WS-I:3) = 'six'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (6 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                ELSE
                    IF WS-INPUT(WS-I:5) = 'seven'
                        COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (7 * 10)
                        MOVE 'Y' TO WS-IS-LETTER-FLAG   
                    END-IF
                END-IF
            WHEN 'e'
                IF WS-INPUT(WS-I:5) = 'eight'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (8 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 'n'
                IF WS-INPUT(WS-I:4) = 'nine'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + (9 * 10)
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
        END-EVALUATE
        END-PERFORM
        
        MOVE ' ' TO WS-IS-NUMERIC-FLAG
        MOVE ' ' TO WS-IS-LETTER-FLAG
        MOVE FUNCTION REVERSE(WS-INPUT(1:WS-ACTUAL-LENGTH)) TO WS-REVERSE-INPUT
        
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACTUAL-LENGTH OR WS-IS-NUMERIC-FLAG = 'Y' OR WS-IS-LETTER-FLAG = 'Y'
        EVALUATE WS-REVERSE-INPUT(WS-I:1)
            WHEN '1'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '2'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '3'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '4'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '5'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '6'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '7'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '8'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN '9'
                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + FUNCTION NUMVAL(WS-REVERSE-INPUT(WS-I:1))
                MOVE 'Y' TO WS-IS-NUMERIC-FLAG
            WHEN 'e'
                IF WS-REVERSE-INPUT(WS-I:3) = 'eno'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 1
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                ELSE
                    IF WS-REVERSE-INPUT(WS-I:5) = 'eerht'
                        COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 3
                        MOVE 'Y' TO WS-IS-LETTER-FLAG
                    ELSE
                        IF WS-REVERSE-INPUT(WS-I:4) = 'evif'
                            COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 5
                            MOVE 'Y' TO WS-IS-LETTER-FLAG
                        ELSE
                            IF WS-REVERSE-INPUT(WS-I:4) = 'enin'
                                COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 9
                                MOVE 'Y' TO WS-IS-LETTER-FLAG
                            END-IF
                        END-IF
                    END-IF
                END-IF
            WHEN 'o'
                IF WS-REVERSE-INPUT(WS-I:3) = 'owt'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 2
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 'r'
                IF WS-REVERSE-INPUT(WS-I:4) = 'ruof'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 4
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 'x'
                IF WS-REVERSE-INPUT(WS-I:3) = 'xis'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 6
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 'n'
                IF WS-REVERSE-INPUT(WS-I:5) = 'neves'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 7
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
            WHEN 't'
                IF WS-REVERSE-INPUT(WS-I:5) = 'thgie'
                    COMPUTE WS-CALIB-COUNTER = WS-CALIB-COUNTER + 8
                    MOVE 'Y' TO WS-IS-LETTER-FLAG
                END-IF
        END-EVALUATE
        END-PERFORM
        INITIALIZE  WS-INPUT
                    WS-REVERSE-INPUT
                    WS-ACTUAL-LENGTH
                    WS-IS-NUMERIC-FLAG
                    WS-IS-LETTER-FLAG
                    WS-COUNT.
                    
