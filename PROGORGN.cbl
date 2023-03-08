      ******************************************************************
      * Author: Renan Cicero
      * Date: //2023
      * Purpose: Organizar por ordem alfabetica
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       
       
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  CONT1      PIC 999 VALUE 1.
       77  CONT2      PIC 999 VALUE 1.
       77  CONT3      PIC 999 VALUE 1.
       77  TEMP       PIC X(10).
       01  NOMES.
           03 NOME OCCURS 6 TIMES PIC X(10).


      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY 'INSIRA 6 NOMES'

           PERFORM LOOP WITH TEST AFTER VARYING CONT1 FROM 1 BY 1 
           UNTIL CONT1 > 5. 

           PERFORM SORT1 WITH TEST AFTER VARYING CONT1 FROM 1 BY 1 
           UNTIL CONT1 > 5.

           DISPLAY 'ORGANIZANDO EM ORDEM ALFABETICA'

           PERFORM REDISPLAY WITH TEST AFTER VARYING CONT1 FROM 1 BY 1
           UNTIL CONT1 > 5.

           STOP RUN.

       LOOP.
           DISPLAY '>' WITH NO ADVANCING 
           ACCEPT  NOME(CONT1).

       SORT1.
           PERFORM SORT2 WITH TEST AFTER VARYING CONT2 FROM 1 BY 1 
           UNTIL CONT2 > 4.

       SORT2.
           ADD 1 TO CONT2 GIVING CONT3.
           DISPLAY CONT2' - 'CONT3
           IF NOME(CONT2) > NOME(CONT3)
              MOVE NOME(CONT2) TO TEMP
              MOVE NOME(CONT3) TO NOME(CONT2)
              MOVE TEMP TO NOME(CONT3)
           END-IF
           .
       
       REDISPLAY.
           DISPLAY NOME(CONT1)
           .


       END PROGRAM PROG.
