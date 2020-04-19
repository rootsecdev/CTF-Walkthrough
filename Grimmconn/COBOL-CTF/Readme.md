# COBOL CTF Walkthrough for Grimmconn

CTF URL - https://samsclass.info/129S/COBOL.shtml

## 1. ED 200: Google Cloud Linux Server (15 pts)

**Answers:**

Google PersistentDisk

## CBL 1: Basic COBOL (160 pts)

Install COBOL:

```
sudo apt update
sudo apt install open-cobol -y
```

Create your first Hello World Cobol Program:

```
nano hello.cbl
```

Insert Code:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
DISPLAY 'Hello world!'.
STOP RUN.
```

ctrl + o to write out file in nano then ctrl + x to exit.

Compile Code:

```
cobc -free -x -o hello hello.cbl
```

Execute Code:

```
./hello
```

**Flag CBL 1.1: File Type (5 pts)**

Code:

```
file hello
```

**Answer:**

stripped

**Flag CBL 1.2: COMPUTE (10 pts)**

ANSWER:

```
IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
            01 NAME PIC A(20) VALUE 'Barf'.
            01 NUM PIC 9(5) VALUE 12345.

            01 FLAG PIC 9(10) VALUE 42467.
        
       PROCEDURE DIVISION.
           A-PARA.
              DISPLAY 'Literal string'.
              DISPLAY "NAME : "NAME.
              DISPLAY "NUM : "NUM.
              MOVE 'Barfolomew' TO NAME.
              MOVE 31337 TO NUM.
              DISPLAY "REVISED NAME : "NAME.
              DISPLAY "REVISED NUM : "NUM.

              COMPUTE FLAG = (FLAG*FLAG) - (FLAG*99/NUM).
              DISPLAY "FLAG: "FLAG.

       STOP RUN.
```
Compile Code:

```
cobc -free -x -o var2 var2.cbl
```
Run:

```
./var2
```

**FLAG**

```
1803445954
```

**Flag CBL 1.3: Server Version (10 pts)**

Code:

```
IDENTIFICATION DIVISION.  
       PROGRAM-ID. GET1.  
  
       DATA DIVISION.  
           WORKING-STORAGE SECTION.  
            01 GETLINE PIC A(20).  
            01 HOSTLINE PIC A(24).  
            01 CR PIC X VALUE X'0A'.  
  
       PROCEDURE DIVISION.  
           A-PARA.
              MOVE "GET /COBOL/ HTTP/1.1" TO GETLINE.  
              MOVE "Host: ad.samsclass.info" TO HOSTLINE.  

              DISPLAY GETLINE. 
              DISPLAY HOSTLINE. 
              DISPLAY CR. 
       STOP RUN. 
```

Compile Code:

```
cobc -free -x -o get1 get1.cbl
```

Run:

```
sudo apt install netcat -y
```

```
./get1 | nc ad.samsclass.info 80
```

**FLAG:**

```
Ubuntu
```
**Flag CBL 1.4: FLAG_ME (15 pts)**

Code:

```
IDENTIFICATION DIVISION.
       PROGRAM-ID. GET1.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
            01 GETLINE PIC A(34).
            01 HOSTLINE PIC A(24).
            01 AGENTLINE PIC A(19).
            01 CR PIC X VALUE X'0A'.

       PROCEDURE DIVISION.
           A-PARA.
              MOVE "GET /COBOL/USER_AGENT.php HTTP/1.1" TO GETLINE.
              MOVE "Host: ad.samsclass.info" TO HOSTLINE.
              MOVE "User-Agent: FLAG_ME" TO AGENTLINE.

              DISPLAY GETLINE.
              DISPLAY HOSTLINE.
              DISPLAY AGENTLINE.
              DISPLAY CR.
       STOP RUN.
```       

Compile Code:

```
cobc -free -x -o get2a get2a.cbl
```

Run:

```
./get2a | nc ad.samsclass.info 80
```

**FLAG**

```
COBOL_4_EVER
```

**Flag CBL 1.5: Total (10 pts)**

Make a loop that totals the numbers from 10 through 49.

CODE:

```
IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
            01 NUM PIC 9(2) VALUE 10.
            01 TOTAL PIC 9(10).

       PROCEDURE DIVISION.
           A-PARA.
           PERFORM B-PARA WITH TEST AFTER UNTIL NUM>49.
           STOP RUN.

           B-PARA.
           DISPLAY 'NUM : 'NUM.
           ADD 1 TO NUM.
           COMPUTE TOTAL = (NUM+TOTAL) - 1.
           DISPLAY 'TOTAL : 'TOTAL.
```

Compile:

```
cobc -free -x -o loop loop.cbl
```

Run:

```
.\loop
```

**FLAG:**

```
0000001180
```

**Flag CBL 1.6: Fib(100) (15 pts)**

Create a COBOL program to calculate Fibonacci numbers, as shown below. Find the 100th value.

Code: (Credit goes to Rosetta Code). Enter 99 for answer when running.

```
Program-ID. Fibonacci-Sequence.
Data Division.
Working-Storage Section.
  01  FIBONACCI-PROCESSING.
    05  FIBONACCI-NUMBER  PIC 9(36)   VALUE 0.
    05  FIB-ONE           PIC 9(36)   VALUE 0.
    05  FIB-TWO           PIC 9(36)   VALUE 1.
  01  DESIRED-COUNT       PIC 9(4).
  01  FORMATTING.
    05  INTERM-RESULT     PIC Z(35)9.
    05  FORMATTED-RESULT  PIC X(36).
    05  FORMATTED-SPACE   PIC x(35).
Procedure Division.
  000-START-PROGRAM.
    Display "What place of the Fibonacci Sequence would you like (<173)? " with no advancing.
    Accept DESIRED-COUNT.
    If DESIRED-COUNT is less than 1
      Stop run.
    If DESIRED-COUNT is less than 2
      Move FIBONACCI-NUMBER to INTERM-RESULT
      Move INTERM-RESULT to FORMATTED-RESULT
      Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT
      Display FORMATTED-RESULT
      Stop run.
    Subtract 1 from DESIRED-COUNT.
    Move FIBONACCI-NUMBER to INTERM-RESULT.
    Move INTERM-RESULT to FORMATTED-RESULT.
    Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT.
    Display FORMATTED-RESULT.
    Perform 100-COMPUTE-FIBONACCI until DESIRED-COUNT = zero.
    Stop run.
  100-COMPUTE-FIBONACCI.
    Compute FIBONACCI-NUMBER = FIB-ONE + FIB-TWO.
    Move FIB-TWO to FIB-ONE.
    Move FIBONACCI-NUMBER to FIB-TWO.
    Subtract 1 from DESIRED-COUNT.
    Move FIBONACCI-NUMBER to INTERM-RESULT.
    Move INTERM-RESULT to FORMATTED-RESULT.
    Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT.
    Display FORMATTED-RESULT.
```

Compile:

```
cobc -free -x -o fib fib.cbl
```

Run:

```
./fib
```

**FLAG:**

```
218922995834555169026
```
