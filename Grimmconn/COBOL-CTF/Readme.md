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

