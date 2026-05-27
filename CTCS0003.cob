      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0003.
      *------------------------------------------------------------------------
      * PROGRAMADOR....: Mateus Barbosa da Silva
      * SIGLA..........: CTC - Controle de contas
      * OBJETIVO.......: registrar saque sobre uma conta especifica
      * DATA DE CRIACAO: 21/03/2026
      *------------------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *------------------------------------------------------------------------
       CONFIGURATION SECTION.
      *------------------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *------------------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      *------------------------------------------------------------------------
       FILE-CONTROL.
      *------------------------------------------------------------------------
      *
       SELECT CNT001
           ASSIGN TO 'C:\Users\F7021226\Documents\contas.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS RANDOM
           RECORD KEY   IS CNT001-ID-CLI
           FILE STATUS  IS W-CNT001-FILE-STATUS
           .
      *
       SELECT SAQ001
           ASSIGN TO 'C:\Users\F7021226\Documents\saques_contas.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS SAQ001-ID-SAQ
           FILE STATUS  IS W-SAQ001-FILE-STATUS
           .
      *
      *------------------------------------------------------------------------
       DATA DIVISION.
      *------------------------------------------------------------------------
       FILE SECTION.
      *------------------------------------------------------------------------
       FD CNT001.
      *------------------------------------------------------------------------
      *
       01  CNT001-REGISTRO.
           03  CNT001-ID-CLI.
               05  CNT001-AG                  PIC  9(004).
               05  CNT001-CT                  PIC  9(008).
           03  CNT001-DV-AG                   PIC  X(001).
           03  CNT001-DV-CT                   PIC  X(001).
           03  CNT001-NM                      PIC  X(040).
           03  CNT001-CPF                     PIC  9(011).
           03  CNT001-DT-NSC                  PIC  X(008).
           03  CNT001-SDO                     PIC  9(015)V99.
      *
      *------------------------------------------------------------------------
       FD SAQ001.
      *------------------------------------------------------------------------
      *
       01  SAQ001-REGISTRO.
           03  SAQ001-ID-SAQ                  PIC  9(018).
           03  SAQ001-CT-CLI                  PIC  9(008).
           03  SAQ001-AG-CLI                  PIC  9(008).
           03  SAQ001-VL                      PIC  9(015)V99.
           03  SAQ001-TS-SAQ                  PIC  X(016).
      *
      *------------------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       77  W-CNT001-FILE-STATUS               PIC  X(002) VALUE ZEROS.
       77  W-SAQ001-FILE-STATUS               PIC  X(002) VALUE ZEROS.
      *
       01  W-FIM-SAQ001                       PIC  X(001).
           88  W-FIM-SAQ001-S                 VALUE "S".
           88  W-FIM-SAQ001-N                 VALUE "N".
      *
      *------------------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  W-TS-CRR.
           03  W-AA-CRR                        PIC  9(004).
           03  W-MM-CRR                        PIC  9(002).
           03  W-DD-CRR                        PIC  9(002).
           03  W-HH-CRR                        PIC  9(002).
           03  W-MNTO-CRR                      PIC  9(002).
           03  W-SGDO-CRR                      PIC  9(002).
           03  W-CTSG-CRR                      PIC  9(002).
           03  W-DIF-HH-CRR                    PIC  S9(004).
      *
       01  W-ID-ULT-REG-SAQ001                 PIC  9(018) VALUE ZEROS.
      *
      * LINKAGE SECTION
      *
       01  COMMAREA.
           03  CTCS0003-VRV-ENTD.
               05  CTCS0003-AG                 PIC  9(004) VALUE 1234.
               05  CTCS0003-CT                 PIC  9(008)
                   VALUE 12345678.
               05  CTCS0003-VL-SAQ             PIC  9(015)V99
                   VALUE 99,99.
           03  CTCS0003-VRV-RTN.
               05  CTCS0003-CD-RTN             PIC  9(004) VALUE ZEROS.
               05  CTCS0003-TX-MSG-RTN         PIC  X(080) VALUE SPACES.
      *
      *------------------------------------------------------------------------
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
       000000-PRINCIPAL SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 010000-OBTER-TS
           PERFORM 020000-TRATAR-BASE-DADOS
           PERFORM 030000-DEBITAR-CT
           PERFORM 040000-SALVAR-REG-SAQ
           .
      *
       000000-FINALIZAR.
           DISPLAY 'CTCS0003-CD-RTN....: ' CTCS0003-CD-RTN
           DISPLAY 'CTCS0003-TX-MSG-RTN: ' CTCS0003-TX-MSG-RTN
           CLOSE CNT001 SAQ001
           GOBACK
           .
      *------------------------------------------------------------------------
       010000-OBTER-TS SECTION.
      *------------------------------------------------------------------------
      *
           MOVE FUNCTION CURRENT-DATE TO W-TS-CRR
           .
      *
       010000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       020000-TRATAR-BASE-DADOS SECTION.
      *------------------------------------------------------------------------
      *
           OPEN I-O SAQ001
      *
           IF W-SAQ001-FILE-STATUS EQUAL "35"
               CLOSE SAQ001
               OPEN OUTPUT SAQ001
               CLOSE SAQ001
               OPEN I-O SAQ001
           END-IF
      *
           OPEN I-O CNT001
      *
           IF W-CNT001-FILE-STATUS EQUAL "35"
               CLOSE CNT001
               OPEN OUTPUT SAQ001
               CLOSE SAQ001
               OPEN I-O SAQ001
           END-IF
      *
           .
      *
       020000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       030000-DEBITAR-CT SECTION.
      *------------------------------------------------------------------------
      *
           MOVE CTCS0003-AG TO CNT001-AG
           MOVE CTCS0003-CT TO CNT001-CT
      *
           READ CNT001
               KEY IS CNT001-ID-CLI
           END-READ
      *
           IF W-CNT001-FILE-STATUS EQUAL "00"
               CONTINUE
           ELSE
               PERFORM 999002-TRATAR-ERRO
           END-IF
      *
           IF CTCS0003-VL-SAQ GREATER THAN CNT001-SDO
               PERFORM 999004-TRATAR-ERRO
           END-IF
      *
           SUBTRACT
               CTCS0003-VL-SAQ FROM CNT001-SDO
           END-SUBTRACT
      *
           REWRITE CNT001-REGISTRO
               INVALID KEY
                   PERFORM 999006-TRATAR-ERRO
           END-REWRITE
      *
           .
      *
       030000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       040000-SALVAR-REG-SAQ SECTION.
      *------------------------------------------------------------------------
      *
           INITIALIZE SAQ001-REGISTRO
      *
           MOVE ZEROS         TO W-ID-ULT-REG-SAQ001
           SET W-FIM-SAQ001-N TO TRUE
      *
           PERFORM UNTIL W-FIM-SAQ001-S
               READ SAQ001 NEXT
                   AT END
                       MOVE SAQ001-ID-SAQ TO W-ID-ULT-REG-SAQ001
                       SET W-FIM-SAQ001-S TO TRUE
               END-READ
           END-PERFORM
      *
           IF W-SAQ001-FILE-STATUS NOT EQUAL "00" AND
               W-SAQ001-FILE-STATUS NOT EQUAL "10"
               PERFORM 999008-TRATAR-ERRO
           END-IF
      *
           INITIALIZE SAQ001-REGISTRO
      *
           ADD 1 TO W-ID-ULT-REG-SAQ001 GIVING SAQ001-ID-SAQ
      *
           MOVE CTCS0003-AG     TO SAQ001-AG-CLI
           MOVE CTCS0003-CT     TO SAQ001-CT-CLI
           MOVE CTCS0003-VL-SAQ TO SAQ001-VL
           MOVE W-TS-CRR(1:16)  TO SAQ001-TS-SAQ
      *
           WRITE SAQ001-REGISTRO
               AFTER ADVANCING W-ID-ULT-REG-SAQ001 LINES
               NOT INVALID KEY
                   MOVE "CTCS0003 - Saque realizado com sucesso." TO
                       CTCS0003-TX-MSG-RTN
               INVALID KEY
                   PERFORM 999010-TRATAR-ERRO
           END-WRITE
           .
      *
       040000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       999002-TRATAR-ERRO SECTION.
      *------------------------------------------------------------------------
      *
           MOVE 2 TO CTCS0003-CD-RTN
           STRING
               "CTCS0003 - Cliente não encontrado ou erro ao "
               "acessar a base de dados. FS="
               W-CNT001-FILE-STATUS
               "."
               DELIMITED BY SIZE
               INTO CTCS0003-TX-MSG-RTN
           END-STRING
      *
           .
      *
       999002-SAIR.
           PERFORM 000000-FINALIZAR
           .
      *------------------------------------------------------------------------
       999004-TRATAR-ERRO SECTION.
      *------------------------------------------------------------------------
      *
           MOVE 4 TO CTCS0003-CD-RTN
           MOVE "CTCS0003 - Saldo insuficiente para saque."
               TO CTCS0003-TX-MSG-RTN
           .
      *
       999004-SAIR.
           PERFORM 000000-FINALIZAR
           .
      *
      *------------------------------------------------------------------------
       999006-TRATAR-ERRO SECTION.
      *------------------------------------------------------------------------
      *
           MOVE 6 TO CTCS0003-CD-RTN
           STRING
               "CTCS0003 - Erro ao atualizar conta de cliente ou erro "
               " ao acessar base de dados. FS="
               W-CNT001-FILE-STATUS
               "."
               DELIMITED BY SIZE
               INTO CTCS0003-TX-MSG-RTN
           END-STRING
      *
           .
      *
       999006-SAIR.
           PERFORM 000000-FINALIZAR
           .
      *------------------------------------------------------------------------
       999008-TRATAR-ERRO SECTION.
      *------------------------------------------------------------------------
      *
           MOVE 8 TO CTCS0003-CD-RTN
           STRING
               "CTCS0003 - Erro ao acessar base de dados do SAQ001. "
               "FS="
               W-SAQ001-FILE-STATUS
               "."
               DELIMITED BY SIZE
               INTO CTCS0003-TX-MSG-RTN
           END-STRING
      *
           .
      *
       999008-SAIR.
           PERFORM 000000-FINALIZAR
           .
      *------------------------------------------------------------------------
       999010-TRATAR-ERRO SECTION.
      *------------------------------------------------------------------------
      *
           MOVE 10 TO CTCS0003-CD-RTN
           STRING
               "CTCS0003 - Erro ao inserir registro do saque na base de"
               " dados. FS="
               W-SAQ001-FILE-STATUS
               "."
               DELIMITED BY SIZE
               INTO CTCS0003-TX-MSG-RTN
           END-STRING
      *
           .
      *
       999010-SAIR.
           PERFORM 000000-FINALIZAR
           .
