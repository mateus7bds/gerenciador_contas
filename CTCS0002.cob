      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0002.
      *------------------------------------------------------------------------
      * PROGRAMADOR: Mateus Barbosa da Silva
      * SIGLA......: CTC - Controle de Contas
      * OBJETIVO...: Realizar saque e deposito nas contas gerando tambem
      * os comprovantes dessas operacoes
      * DATA DE CRIACAO: 14/04/2026
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
           RECORD KEY   IS CNT001-ID-CT
           FILE STATUS  IS W-FILE-STATUS-CNT001
           .
      *
       SELECT DEP001
           ASSIGN TO 'C:\Users\F7021226\Documents\depositos_contas.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS DEP001-ID-DEP
           FILE STATUS  IS W-FILE-STATUS-DEP001
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
           03  CNT001-ID-CT.
               05  CNT001-AG                  PIC  9(004).
               05  CNT001-CT                  PIC  9(008).
           03  CNT001-DV-AG                   PIC  X(001).
           03  CNT001-DV-CT                   PIC  X(001).
           03  CNT001-NM                      PIC  X(040).
           03  CNT001-CPF                     PIC  9(011).
           03  CNT001-DT-NSC                  PIC  X(008).
           03  CNT001-SDO                     PIC  9(015)V99.
      *>      03  CNT001-SDO-PTE-INT             PIC  9(015).
      *>      03  CNT001-SDO-PTE-DCML            PIC  9(002).
      *
      *------------------------------------------------------------------------
       FD DEP001.
      *------------------------------------------------------------------------
      *
       01  DEP001-REGISTRO.
           03  DEP001-ID-DEP                   PIC  9(018).
           03  DEP001-CT-CLI                   PIC  9(008).
           03  DEP001-AG-CLI                   PIC  9(008).
           03  DEP001-VL-PTE-INT               PIC  9(015).
           03  DEP001-VL-PTE-DCML              PIC  9(002).
           03  DEP001-TS-DEP                   PIC  X(016).
      *
      *------------------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       77  W-FILE-STATUS-CNT001                PIC  X(002) VALUE ZEROS.
       77  W-FILE-STATUS-DEP001                PIC  X(002) VALUE ZEROS.
      *
      *------------------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       77  W-SDO-PTE-DCML-NRC                  PIC  9(002) VALUE ZEROS.
       77  W-MOR-VL-ID-DEP001                  PIC  9(018) VALUE
           999999999999999999.
       77  W-ID-ULT-REG-DEP001                 PIC  9(018) VALUE ZEROS.
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
       01  W-SDO-ATZD-CMT                      PIC  9(015)V99.
      *
      *------------------------------------------------------------------------
      * LINKAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  COMMAREA.
           03  CTCS0002-VRV-ENTD.
               05  CTCS0002-AG                 PIC 9(004) VALUE 1234.
               05  CTCS0002-CT                 PIC 9(008) VALUE
                  12345678.
               05  CTCS0002-VL-DEP             PIC 9(015)V99 VALUE
                   1999,99.
           03  CTCS0002-VRV-RTN.
               05  CTCS0002-CD-RTN             PIC  X(004) VALUE ZEROS.
               05  CTCS0002-TX-MSG-RTN         PIC  X(080) VALUE SPACES.
      *
      *------------------------------------------------------------------------
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
       000000-PRINCIPAL SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 010000-OBTER-TS
           PERFORM 020000-TRATAR-BASE-DADOS
           PERFORM 030000-DEPOSITAR-CONTA
           PERFORM 040000-SALVAR-REG-DEP
           .
      *
       000000-SAIR.
           DISPLAY 'CTCS0002-TX-MSG-RTN: ' CTCS0002-TX-MSG-RTN
           CLOSE CNT001 DEP001
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
           OPEN I-O CNT001
      *
           IF W-FILE-STATUS-CNT001 EQUAL "35"
               CLOSE CNT001
               OPEN OUTPUT CNT001
               CLOSE CNT001
               OPEN I-O CNT001
           END-IF
      *
           OPEN I-O DEP001
      *
           IF W-FILE-STATUS-DEP001 EQUAL "35"
               CLOSE DEP001
               OPEN OUTPUT DEP001
               CLOSE DEP001
               OPEN I-O DEP001
           END-IF
           .
      *
       020000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       030000-DEPOSITAR-CONTA SECTION.
      *------------------------------------------------------------------------
      *
           MOVE CTCS0002-AG TO CNT001-AG
           MOVE CTCS0002-CT TO CNT001-CT
      *
           READ CNT001
               KEY IS CNT001-ID-CT
           END-READ
      *
           IF W-FILE-STATUS-CNT001 EQUAL "00"
               CONTINUE
           ELSE
               STRING "CTCS0002 - Conta nao existe ou "
                      "erro ao acessar a base de dados."
                      " - FILE-STATUS=" W-FILE-STATUS-CNT001 "."
                      DELIMITED BY SIZE INTO CTCS0002-TX-MSG-RTN
               PERFORM 000000-SAIR
           END-IF
      *
      *>      DISPLAY 'CNT001-SDO-PTE-INT.: ' CNT001-SDO-PTE-INT
      *>      DISPLAY 'CNT001-SDO-PTE-DCML: ' CNT001-SDO-PTE-DCML
      *> *
      *>      MOVE CNT001-SDO-PTE-DCML TO W-SDO-PTE-DCML-NRC
      *> *
      *>      COMPUTE W-SDO-ATZD-CMT = CNT001-SDO-PTE-INT
      *>          + CNT001-SDO-PTE-DCML*(0,01)
      *>          ON SIZE ERROR
      *>              STRING "CTCS0002 - Erro ao processar saldo "
      *>                     "do cliente (base de dados)."
      *>                     DELIMITED BY SIZE
      *>                     INTO CTCS0002-TX-MSG-RTN
      *>              PERFORM 000000-SAIR
      *>      END-COMPUTE
      *
      *>      ADD CTCS0002-VL-DEP TO W-SDO-ATZD-CMT
           ADD CTCS0002-VL-DEP TO CNT001-SDO
               ON SIZE ERROR
                   STRING "CTCS0002 - Valor na conta extrapola o "
                          "limite do sistema." DELIMITED BY SIZE
                       INTO CTCS0002-TX-MSG-RTN
                   PERFORM 000000-SAIR
           END-ADD
      *> *
      *>      MOVE W-SDO-ATZD-CMT(1:15) TO CNT001-SDO-PTE-INT
      *>      MOVE W-SDO-ATZD-CMT(16:2) TO CNT001-SDO-PTE-DCML
      *
           REWRITE CNT001-REGISTRO
               INVALID KEY
                   STRING "CTCS0002 - Erro ao depositar valor na conta."
                          " - FILE-STATUS=" W-FILE-STATUS-CNT001
                          DELIMITED BY SIZE INTO CTCS0002-TX-MSG-RTN
                   PERFORM 000000-SAIR
               NOT INVALID KEY
                   STRING "CTCS0002 - Valor depositado na conta com "
                          "sucesso." DELIMITED BY SIZE
                          INTO CTCS0002-TX-MSG-RTN
           END-REWRITE
      *
           .
      *
       030000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       040000-SALVAR-REG-DEP SECTION.
      *------------------------------------------------------------------------
      *
           MOVE W-MOR-VL-ID-DEP001 TO DEP001-ID-DEP
      *
           START DEP001
               KEY > DEP001-ID-DEP
               NOT INVALID KEY
                   DISPLAY 'ERRO NO START'
                   PERFORM 000000-SAIR
           END-START
      *
           READ DEP001 PREVIOUS
               INVALID KEY
                   DISPLAY 'NAO ACHOU O ULTIMO REGISTRO'
                   PERFORM 000000-SAIR
               NOT INVALID KEY
                   DISPLAY 'DEP001-ID-DEP: ' DEP001-ID-DEP
                   MOVE DEP001-ID-DEP TO W-ID-ULT-REG-DEP001
                   DISPLAY 'W-ID-ULT-REG-DEP001: ' W-ID-ULT-REG-DEP001
           END-READ
      *
           INITIALIZE DEP001-REGISTRO
      *
           ADD W-ID-ULT-REG-DEP001 1 GIVING DEP001-ID-DEP
      *
           MOVE CTCS0002-AG     TO DEP001-AG-CLI
           MOVE CTCS0002-CT     TO DEP001-CT-CLI
           MOVE CTCS0002-VL-DEP(1:15) TO DEP001-VL-PTE-INT
           MOVE CTCS0002-VL-DEP(16:2) TO DEP001-VL-PTE-DCML
           MOVE W-TS-CRR(1:16)  TO DEP001-TS-DEP
      *
           WRITE DEP001-REGISTRO
               NOT INVALID KEY
                   DISPLAY 'DEPOSITO SALVO COM SUCESSO'
                   PERFORM 000000-SAIR
               INVALID KEY
                   DISPLAY 'ERRO AO TENTAR SALVAR DEPOSITO'
           END-WRITE
      *
           .
      *
       040000-SAIR.
           EXIT SECTION
           .
