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
           ACCESS MODE IS RANDOM
           RECORD KEY IS CNT001-REGISTRO
           FILE STATUS IS W-FILE-STATUS-CNT001
           .
      *
       SELECT DEP001
           ASSIGN TO 'C:\Users\F7021226\Documents\depositos_contas.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS DEP001-REGISTRO
           FILE STATUS IS W-FILE-STATUS-DEP001
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
               05  CNT001-AG                  PIC  9(008).
               05  CNT001-CT                  PIC  9(008).
           03  CNT001-DV-AG                   PIC  X(001).
           03  CNT001-DV-CT                   PIC  X(001).
           03  CNT001-NM                      PIC  X(040).
           03  CNT001-CPF                     PIC  9(011).
           03  CNT001-DT-NSC                  PIC  X(008).
           03  CNT001-SDO                     PIC  9(008)V99.
      *
      *------------------------------------------------------------------------
       FD DEP001.
      *------------------------------------------------------------------------
      *
       01  DEP001-REGISTRO.
           03  DEP001-ID-DEP                   PIC  9(006).
           03  DEP001-VL-DEP                   PIC  9(008)V99.
           03  DEP001-TS-DEP                   PIC  X(008).
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
      *------------------------------------------------------------------------
      * LINKAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  COMMAREA.
           03  CTCS0002-VRV-ENTD.
               05  CTCS0002-AG                 PIC 9(004) VALUE ZEROS.
               05  CTCS0002-CT                 PIC 9(008) VALUE ZEROS.
               05  CTCS0002-VL                 PIC 9(008)V99 VALUE
                   ZEROS.
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
           PERFORM 040000-SALVAR-REG-CT
           .
      *
       000000-SAIR.
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
           ADD CTCS0002-VL TO CNT001-SDO
               ON SIZE ERROR
                   STRING "CTCS0002 - Valor na conta extrapola o "
                          "limite do sistema." DELIMITED BY SIZE
                       INTO CTCS0002-TX-MSG-RTN
                   PERFORM 000000-SAIR
           END-ADD
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
       040000-SALVAR-REG-CT SECTION.
      *------------------------------------------------------------------------
      *
           DISPLAY 'SALVANDO REGISTRO'
           .
      *
       040000-SAIR.
           EXIT SECTION
           .
