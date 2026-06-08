      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0004.
      *------------------------------------------------------------------------
      * PROGRAMADOR: Mateus Barbosa da Silva.
      * SIGLA......: MCT - Movimentação de Contas.
      * OBJETIVO...: realizar transferencia intrabancaria simples.
      * DATA DE CRIACAO: 08/06/2026.
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
      *------------------------------------------------------------------------
       DATA DIVISION.
      *------------------------------------------------------------------------
       FILE SECTION.
      *------------------------------------------------------------------------
       FD CNT001.
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
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       77  W-CNT001-FILE-STATUS               PIC  X(002) VALUE ZEROS.
      *
      *------------------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  L-TS-CRR.
           03  L-AA-CRR                        PIC  9(004).
           03  L-MM-CRR                        PIC  9(002).
           03  L-DD-CRR                        PIC  9(002).
           03  L-HH-CRR                        PIC  9(002).
           03  L-MNTO-CRR                      PIC  9(002).
           03  L-SGDO-CRR                      PIC  9(002).
           03  L-CTSG-CRR                      PIC  9(002).
           03  L-DIF-HH-CRR                    PIC  S9(004).
      *
       01  L-VLDC-CLI-EXT.
           03  L-AG-CLI-VLDC                   PIC  9(004) VALUE ZEROS.
           03  L-CT-CLI-VLDC                   PIC  9(008) VALUE ZEROS.
           03  L-TIPO-CLI                      PIC  X(020) VALUE SPACES.
      *
      *------------------------------------------------------------------------
      * LINKAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  COMMAREA.
      *
           03  CTCS0004-VRV-ENTD.
               05  CTCS0004-AG-RMTE          PIC  9(004) VALUE 1234.
               05  CTCS0004-CT-RMTE          PIC  9(008) VALUE 12345678.
               05  CTCS0004-AG-BNFC          PIC  9(004) VALUE 4321.
               05  CTCS0004-CT-BNFC          PIC  9(008) VALUE 87654321.
               05  CTCS0004-VL-TRNS          PIC  9(015)V99
                   VALUE 200,40.
      *
           03  CTCS0004-VRV-RTN.
               05  CTCS0004-CD-RTN             PIC  9(004) VALUE ZEROS.
               05  CTCS0004-TX-MSG-RTN         PIC  X(080) VALUE SPACES.
               05  CTCS0004-FILE-STATUS-VSAM   PIC  X(002) VALUE ZEROS.
               05  CTCS0004-TX-MSG-FILE-STATUS PIC  X(080) VALUE ZEROS.
      *
      *------------------------------------------------------------------------
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 010000-OBTER-TIMESTAMP
           PERFORM 020000-ABRIR-BASE-DADOS
           PERFORM 030000-VALIDAR-DD-ENTD
           PERFORM 040000-DEBITAR-REMETENTE
           PERFORM 050000-CREDITAR-BENEFICIARIO
      *
      * DEBITAR DO ORDENANTE
      *    PERFORM 030000-DEBITAR-REMETENTE
      * CREDITAR NA BENEFICIARIO
      *    PERFORM 040000-CREDITAR-BENEFICIARIO
      * REGISTRAR TRANSFERENCIA
      *    PERFORM 050000-REGISTRAR-TRANSFERENCIA
           .
      *
       000000-FINALIZAR.
           CLOSE CNT001
           GOBACK
           .
      *------------------------------------------------------------------------
       010000-OBTER-TIMESTAMP SECTION.
      *------------------------------------------------------------------------
      *
           MOVE FUNCTION CURRENT-DATE TO L-TS-CRR
      *
           .
      *
       010000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       020000-ABRIR-BASE-DADOS SECTION.
      *------------------------------------------------------------------------
      *
           OPEN I-O CNT001
      *
      * FILE-STATUS - "35" - Arquivo inexistente
      *
           IF W-CNT001-FILE-STATUS EQUAL "35"
               CLOSE CNT001
               OPEN OUTPUT CNT001
               CLOSE CNT001
               OPEN I-O CNT001
           END-IF
      *
           .
      *
       02000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       030000-VALIDAR-DD-ENTD SECTION.
      *------------------------------------------------------------------------
      *
      * validar remetente
      *
           MOVE CTCS0004-AG-RMTE TO L-AG-CLI-VLDC
           MOVE CTCS0004-CT-RMTE TO L-CT-CLI-VLDC
           MOVE "Remetente"      TO L-TIPO-CLI
           PERFORM 031000-VALIDAR-CT-CLI
           DISPLAY "REMETENTE VALIDO!"
      *
      * validar beneficiario
      *
           MOVE CTCS0004-AG-BNFC TO L-AG-CLI-VLDC
           MOVE CTCS0004-CT-BNFC TO L-CT-CLI-VLDC
           MOVE "Beneficiario"   TO L-TIPO-CLI
           PERFORM 031000-VALIDAR-CT-CLI
           DISPLAY 'BENEFICIARIO VALIDO!'
      *
      * verificar se saldo está abaixo do limite diario
      *
      * aqui seria realizado o acesso a um banco de dados para consultar
      * o limite diário, mas eu usarei um valor fixo - R$ 10.000,00
      *
           IF CTCS0004-VL-TRNS GREATER THAN 10000
               DISPLAY 'VALOR ULTRAPASSA O LIMITE DIARIO'
               PERFORM 000000-FINALIZAR
           END-IF
      *
           .
       030000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       031000-VALIDAR-CT-CLI SECTION.
      *------------------------------------------------------------------------
      *
      * Entrada:
      *   L-AG-CLI-VLDC
      *   L-CT-CLI-VLDC
      * Saida: não tem saida, apenas verifica se o cliente existe de
      * fato
      *
           INITIALIZE CNT001-REGISTRO
      *
           MOVE CTCS0004-AG-RMTE TO CNT001-AG
           MOVE CTCS0004-CT-RMTE TO CNT001-CT
      *
      * Arquivo indexado - Uso de chaves para encontrar registros
      *
           READ CNT001
               KEY IS CNT001-ID-CLI
           END-READ
      *
      * "00" - Sucesso na operação - Cliente encontrado e sem erros
      *
           IF W-CNT001-FILE-STATUS EQUAL "00"
               CONTINUE
           ELSE
               DISPLAY 'ERRO - CLIENTE NAO ENCONTRADO'
               DISPLAY 'TIPO DE CLIENTE: ' L-TIPO-CLI
               PERFORM 000000-FINALIZAR
           END-IF
      *
           .
      *
       0210000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       040000-DEBITAR-REMETENTE SECTION.
      *------------------------------------------------------------------------
      *
           INITIALIZE CNT001-REGISTRO
      *
           MOVE CTCS0004-AG-RMTE TO CNT001-AG
           MOVE CTCS0004-CT-RMTE TO CNT001-CT
      *
           READ CNT001
               KEY IS CNT001-ID-CLI
           END-READ
      *
           IF W-CNT001-FILE-STATUS EQUAL "00"
               CONTINUE
           ELSE
               DISPLAY 'ERRO AO ENCONTRAR REMETENTE'
               PERFORM 000000-FINALIZAR
           END-IF
      *
           SUBTRACT
               CTCS0004-VL-TRNS FROM CNT001-SDO
               GIVING CNT001-SDO
               ON SIZE ERROR
                   DISPLAY 'ERRO AO SUBTRAIR DA CT DO REMETENTE'
                   PERFORM 000000-FINALIZAR
           END-SUBTRACT
      *
           REWRITE CNT001-REGISTRO
               INVALID KEY
                   DISPLAY 'ERRO AO DEBITAR NA CT DO REMETENTE'
                   PERFORM 000000-FINALIZAR
           END-REWRITE
      *
           .
      *
       040000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       050000-CREDITAR-BENEFICIARIO SECTION.
      *------------------------------------------------------------------------
      *
           INITIALIZE CNT001-REGISTRO
      *
           MOVE CTCS0004-AG-BNFC TO CNT001-AG
           MOVE CTCS0004-CT-BNFC TO CNT001-CT
      *
           READ CNT001
               KEY IS CNT001-ID-CLI
           END-READ
      *
           IF W-CNT001-FILE-STATUS EQUAL "00"
               CONTINUE
           ELSE
               DISPLAY 'ERRO ACESSO - BASE DE DADOS - BENEFICIARIO'
               PERFORM 000000-FINALIZAR
           END-IF
      *
           ADD
              CTCS0004-VL-TRNS TO CNT001-SDO
           END-ADD
      *
           REWRITE CNT001-REGISTRO
               NOT INVALID KEY
                   DISPLAY 'ERRO ATUALIZAR - BASE DADOS - BENEFICIARIO'
                   PERFORM 000000-FINALIZAR
           END-REWRITE
      *
           .
      *
       050000-SAIR.
           EXIT SECTION
           .
