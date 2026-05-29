      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0004.
      *------------------------------------------------------------------------
      * PROGRAMADOR: Nome do desenvolvedor.
      * SIGLA......: Sigla – Nome da sigla (ou sistema).
      * OBJETIVO...: Objetivo do programa.
      * DATA DE CRIACAO: Data de criação do programa.
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
       77  W-CNT001-FILE-STATUS               PIC  X(002) VALUE ZEROS.
      *------------------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *------------------------------------------------------------------------
      *------------------------------------------------------------------------
      * LINKAGE SECTION.
      *------------------------------------------------------------------------
       01  COMMAREA.
           03  CTCS0004-VRV-ENTD.
               05  CTCS0004-AG-ETC PIC X(010).

      *------------------------------------------------------------------------
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *------------------------------------------------------------------------
      * VERIFICAR TAMANHO DA AREA DE DADOS DA LINKAGE SECTION
      * OBTER TIMESTAMP
      *    PERFORM 010000-OBTER-TIMESTAMP
      * ABRIR ARQUIVOS
      *    PERFORM 020000-ABRIR-BASE-DADOS
      * DEBITAR DO ORDENANTE
      *    PERFORM 030000-DEBITAR-ORDENANTE
      * CREDITAR NA BENEFICIARIO
      *    PERFORM 040000-CREDITAR-BENEFICIARIO
      * REGISTRAR TRANSFERENCIA
      *    PERFORM 050000-REGISTRAR-TRANSFERENCIA
           .
      *
       000000-FINALIZAR.
           GOBACK
           .
