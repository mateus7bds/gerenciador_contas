      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0001.
      *------------------------------------------------------------------------
      * PROGRAMADOR....: Mateus Barbosa da Silva
      * Sigla..........: CTC - Controle de contas
      * OBJETIVO.......: permitir a inclusao, alteração e exclusão de
      * contas bancárias simples
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
       SELECT ARQ-CONTAS
           ASSIGN TO CAMINHO-ARQ-CONTAS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS REGISTRO-CONTAS
           FILE STATUS IS STATUS-ARQ-CONTAS
           .
      *------------------------------------------------------------------------
       DATA DIVISION.
      *------------------------------------------------------------------------
       FILE SECTION.
      *------------------------------------------------------------------------
      *
       FD  ARQ-CONTAS.
      *
       01  REGISTRO-CONTAS.
           03  AGENCIA           PIC  9(008).
           03  CONTA             PIC  9(008).
           03  NOME              PIC  X(040).
           03  CPF               PIC  9(011).
           03  DATA-NASCIMENTO   PIC  X(008).
           03  SALDO             PIC  9(008)V99.
      *------------------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------------------
       01  STATUS-ARQ-CONTAS         PIC  X(002) VALUE SPACES.
      *------------------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  IC-ITRA                   PIC  9(003) VALUE ZEROS COMP-5.
      *
       01  CAMINHO-ARQ-CONTAS        PIC  X(100) VALUE SPACES.
      *
       01  DIA-HORARIO-CORRENTE.
           03  ANO                   PIC  9(004).
           03  MES                   PIC  9(002).
           03  DIA                   PIC  9(002).
           03  HORA                  PIC  9(002).
           03  MINUTOS               PIC  9(002).
           03  SEGUNDOS              PIC  9(002).
           03  CENTESIMO             PIC  9(002).
           03  DIF-HORA              PIC  S9(004).
      *
       01  HORARIO-EXIBICAO.
           03  HORA                 PIC  9(002) VALUE ZEROS.
           03  FILLER               PIC  X(001) VALUE ':'.
           03  MINUTOS              PIC  9(002) VALUE ZEROS.
           03  FILLER               PIC  X(001) VALUE ':'.
           03  SEGUNDOS             PIC  9(002) VALUE ZEROS.
      *
       01  DATA-EXIBICAO.
           03  DIA                  PIC  9(002) VALUE ZEROS.
           03  FILLER               PIC  X(001) VALUE '/'.
           03  MES                  PIC  9(002) VALUE ZEROS.
           03  FILLER               PIC  X(001) VALUE '/'.
           03  ANO                  PIC  9(004) VALUE ZEROS.
      *
      * Nao ha como utilizar o LINKAGE SECTION no OpenCOBOL, entao
      * defini uma variavel que funcionaria de forma semelhante ao
      * COMMAREA
      *
      *------------------------------------------------------------------------
      * LINKAGE SECTION.
      *------------------------------------------------------------------------
      *
       01  COMMAREA.
           03  ENTD-PROGRAMA.
               05  OPERACAO          PIC  9(002) VALUE ZEROS.
               05  AGENCIA           PIC  9(008) VALUE ZEROS.
               05  DV-AGENCIA        PIC  X(002) VALUE ZEROS.
               05  CONTA             PIC  9(008) VALUE ZEROS.
               05  DV-CONTA          PIC  X(002) VALUE ZEROS.
               05  NOME              PIC  X(040) VALUE SPACES.
               05  CPF               PIC  9(011) VALUE ZEROS.
               05  DATA-NASCIMENTO   PIC  X(008) VALUE ZEROS.
      *
      * PROCEDURE DIVISION USING COMMAREA.
      *
      *------------------------------------------------------------------------
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
       000000-PRINCIPAL SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 010000-OBTER-HORA-DATA
      *
           PERFORM 020000-TRATAR-BASE-DADOS
      *
           EVALUATE OPERACAO
               WHEN 1
                   PERFORM 030000-CRIAR-CONTA
               WHEN 2
      *             PERFORM 030000-ATUALIZAR-CC
                    DISPLAY 'ATUALIZAR CONTA CORRENTE'
               WHEN 3
      *             PERFORM 040000-EXCLUIR-CC
                    DISPLAY 'EXCLUIR CONTA CORRENTE'
               WHEN OTHER
                   DISPLAY 'OPCAO INVALIDA'
           END-EVALUATE

           .
      *
       000000-SAIR-PGM.
           CLOSE ARQ-CONTAS
           GOBACK
           .
      *------------------------------------------------------------------------
       010000-OBTER-HORA-DATA SECTION.
      *------------------------------------------------------------------------
      * obtendo a data e hora do sistema operacional em que o programa estah
      * sendo executado
      *
           MOVE FUNCTION CURRENT-DATE TO DIA-HORARIO-CORRENTE
      *
           MOVE CORRESPONDING DIA-HORARIO-CORRENTE TO DATA-EXIBICAO
                                                      HORARIO-EXIBICAO
           .
      *
       010000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       020000-TRATAR-BASE-DADOS SECTION.
      *------------------------------------------------------------------------
      * Determinando o caminho do arquivo
      *
           STRING 'C:\Users\F7021226\'          DELIMITED BY SIZE
                  'Documents\projetos_pessoais' DELIMITED BY SIZE
                  '\cobol\gerenciador_cc\'      DELIMITED BY SIZE
                  'ARQ-CONTAS.TXT'              DELIMITED BY SIZE
           INTO CAMINHO-ARQ-CONTAS
      *
      * Tenta abrir o arquivo apenas para consulta e verificar se existe
           OPEN INPUT ARQ-CONTAS
      * Verifica se o arquivo não existe (35)
           IF STATUS-ARQ-CONTAS EQUAL "35"
               CLOSE ARQ-CONTAS
      * Criando arquivo
               OPEN OUTPUT ARQ-CONTAS
           END-IF
      * Fechando o arquivo criado ou existente
           CLOSE ARQ-CONTAS
      * Abrindo o arquivo com a possibilidade de alterações sem perca do
      * conteudo existente
           OPEN I-O ARQ-CONTAS
      *
           .
      *
       020000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       030000-CRIAR-CONTA SECTION.
      *------------------------------------------------------------------------
      *
           DISPLAY 'CRIAR CONTA CORRENTE'
           .
      *
       030000-SAIR.
           EXIT SECTION
           .
