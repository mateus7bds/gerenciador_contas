      *------------------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
       PROGRAM-ID. CTCS0001.
      *------------------------------------------------------------------------
      * PROGRAMADOR....: Mateus Barbosa da Silva
      * SIGLA..........: CTC - Controle de contas
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
      *
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
       01  PESO-DGT                 PIC  9(001) VALUE ZERO.
       01  DIGITO                   PIC  9(001) VALUE ZERO.
       01  AGENCIA-STRING           PIC  X(004) VALUE ZEROS.
       01  RESULTADO-PESO           PIC  9(002) VALUE ZEROS.
       01  SOMA-PESOS               PIC  9(003) VALUE ZEROS.
       01  FILLER-QUOCIENTE         PIC  9(002) VALUE ZEROS.
       01  RESTO                    PIC  9(002) VALUE ZEROS.
       01  DV-NUMERO                PIC  9(002) VALUE ZEROS.
       01  DV-CALCULADO             PIC  X(001) VALUE SPACE.
      *
       01  VRV-TRT-STRING.
           03  FLAG-ESP             PIC  9(001) VALUE ZEROS.
           03  CCT-STRING           PIC  X(001) VALUE SPACE.
           03  STRING-ENTD          PIC  X(080) VALUE SPACES.
           03  STRING-SAID          PIC  X(080) VALUE SPACES.
           03  DOIS-CCT             PIC  X(002) VALUE SPACES.
       01  ALFABETO                 PIC  X(052) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".
       01  FLAG-LETRA               PIC  9(001) VALUE ZERO.
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
           03  S0001-ENTD-PROGRAMA.
               05  S0001-OPERACAO          PIC  9(002) VALUE 1.
               05  S0001-AGENCIA           PIC  9(004) VALUE 1234.
               05  S0001-DV-AGENCIA        PIC  X(001) VALUE "3".
               05  S0001-CONTA             PIC  9(008) VALUE 12345678.
               05  S0001-DV-CONTA          PIC  X(001) VALUE "9".
               05  S0001-NOME              PIC  X(080) VALUE SPACES.
               05  S0001-CPF               PIC  9(011) VALUE ZEROS.
               05  S0001-DATA-NASCIMENTO   PIC  X(008) VALUE ZEROS.
           03  S0001-VRV-RTN.
               05  S0001-CD-RTN            PIC  9(002) VALUE ZEROS.
               05  S0001-TX-MSG-RTN        PIC  X(080) VALUE SPACES.
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
           EVALUATE S0001-OPERACAO
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
           PERFORM 031000-VALIDAR-DD-CRIAR-CONTA
           .
      *
       030000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       031000-VALIDAR-DD-CRIAR-CONTA SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 0X1000-VALIDAR-AGENCIA
           PERFORM 0X2000-VALIDAR-CONTA
           PERFORM 0X3100-VALIDAR-CCT-NOME
           PERFORM 0X3200-TRATAR-NOME
           DISPLAY 'S0001-NOME: ' S0001-NOME
           .
      *
       031000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X1000-VALIDAR-AGENCIA SECTION.
      *------------------------------------------------------------------------
      * Agencia:
      *  - formada por 4 digitos
      *  - Pesos: 5, 4, 3, 2
      *  - Módulo: 11
      *
           IF S0001-AGENCIA EQUAL ZEROS
               MOVE 01 TO S0001-CD-RTN
               MOVE 'CTCS0001 - Número da agência igual a zero.' TO
                   S0001-TX-MSG-RTN
           END-IF
      *
           MOVE 5     TO PESO-DGT
           MOVE ZEROS TO SOMA-PESOS
      *
           PERFORM VARYING IC-ITRA FROM 1 BY 1 UNTIL IC-ITRA > 4
               MOVE S0001-AGENCIA(IC-ITRA:1) TO DIGITO
               MULTIPLY PESO-DGT BY DIGITO GIVING RESULTADO-PESO
               ADD RESULTADO-PESO TO SOMA-PESOS
               SUBTRACT 1 FROM PESO-DGT
           END-PERFORM
      *
           DIVIDE SOMA-PESOS BY 11 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO
      *
           SUBTRACT RESTO FROM 11 GIVING DV-NUMERO
      *
           IF DV-NUMERO EQUAL 10
               MOVE "X" TO DV-CALCULADO
           ELSE
               MOVE DV-NUMERO(2:1) TO DV-CALCULADO
           END-IF
      *
           IF S0001-DV-AGENCIA NOT EQUAL DV-CALCULADO
               MOVE 02 TO S0001-CD-RTN
               MOVE "CTCS0001 - Dígito verificador da agência inválida."
                   TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
      *
           DISPLAY 'SAIU VALIDAR-AGENCIA'
           .

      *
       0X1000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X2000-VALIDAR-CONTA SECTION.
      *------------------------------------------------------------------------
      * Conta:
      * - formada por 8 digitos
      * - Pesos: 9, 8, 7, 6, 5, 4, 3, 2
      * - Módulo: 11
      *
           IF S0001-CONTA EQUAL ZEROS
               MOVE 03 TO S0001-CD-RTN
               MOVE "CTCS0001 - Número da conta igual a zero." TO
                   S0001-TX-MSG-RTN
           END-IF
      *
           MOVE 9 TO PESO-DGT
           MOVE ZEROS TO SOMA-PESOS
      *
           PERFORM VARYING IC-ITRA FROM 1 BY 1 UNTIL IC-ITRA > 8
               MOVE S0001-CONTA(IC-ITRA:1) TO DIGITO
               MULTIPLY DIGITO BY PESO-DGT GIVING RESULTADO-PESO
               ADD RESULTADO-PESO TO SOMA-PESOS
               SUBTRACT 1 FROM PESO-DGT
           END-PERFORM
      *
           DIVIDE SOMA-PESOS BY 11 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO
      *
           SUBTRACT RESTO FROM 11 GIVING DV-NUMERO
      *
           IF DV-NUMERO EQUAL 10
               MOVE "X" TO DV-CALCULADO
           ELSE
               MOVE DV-NUMERO(2:1) TO DV-CALCULADO
           END-IF
      *
           IF S0001-DV-CONTA NOT EQUAL DV-CALCULADO
               MOVE 04 TO S0001-CD-RTN
               MOVE "CTCS0001 - Digito verificador da conta invalido."
                   TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
           .
      *
       0X2000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X3200-TRATAR-NOME SECTION.
      *------------------------------------------------------------------------
      * Entrada: S0001-NOME
      * Saida..: STRING-SAID para o S0001-NOME
      *
           MOVE FUNCTION TRIM(S0001-NOME) TO S0001-NOME
      *
      * inicializo a string de saida
           MOVE SPACES TO STRING-SAID
      * considero que o caracter anterior nao eh espaco
           MOVE ZERO TO FLAG-ESP
      * itero sobre cada caracter da string
           PERFORM VARYING IC-ITRA FROM 1 BY 1
               UNTIL IC-ITRA > LENGTH OF STRING-ENTD
      * extraio o caracter
               MOVE STRING-ENTD(IC-ITRA:1) TO CCT-STRING
      * se for espaco, ignore, passe para a proxima iteracao e sinalize
               IF CCT-STRING EQUAL SPACE
                   MOVE 1 TO FLAG-ESP
                   CONTINUE
               END-IF
      * se o caracter nao for um espaco
               IF CCT-STRING NOT EQUAL SPACE
      * se nao for o primeiro caracter e o caracter anterior for um
      * espaco
                   IF IC-ITRA NOT EQUAL 1 AND FLAG-ESP EQUAL 1
      * string saida = string-saida + # (representa o espaco) + cct atual
                       STRING STRING-SAID   DELIMITED BY SIZE
                              "#"           DELIMITED BY SIZE
                              CCT-STRING    DELIMITED BY SIZE
                              INTO STRING-SAID
                   ELSE
      * string saida = string saida + cct atual
                       STRING STRING-SAID   DELIMITED BY SIZE
                              CCT-STRING    DELIMITED BY SIZE
                              INTO STRING-SAID
                   END-IF
                   MOVE 0 TO FLAG-ESP
               END-IF
           END-PERFORM
      *
           INSPECT STRING-SAID REPLACING ALL "#" BY SPACE
      *
           MOVE FUNCTION TRIM(STRING-SAID) TO S0001-NOME
           .
      *
       0X3200-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X3100-VALIDAR-CCT-NOME SECTION.
      *------------------------------------------------------------------------
      * verifica se o nome possui apenas espacos
           IF S0001-NOME EQUAL SPACES
               MOVE 05 TO S0001-CD-RTN
               MOVE "CTCS0001 - Nome de entrada igual a espaços."
                   TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
      * percorre cada caracter do nome para validar todos os caracteres
           PERFORM VARYING IC-ITRA FROM 1 BY 1
               UNTIL IC-ITRA > LENGTH OF S0001-NOME
      * extrai o caracter
               MOVE S0001-NOME(IC-ITRA:1) TO CCT-STRING
      * verifica se o caracter se encontra no alfabeto
               INSPECT ALFABETO TALLYING FLAG-LETRA
                   FOR ALL CCT-STRING
      * se nao for letra nem espaco, lanca-se um erro
               IF (FLAG-LETRA NOT EQUAL 1) AND
                  (CCT-STRING NOT EQUAL SPACE)
                   MOVE 06 TO S0001-CD-RTN
                   STRING "CTCS0001 - Foram encontrados "
                          "caracteres invalidos "
                          "nome do cliente."
                      DELIMITED BY SIZE INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
               END-IF
           END-PERFORM
           .
      *
       0X3100-SAIR.
           EXIT SECTION
           .
