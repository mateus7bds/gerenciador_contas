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
           RECORD KEY IS REGISTRO-GERAL-CONTAS
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
       01  REGISTRO-GERAL-CONTAS.
           03  ID-CONTA          PIC  9(008).
           03  RESTANTE          PIC  X(071).
      *
       01  REGISTRO-CONTAS REDEFINES REGISTRO-GERAL-CONTAS.
           03  AGENCIA           PIC  9(008).
           03  CONTA             PIC  9(008).
           03  DV-AGENCIA        PIC  X(001).
           03  DV-CONTA          PIC  X(001).
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
       01  NUMEROS                  PIC  X(010) VALUE "0123456789".
       01  FLAG-NUMERO              PIC  9(001) VALUE ZERO.
      * Variaveis - Validacao do CPF
       01  VRV-VLDC-CPF.
           03  CPF-ENTD              PIC  9(011) VALUE ZEROS.
           03  FILLER-CPF REDEFINES CPF-ENTD.
               05  BASE-CPF-ENTD     PIC  9(009).
               05  DV-CPF-ENTD       PIC  X(002).
           03  DV-CLCD-CMT           PIC  X(002) VALUE ZEROS.
           03  VRV-CLC-SMAT.
               05  SMAT-PESOS             PIC  9(005) VALUE ZEROS.
               05  PESO-DGTO              PIC  9(002) VALUE ZEROS.
               05  PSC-DGTO-ULT-CLC       PIC  9(002) VALUE ZEROS.
               05  RESTO-CLCD-DV          PIC  9(002) VALUE ZEROS.
               05  PESO-CLCD              PIC  9(004) VALUE ZEROS.
               05  DV-CLCD                PIC  9(001) VALUE ZEROS.
      * ATENCAO: essa variavel ja existe, vou refatorar o programa depois
      *         05  FILLER-QUOCIENTE       PIC  9(002) VALUE ZEROS.
           03  DGTO-ATUAL                 PIC  9(001) VALUE ZEROS.
           03  VRV-DGTO-CLCD.
               05  PRMO-DV                PIC  9(001) VALUE ZEROS.
               05  SGDO-DV                PIC  9(001) VALUE ZEROS.
      *
       01  VRV-VLDC-DATA.
           03  DIA-VLDC                   PIC  9(002) VALUE ZEROS.
           03  MES-VLDC                   PIC  9(002) VALUE ZEROS.
           03  ANO-VLDC                   PIC  9(004) VALUE ZEROS.
      *
       01  DIA-LIMITE-MES                 PIC  9(002) VALUE ZEROS.
      *
       01  VRV-CLCD-DIAS-MES-2.
           03  RESTO-400                  PIC  9(002) VALUE ZEROS.
           03  RESTO-100                  PIC  9(002) VALUE ZEROS.
           03  RESTO-4                    PIC  9(002) VALUE ZEROS.
      *
       01  FIM-ARQ-CONTAS                 PIC  X(001).
           88  SIM-FIM                                VALUE "S".
           88  NAO-FIM                                VALUE "N".
      *
       01  ID-ULTIMA-CT                   PIC  9(004) VALUE ZEROS.
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
               05  S0001-OPERACAO          PIC  9(002) VALUE 2.
               05  S0001-AGENCIA           PIC  9(004) VALUE 1234.
               05  S0001-DV-AGENCIA        PIC  X(001) VALUE "3".
               05  S0001-CONTA             PIC  9(008) VALUE 12345678.
               05  S0001-DV-CONTA          PIC  X(001) VALUE "9".
               05  S0001-NOME              PIC  X(080) VALUE
                   "    MATEUS   BARBOSA  DA  SILVA".
               05  S0001-CPF               PIC  9(011) VALUE
                   18727199703.
               05  S0001-DATA-NASCIMENTO   PIC  X(008)
                   VALUE "11032002".
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
                   PERFORM 040000-ATUALIZAR-CONTA
               WHEN 3
      *             PERFORM 040000-EXCLUIR-CC
                    DISPLAY 'EXCLUIR CONTA CORRENTE'
               WHEN OTHER
                   DISPLAY 'OPCAO INVALIDA'
           END-EVALUATE
           .
      *
       000000-SAIR-PGM.
           DISPLAY 'S0001-CD-RTN....: ' S0001-CD-RTN
           DISPLAY 'S0001-TX-MSG-RTN: ' S0001-TX-MSG-RTN
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
                  '\cobol\gerenciador_contas\'  DELIMITED BY SIZE
                  'arq_contas.idx'              DELIMITED BY SIZE
           INTO CAMINHO-ARQ-CONTAS
      *
      * Tenta abrir o arquivo apenas para consulta e verificar se existe
           OPEN I-O ARQ-CONTAS
      * Verifica se o arquivo não existe (35)
           IF STATUS-ARQ-CONTAS EQUAL "35"
               CLOSE ARQ-CONTAS
      * cria arquivo, se nao existir
               OPEN OUTPUT ARQ-CONTAS
               CLOSE ARQ-CONTAS
      * abre o arquivo ja criado
               OPEN I-O ARQ-CONTAS
           END-IF
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
      *
           PERFORM 032000-INCLUIR-CONTA
           .
      *
       030000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       040000-ATUALIZAR-CONTA SECTION.
      *------------------------------------------------------------------------
      *
           PERFORM 0X1000-VALIDAR-AGENCIA
           PERFORM 0X2000-VALIDAR-CONTA
      *
           PERFORM 0X3100-VALIDAR-CCT-NOME
           PERFORM 0X3200-TRATAR-NOME
      *
           PERFORM 0X3400-VALIDAR-DATA
      *
           PERFORM 041000-EFETIVAR-ATL-CT
           .
      *
       040000-SAIR.
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
           MOVE S0001-CPF TO CPF-ENTD
           PERFORM 0X3300-VALIDAR-CPF
           DISPLAY 'TODOS OS DADOS VALIDADOS ATE AGORA'
           PERFORM 0X3400-VALIDAR-DATA
      * ATENCAO: validacao da data
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
           DISPLAY "Digito verificador da agência: " DV-CALCULADO
      *
           IF S0001-DV-AGENCIA NOT EQUAL DV-CALCULADO
               MOVE 02 TO S0001-CD-RTN
               MOVE "CTCS0001 - Dígito verificador da agência inválida."
                   TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
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
           DISPLAY "Digito verificador da conta: " DV-CALCULADO
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
      * inicializo a string de saida
           MOVE SPACES TO STRING-SAID
      * considero que o caracter anterior nao eh espaco
           MOVE ZERO TO FLAG-ESP
      * itero sobre cada caracter da string
           PERFORM VARYING IC-ITRA FROM 1 BY 1
               UNTIL IC-ITRA > LENGTH OF S0001-NOME
      * extraio o caracter
               MOVE S0001-NOME(IC-ITRA:1) TO CCT-STRING
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
                       STRING STRING-SAID   DELIMITED BY SPACE
                              "#"           DELIMITED BY SIZE
                              CCT-STRING    DELIMITED BY SIZE
                              INTO STRING-SAID
                   ELSE
      * string saida = string saida + cct atual
                       STRING STRING-SAID   DELIMITED BY SPACE
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
               MOVE ZERO TO FLAG-LETRA
               INSPECT ALFABETO TALLYING FLAG-LETRA
                   FOR ALL CCT-STRING
      * se nao for letra nem espaco, lanca-se um erro
               IF (FLAG-LETRA NOT EQUAL 1) AND
                  (CCT-STRING NOT EQUAL SPACE)
                   MOVE 06 TO S0001-CD-RTN
                   STRING "CTCS0001 - Foram encontrados "
                          "caracteres invalidos "
                          "no nome do cliente."
                      DELIMITED BY SIZE INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
               END-IF
           END-PERFORM
           .
      *
       0X3100-SAIR.
           EXIT SECTION
           .
      *-----------------------------------------------------------------
       0X3300-VALIDAR-CPF SECTION.
      * ----------------------------------------------------------------
      * Entrada: CPF-ENTD
      * Saida..: sem saida
      *
      * Extrai os digitos verificadores e compara os que são calculados
      * a seguir
      *
      * Entrada: SMAT-PESOS, PESO-DGTO, PSC-DGTO-ULT-CLC
      * Saida>.: DV-CLCD-CMT
      *
      * 1 - Calculo do primeiro digito verificador
      *
           MOVE 00 TO SMAT-PESOS
           MOVE 10 TO PESO-DGTO
           MOVE 09 TO PSC-DGTO-ULT-CLC
           PERFORM 0X3310-CALCULAR-DV
           MOVE DV-CLCD TO PRMO-DV
      *
      * 2 - Calcular o segundo digito verificador
      *
           MOVE 00 TO SMAT-PESOS
           MOVE 11 TO PESO-DGTO
           MOVE 10 TO PSC-DGTO-ULT-CLC
           PERFORM 0X3310-CALCULAR-DV
           MOVE DV-CLCD TO SGDO-DV
      *
           MOVE PRMO-DV TO DV-CLCD-CMT(1:1)
           MOVE SGDO-DV TO DV-CLCD-CMT(2:1)
      *
           IF DV-CLCD-CMT NOT EQUAL DV-CPF-ENTD
               MOVE 07 TO S0001-CD-RTN
               MOVE "CTCS0001 - CPF invalido." TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
           .
      *
       0X3300-SAIR.
           EXIT SECTION
           .
      * ----------------------------------------------------------------
       0X3310-CALCULAR-DV SECTION.
      * ----------------------------------------------------------------
      * Entrada:
      *
      *    - SMAT-PESOS       PIC  9(005)
      *    - PESO-DGTO        PIC  9(002)
      *      PSC-DGTO-ULT-CLC PIC  9(002)
      * Saida:
      *    - DV-CLCD          PIC  9(001)
      *
           PERFORM VARYING IC-ITRA FROM 1 BY 1
               UNTIL IC-ITRA > PSC-DGTO-ULT-CLC
      * se for o calculo do segundo digito verificador (inclui
      * o primeiro digito verificador calculado - 10º posicao)
               IF IC-ITRA EQUAL 10 AND PSC-DGTO-ULT-CLC EQUAL 10
                   MOVE PRMO-DV TO DGTO-ATUAL
               ELSE
                   MOVE BASE-CPF-ENTD(IC-ITRA:1) TO DGTO-ATUAL
               END-IF
      * multiplica o ditito atual com o peso correspondente
               MULTIPLY DGTO-ATUAL BY PESO-DGTO GIVING PESO-CLCD
      * adiciona ao somatorio de pesos
               ADD PESO-CLCD TO SMAT-PESOS
      * subtrai 1 do peso do digito para a proxima iteracao
               SUBTRACT 1 FROM PESO-DGTO
           END-PERFORM
      *
           DIVIDE SMAT-PESOS BY 11 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO-CLCD-DV
      *
           SUBTRACT 11 FROM RESTO-CLCD-DV
      *
           IF RESTO-CLCD-DV EQUAL 10
               MOVE 00 TO RESTO-CLCD-DV
           END-IF
      *
           MOVE RESTO-CLCD-DV TO DV-CLCD
           .
      *
       0X3310-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X3400-VALIDAR-DATA SECTION.
      *------------------------------------------------------------------------
      * verifica os caracteres da data
           PERFORM VARYING IC-ITRA FROM 1 BY 1
               UNTIL IC-ITRA > LENGTH OF S0001-DATA-NASCIMENTO
               MOVE S0001-DATA-NASCIMENTO(IC-ITRA:1) TO CCT-STRING
               MOVE ZERO TO FLAG-NUMERO
               INSPECT NUMEROS TALLYING FLAG-NUMERO FOR ALL CCT-STRING
               IF FLAG-NUMERO NOT EQUAL 1
                   MOVE 08 TO S0001-CD-RTN
                   STRING "CTCS0001 - Caracter invalido na data de "
                          "nascimento." DELIMITED BY SIZE
                          INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
               END-IF
           END-PERFORM
      * extrai o dia, mes e ano da data de entrada
           MOVE S0001-DATA-NASCIMENTO TO VRV-VLDC-DATA
      * obtem o numero maximo de dias do mes
           PERFORM 0X3410-OBTER-QTD-DIAS-MES
      * verifica, de acordo com o mes, se o dia esta dentro da faixa
      * limite de dias do mes
           IF DIA-VLDC < 1 OR DIA-VLDC > DIA-LIMITE-MES
               MOVE 10 TO S0001-CD-RTN
               STRING "CTCS0001 - Quantidade de dias do"
                      " mês torna a data invalida." DELIMITED BY SIZE
                      INTO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
      * verifica se o ano de nascimento eh anterior ao ano vigente
           IF ANO-VLDC >= ANO OF DIA-HORARIO-CORRENTE
               MOVE 11 TO S0001-CD-RTN
               STRING "CTCS0001 - Ano de nascimetno deve "
                      "ser menor que o ano vigente." DELIMITED BY SIZE
                      INTO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
           .
      *
       0X3400-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X3410-OBTER-QTD-DIAS-MES SECTION.
      *------------------------------------------------------------------------
      * Entrada: MES-VLDC
      * Saida..: DIA-LIMITE-MES
      *
           EVALUATE MES-VLDC
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO DIA-LIMITE-MES
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO DIA-LIMITE-MES
               WHEN 2
                   PERFORM 0X3411-CALCULAR-DIAS-MES-2
               WHEN OTHER
                   MOVE 09 TO S0001-CD-RTN
                   MOVE "CTCS0001 - Mes da data de nascimento invalido."
                      TO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
           END-EVALUATE
           .
      *
       0X3410-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       0X3411-CALCULAR-DIAS-MES-2 SECTION.
      *------------------------------------------------------------------------
      * Entrada: ANO-VLDC
      * Saida..: DIA-LIMITE-MES
           MOVE ZEROS TO FILLER-QUOCIENTE
           DIVIDE ANO-VLDC BY 400 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO-400
      *
           MOVE ZEROS TO FILLER-QUOCIENTE
           DIVIDE ANO-VLDC BY 100 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO-100
      *
           MOVE ZEROS TO FILLER-QUOCIENTE
           DIVIDE ANO-VLDC BY 4 GIVING FILLER-QUOCIENTE
               REMAINDER RESTO-4
      *
           IF (RESTO-4 EQUAL 0 AND RESTO-100 NOT EQUAL 100) OR
               RESTO-400 EQUAL 0
               MOVE 29 TO DIA-LIMITE-MES
           ELSE
               MOVE 28 TO DIA-LIMITE-MES
           END-IF
           .
      *
       0X3411-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       032000-INCLUIR-CONTA SECTION.
      *------------------------------------------------------------------------
       DISPLAY 'ENTREI 032000-INCLUIR-CONTA'
      * Move a agencia e a conta para posterior pesquisa na base de
      * dados
           MOVE S0001-AGENCIA         TO AGENCIA
           MOVE S0001-CONTA           TO CONTA
      * Procura nos registros, se ha algum registro com a conta
      * registrada
           READ ARQ-CONTAS
               KEY IS ID-CONTA
      * se encontrar algum registro com a agencia e a conta ja
      * cadastrada, lanca-se um erro
               NOT INVALID KEY
                   MOVE 10 TO S0001-CD-RTN
                   STRING "CTCS0001 - Conta ja esta cadastrada. "
                          " - STATUS-CODE=" STATUS-ARQ-CONTAS "."
                          DELIMITED BY SIZE
                       INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
           END-READ
      * movendo para a variavel que corresponde ao registro os dados
      * para posterior armazenamento
           MOVE S0001-AGENCIA         TO AGENCIA
           MOVE S0001-DV-AGENCIA      TO DV-AGENCIA
           MOVE S0001-CONTA           TO CONTA
           MOVE S0001-DV-CONTA        TO DV-CONTA
           MOVE S0001-NOME            TO NOME
           MOVE S0001-CPF             TO CPF
           MOVE S0001-DATA-NASCIMENTO TO DATA-NASCIMENTO
           MOVE ZEROS                 TO SALDO
      * salvando os dados da conta
           WRITE REGISTRO-GERAL-CONTAS
               INVALID KEY
                   MOVE 11 TO S0001-CD-RTN
                   STRING "CTCS0001 - Erro ao salvar a conta."
                          " - STATUS CODE=" STATUS-ARQ-CONTAS "."
                          DELIMITED BY SIZE INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
               NOT INVALID KEY
                   MOVE ZEROS TO S0001-CD-RTN
                   MOVE "CTCS0001 - Conta criada com sucesso." TO
                       S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
           END-WRITE
      *
           .
      *
       032000-SAIR.
           EXIT SECTION
           .
      *------------------------------------------------------------------------
       041000-EFETIVAR-ATL-CT SECTION.
      *------------------------------------------------------------------------
           DISPLAY 'EFETIVAR ATUALIZACAO CONTA'
      *
      * define a chave de busca da conta na base dados
      *
           MOVE S0001-AGENCIA TO AGENCIA
           MOVE S0001-CONTA   TO CONTA
      *
      * procura a conta no arquivo
      *
      *     READ ARQ-CONTAS
      *         KEY IS CONTA-CHAVE
      *         INVALID KEY
      *             MOVE 11 TO S0001-CD-RTN
      *             MOVE "CTCS0001 - Conta nao existente."
      *                 TO S0001-TX-MSG-RTN
      *             PERFORM 000000-SAIR-PGM
      *         NOT INVALID KEY
      *             DISPLAY REGISTRO-CONTAS
      *     END-READ
      *
           READ ARQ-CONTAS
               KEY IS ID-CONTA
           END-READ
      *
           IF STATUS-ARQ-CONTAS NOT EQUAL "00"
               MOVE 11 TO S0001-CD-RTN
               MOVE "CTCS0001 - Conta nao existente."
                  TO S0001-TX-MSG-RTN
               PERFORM 000000-SAIR-PGM
           END-IF
      *
      * mantem os dados anteriores, so atualizando o nome e
      * a data de nascimento
      *
           DISPLAY 'REGISTRO-GERAL-CONTAS: ' REGISTRO-GERAL-CONTAS
      *
           MOVE S0001-NOME            TO NOME
           MOVE S0001-DATA-NASCIMENTO TO DATA-NASCIMENTO
      *
           DISPLAY 'REGISTRO-GERAL-CONTAS: ' REGISTRO-GERAL-CONTAS
      *
           REWRITE REGISTRO-GERAL-CONTAS
               INVALID KEY
                   MOVE 12 TO S0001-CD-RTN
                   STRING "CTCS0001 - Erro ao atualizar a conta. "
                          " - STATUS CODE=" STATUS-ARQ-CONTAS "."
                          DELIMITED BY SIZE INTO S0001-TX-MSG-RTN
                   PERFORM 000000-SAIR-PGM
               NOT INVALID KEY
                   MOVE 0 TO S0001-CD-RTN
                   MOVE "CTCS0001 - Conta atualizada com sucesso." TO
                      S0001-TX-MSG-RTN
           END-REWRITE
           .
      *
       041000-SAIR.
           EXIT SECTION
           .
