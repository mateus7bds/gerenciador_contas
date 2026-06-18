
#interna

# Gerenciador de Contas Simples - OpenCOBOL

## Descrição do Produto

O gerenciador de contas é um conjunto de programas que quando executados, simulam um sistema responsável por gerenciar contas e permitir operações financeiras básicas como saques, débitos e transferências intrabancárias simples.

## Justificativa do Projeto

O gerenciador de contas é um projeto que simula, de forma bem simples, operações financeiras intrabancárias (entre contas do mesmo banco) básicas.

## Finalidade do Projeto

O gerenciador de contas é um projeto que utilizarei para aplicar alguns conhecimentos que obtive na minha carreira profissional (como programador COBOL), além de servir de base para outros projetos futuros.

## Escopo

- Criação, exclusão e exclusão de contas bancárias simples;
- Realizar movimentações de contas;
- Uso de datasets/arquivos como base de dados;

## Fora do Escopo

- Interface gráfica (tela);
- Uso de banco de dados relacional (usa apenas arquivos);
- Processamento em tempo real (online) - CICS;
- Processamento batch via JCL;

## Tecnologias utilizadas

- **COBOL:** linguagem de programação principal;
- **Datasets:** arquivos Windows que simulam datasets em mainframes;
- **GnuCOBOL/OpenCOBOL:** ambiente de desenvolvimento, testes e execução;

## Níveis de Prioridade dos Requisitos

Os requisitos são classificados em três categorias:
- **Obrigatório:** o sistema não funciona sem este requisito. É imprescindível e deve ser implementado obrigatoriamente.
- **Necessário:** o sistema funciona, mas de forma insatisfatória sem este requisito. Deve ser implementado, mas o sistema ainda pode ser usado sem ele.
- **Opcional:** o sistema funciona bem sem este requisito. Pode ser deixado para uma versão futura, se não houver tempo para implementá-lo agora.


## Requisitos não funcionais

---
**RNF001** – Template padrão de documentação interna de programas COBOL. Este template deve ficar entre a declaração PROGRAM-ID e o ENVIRONMENT DIVISION.
```cobol
* PROGRAMADOR: Nome do desenvolvedor.
* SIGLA......: Sigla – Nome da sigla (ou sistema).
* OBJETIVO...: Objetivo do programa.
* DATA DE CRIACAO: Data de criação do programa.
```
---

**RNF002** - Códigos de retorno deve ser múltiplos de 2.

---

**RNF003** - Programas deve sempre retornar o código de retorno, o texto da mensagem de retorno, o file status (se houver operações envolvendo datasets) e o texto que detalha o file status.

## Requisitos Funcionais

### RF0001 - Permitir criação, exclusão e atualização de contas bancárias

**Descrição:** o sistema deve possuir um módulo que seja responsável por criar, excluir ou atualizar contas bancárias.

**Pré-condição e Entrada**
- Operações a serem inseridas:
  - criação;
  - exclusão;
  - atualização;
- Dados necessários para a criação da conta:
  - Nome;
  - CPF;
  - Data de nascimento;
  - Agência;
  - Digito verificador da agência;
  - Conta;
  - Dígito verificador da conta;
- Dados que podem ser atualizados em uma conta:
  - Nome;
  - Data de nascimento;
- A busca pela conta a ser excluída ou atualizada deve ser feita exclusivamente pela conta e agência da conta bancária;
- Deve-se realizar uma validação antes da execução das operações citadas;

**Pós-condições e Saída**
- Mensagem de operação bem-sucedida;

### RF002 – Permitir depositar na conta

**Descrição:** o sistema deve possuir um módulo que realize o depósito na conta e salve os dados do depósito na base de dados.

**Pré-condição e Entrada**

- Operações a serem criadas:
  - Depósito em uma conta existente;
  - Registro do depósito deve ser salvo;

- Dados necessários para a realização do depósito:
  - Agência;
  - Conta;
  - Valor do depósito;

**Pós-condição e Saída**
- Mensagem resultante da operação;

### RF003 – Permitir realizar registro de saque na conta

**Descrição:** o sistema deve possuir um módulo que realize o registro de saques na conta.

**Pré-condição e Entrada**

- Operações a serem criadas:
  - Registro de saque em determinada conta;
- Dados necessários para a realização do depósito:  
  - Agência;
  - Conta;
  - Valor do depósito;

**Pós-condição e Saida**
- Mensagem resultante da operação;

### RF004 – Programa que permite a realização de transferências intrabancárias (internas) – Cliente interno para cliente interno do banco

**Descrição:** o sistema deve possuir um programa que realize a transferência de valores entre contas cadastradas no banco.

**Pré-condição**

- Operações a serem criadas:
  - Transferência de valor entre contas cadastradas no próprio banco;
    - De cliente interno para cliente interno;
- Dados de entrada necessários:
  - Cliente ordenante:
    - Agência;
    - Conta;
    - Valor;
  - Cliente beneficiário:
    - Agência;
    - Conta;
- Validar dados de entrada;
  - Se as contas e agências possuem dígitos verificadores válidos;
  - Verificar se as contas realmente existem;
  - Verificar se o saldo supera o limite diário;

**Pós-condição e Saída**
- Mensagem resultante da operação;

Clique aqui para download do **[OPENCOBOL Ide] (https://launchpad.net/cobcide/+download)**
