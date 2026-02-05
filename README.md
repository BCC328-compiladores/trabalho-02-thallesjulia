# Compilador SL - BCC328

Trabalho pratico de Construcao de Compiladores I (BCC328 - DECOM/UFOP).

Compilador para a linguagem SL implementado em Haskell.

## Requisitos

- Docker e Docker Compose

## Iniciando o ambiente

```bash
docker-compose up -d
docker-compose exec sl bash
```

## Compilando o projeto

Dentro do container:

```bash
# Compilar o projeto
make code

# Compilar e testar
make all

# Limpar artefatos de build
make clean
```

## Uso do compilador

```bash
# Analise lexica (tokens)
cabal run slc -- --lexer examples/factorial.sl

# Analise sintatica (AST)
cabal run slc -- --parser examples/factorial.sl

# Pretty printer
cabal run slc -- --pretty examples/factorial.sl
```

## Executando testes

```bash
# Testes unitarios
make test

# Testar todos os exemplos (lexer, parser, pretty, testes unitarios)
bash test-all.sh
```

## Estrutura do projeto

```text
workspace/
  src/SL/
    AST.hs      - Arvore de Sintaxe Abstrata (parametrizada)
    Lexer.hs     - Analisador Lexico (Megaparsec)
    Parser.hs    - Analisador Sintatico
    Pretty.hs    - Pretty Printer
  app/
    Main.hs      - Interface de linha de comando (slc)
  test/
    Spec.hs      - Testes automatizados (111 testes)
  examples/      - Programas de exemplo em SL
```

## Funcionalidades implementadas

- Analise lexica com suporte a palavras reservadas, identificadores, literais, operadores e delimitadores
- Analise sintatica com suporte a funcoes, structs, arrays, generics (forall), tipos funcao, for/while/if-else
- AST parametrizada por anotacoes para futuras etapas (tipos, spans)
- Pretty printer com round-trip (parse -> pretty -> parse)
- Operadores pos-fixos (i++, i--)
- Inferencia de tipos (parametros sem anotacao)

## Autores

- Thalles Felipe
- Julia Gonzaga
