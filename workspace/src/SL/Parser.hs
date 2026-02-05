{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analisador sintático para a linguagem SL
module SL.Parser
  ( -- * Parser principal
    parseProgram,

    -- * Parsers auxiliares exportados para testes
    parseExpr,
    parseStmt,
    parseType,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Text qualified as T
import SL.AST
import SL.Lexer
import Text.Megaparsec

--------------------------------------------------------------------------------
-- Programa
--------------------------------------------------------------------------------

parseProgram :: Parser Program
parseProgram = Program <$> (sc *> many parseTopLevel <* eof) <?> "programa"

--------------------------------------------------------------------------------
-- Declarações Top-Level
--------------------------------------------------------------------------------

parseTopLevel :: Parser TopLevelDecl
parseTopLevel = parseFuncDecl <|> parseStructDecl <?> "declaracao"

parseFuncDecl :: Parser TopLevelDecl
parseFuncDecl = label "funcao" $ do
  tvs <- parseForall <|> pure []
  _ <- rFunc
  name <- ident
  params <- parens (parseParam `sepBy` comma)
  ret <- optional (colon *> parseType)
  FuncDecl tvs name params ret <$> parseBlock

parseForall :: Parser [TypeVar]
parseForall = label "quantificador forall" . try $ rForall *> some ident <* void (symbol ".")

parseParam :: Parser Param
parseParam = Param <$> ident <*> optional (colon *> parseType) <?> "parametro"

parseStructDecl :: Parser TopLevelDecl
parseStructDecl = StructDecl <$> (rStruct *> ident) <*> braces (many parseField) <?> "struct"

parseField :: Parser Field
parseField = Field <$> ident <*> (colon *> parseType <* semicolon) <?> "campo"

--------------------------------------------------------------------------------
-- Tipos
--------------------------------------------------------------------------------

parseType :: Parser Type
parseType = do
  base <- parseBaseType
  foldArrays base
  where
    foldArrays t = (brackets (optional intLiteral) *> foldArrays (TArr t)) <|> pure t

parseBaseType :: Parser Type
parseBaseType =
  choice
    [ TInt <$ rInt,
      TFloat <$ rFloat,
      TString <$ rString,
      TBool <$ rBool,
      TVoid <$ rVoid,
      parseFuncType,
      TRecord . T.unpack <$> identifier
    ]
    <?> "tipo"

parseFuncType :: Parser Type
parseFuncType =
  label "tipo funcao" . try $
    TFunc
      <$> parens (parseType `sepBy` comma)
      <*> (arrow *> parseType)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

parseBlock :: Parser Block
parseBlock = braces (many parseStmt) <?> "bloco"

parseStmt :: Parser Stmt
parseStmt =
  choice
    [ parseVarDecl,
      parseIf,
      parseWhile,
      parseFor,
      parseReturn,
      try parseAssign,
      parseExprStmt
    ]
    <?> "statement"

parseVarDecl :: Parser Stmt
parseVarDecl =
  SVarDecl
    <$> (rLet *> ident)
    <*> optional (colon *> parseType)
    <*> optional (opAssign *> parseExpr)
    <* semicolon
    <?> "declaracao de variavel"

parseIf :: Parser Stmt
parseIf =
  SIf
    <$> (rIf *> parens parseExpr)
    <*> parseBlock
    <*> optional (rElse *> parseBlock)
    <?> "if"

parseWhile :: Parser Stmt
parseWhile =
  SWhile
    <$> (rWhile *> parens parseExpr)
    <*> parseBlock
    <?> "while"

parseFor :: Parser Stmt
parseFor =
  do
    _ <- rFor *> lparen
    var <- ident
    _ <- opAssign
    ini <- parseExpr
    _ <- semicolon
    cond <- parseExpr
    _ <- semicolon
    incr <- parseForIncr
    _ <- rparen
    SFor var ini cond incr <$> parseBlock
    <?> "for"
  where
    parseForIncr = try incrAssign <|> incrPostfix
    incrAssign = identifier *> opAssign *> parseExpr
    incrPostfix = do
      base <- EVar . T.unpack <$> identifier
      op <- (OpPostInc <$ opIncrement) <|> (OpPostDec <$ opDecrement)
      pure $ EUnaryOp op base

parseReturn :: Parser Stmt
parseReturn = SReturn <$> (rReturn *> optional parseExpr <* semicolon) <?> "return"

parseAssign :: Parser Stmt
parseAssign = SAssign <$> parseLValue <*> (opAssign *> parseExpr <* semicolon) <?> "atribuicao"

parseLValue :: Parser Expr
parseLValue = (identifier >>= parsePostfixOps . EVar . T.unpack) <?> "lvalue"

parseExprStmt :: Parser Stmt
parseExprStmt = SExpr <$> (parseExpr <* semicolon) <?> "expressao"

--------------------------------------------------------------------------------
-- Expressões
--------------------------------------------------------------------------------

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable <?> "expressao"

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (EUnaryOp OpNeg),
      prefix "!" (EUnaryOp OpNot)
    ],
    [ infixL "*" (EBinOp OpMul),
      infixL "/" (EBinOp OpDiv)
    ],
    [ infixL "+" (EBinOp OpAdd),
      infixL "-" (EBinOp OpSub)
    ],
    [ infixL "<=" (EBinOp OpLe),
      infixL ">=" (EBinOp OpGe),
      infixL "<" (EBinOp OpLt),
      infixL ">" (EBinOp OpGt)
    ],
    [ infixL "==" (EBinOp OpEq),
      infixL "!=" (EBinOp OpNeq)
    ],
    [ infixL "&&" (EBinOp OpAnd)
    ],
    [ infixL "||" (EBinOp OpOr)
    ]
  ]

infixL :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixL name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

parseTerm :: Parser Expr
parseTerm = parseAtom >>= parsePostfixOps

parsePostfixOps :: Expr -> Parser Expr
parsePostfixOps base = do
  mop <- optional postfixOp
  case mop of
    Nothing -> pure base
    Just f -> parsePostfixOps (f base)
  where
    postfixOp =
      choice
        [ flip EArrayAccess <$> brackets parseExpr,
          flip EFieldAccess . T.unpack <$> (dot *> identifier),
          mkCall <$> parens (parseExpr `sepBy` comma),
          EUnaryOp OpPostInc <$ opIncrement,
          EUnaryOp OpPostDec <$ opDecrement
        ]
    mkCall args = \case
      EVar name -> ECall name args
      _ -> ECall "" args

parseAtom :: Parser Expr
parseAtom =
  choice
    [ parens parseExpr,
      parseNewArray,
      parseArrayLit,
      try parseStructLit,
      try (EFloat <$> floatLiteral),
      EInt <$> intLiteral,
      EString . T.unpack <$> stringLiteral,
      EBool <$> boolLiteral,
      EVar . T.unpack <$> identifier
    ]
    <?> "expressão atômica"

parseNewArray :: Parser Expr
parseNewArray = ENewArray <$> (rNew *> parseBaseType) <*> brackets parseExpr <?> "new array"

parseArrayLit :: Parser Expr
parseArrayLit = EArrayLit <$> brackets (parseExpr `sepBy` comma) <?> "literal de array"

parseStructLit :: Parser Expr
parseStructLit = EStructLit <$> ident <*> braces (parseExpr `sepBy` comma) <?> "literal de struct"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

ident :: Parser String
ident = T.unpack <$> identifier
