{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analisador léxico para a linguagem SL
module SL.Lexer
  ( -- * Tipos
    Parser,
    SLToken (..),
    LocatedToken (..),

    -- * Espaços e comentários
    sc,

    -- * Combinadores básicos
    lexeme,
    symbol,

    -- * Palavras reservadas
    rFunc,
    rStruct,
    rLet,
    rIf,
    rElse,
    rWhile,
    rFor,
    rReturn,
    rNew,
    rTrue,
    rFalse,
    rForall,
    rInt,
    rFloat,
    rString,
    rBool,
    rVoid,

    -- * Identificadores e literais
    identifier,
    intLiteral,
    floatLiteral,
    numberLiteral,
    stringLiteral,
    boolLiteral,

    -- * Operadores
    opPlus,
    opMinus,
    opStar,
    opSlash,
    opIncrement,
    opDecrement,
    opEq,
    opNeq,
    opLe,
    opGe,
    opLt,
    opGt,
    opAnd,
    opOr,
    opNot,
    opAssign,

    -- * Delimitadores
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    parens,
    braces,
    brackets,

    -- * Pontuação
    comma,
    colon,
    semicolon,
    dot,
    arrow,

    -- * Lexer completo
    lexAll,
    formatToken,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
-- Tipos
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------
-- Espaços e Comentários
--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

--------------------------------------------------------------------------------
-- Palavras Reservadas
--------------------------------------------------------------------------------

reservedWords :: [Text]
reservedWords =
  [ "func",
    "struct",
    "let",
    "if",
    "else",
    "while",
    "for",
    "return",
    "new",
    "true",
    "false",
    "forall",
    "int",
    "float",
    "string",
    "bool",
    "void"
  ]

reserved :: Text -> Parser ()
reserved w = lexeme . try $ string w *> notFollowedBy alphaNumChar

rFunc, rStruct, rLet, rIf, rElse, rWhile, rFor, rReturn :: Parser ()
rFunc = reserved "func"
rStruct = reserved "struct"
rLet = reserved "let"
rIf = reserved "if"
rElse = reserved "else"
rWhile = reserved "while"
rFor = reserved "for"
rReturn = reserved "return"

rNew, rTrue, rFalse, rForall :: Parser ()
rNew = reserved "new"
rTrue = reserved "true"
rFalse = reserved "false"
rForall = reserved "forall"

rInt, rFloat, rString, rBool, rVoid :: Parser ()
rInt = reserved "int"
rFloat = reserved "float"
rString = reserved "string"
rBool = reserved "bool"
rVoid = reserved "void"

--------------------------------------------------------------------------------
-- Identificadores
--------------------------------------------------------------------------------

identifier :: Parser Text
identifier = lexeme . try $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  let ident = T.pack (c : cs)
  if ident `elem` reservedWords
    then fail $ "palavra reservada '" <> T.unpack ident <> "' não é identificador"
    else pure ident

--------------------------------------------------------------------------------
-- Literais
--------------------------------------------------------------------------------

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

floatLiteral :: Parser Double
floatLiteral = lexeme L.float

numberLiteral :: Parser (Either Int Double)
numberLiteral = lexeme $ try (Right <$> L.float) <|> (Left <$> L.decimal)

stringLiteral :: Parser Text
stringLiteral = lexeme $ char '"' *> (T.pack <$> many strChar) <* char '"'
  where
    strChar = noneOf ['"', '\\'] <|> escChar
    escChar =
      char '\\'
        *> choice
          [ '"' <$ char '"',
            '\\' <$ char '\\',
            '\n' <$ char 'n',
            '\t' <$ char 't',
            '\r' <$ char 'r'
          ]

boolLiteral :: Parser Bool
boolLiteral = True <$ rTrue <|> False <$ rFalse

--------------------------------------------------------------------------------
-- Operadores
--------------------------------------------------------------------------------

opPlus, opMinus, opStar, opSlash :: Parser Text
opPlus = symbol "+"
opMinus = symbol "-"
opStar = symbol "*"
opSlash = symbol "/"

opIncrement, opDecrement :: Parser Text
opIncrement = symbol "++"
opDecrement = symbol "--"

opEq, opNeq, opLe, opGe, opLt, opGt :: Parser Text
opEq = symbol "=="
opNeq = symbol "!="
opLe = symbol "<="
opGe = symbol ">="
opLt = symbol "<"
opGt = symbol ">"

opAnd, opOr, opNot :: Parser Text
opAnd = symbol "&&"
opOr = symbol "||"
opNot = symbol "!"

opAssign :: Parser Text
opAssign = symbol "="

--------------------------------------------------------------------------------
-- Delimitadores
--------------------------------------------------------------------------------

lparen, rparen, lbrace, rbrace, lbracket, rbracket :: Parser Text
lparen = symbol "("
rparen = symbol ")"
lbrace = symbol "{"
rbrace = symbol "}"
lbracket = symbol "["
rbracket = symbol "]"

parens, braces, brackets :: Parser a -> Parser a
parens = between lparen rparen
braces = between lbrace rbrace
brackets = between lbracket rbracket

--------------------------------------------------------------------------------
-- Pontuação
--------------------------------------------------------------------------------

comma, colon, semicolon, dot, arrow :: Parser Text
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
dot = symbol "."
arrow = symbol "->"

--------------------------------------------------------------------------------
-- Tokens (para modo --lexer)
--------------------------------------------------------------------------------

data SLToken
  = -- Palavras reservadas
    TokFunc
  | TokStruct
  | TokLet
  | TokIf
  | TokElse
  | TokWhile
  | TokFor
  | TokReturn
  | TokNew
  | TokForall
  | -- Tipos
    TokTInt
  | TokTFloat
  | TokTString
  | TokTBool
  | TokTVoid
  | -- Literais
    TokIntLit Int
  | TokFloatLit Double
  | TokStringLit Text
  | TokBoolLit Bool
  | -- Identificador
    TokIdent Text
  | -- Operadores
    TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokIncrement
  | TokDecrement
  | TokEq
  | TokNeq
  | TokLt
  | TokGt
  | TokLe
  | TokGe
  | TokAnd
  | TokOr
  | TokNot
  | TokAssign
  | -- Delimitadores e pontuação
    TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokComma
  | TokColon
  | TokSemicolon
  | TokDot
  | TokArrow
  deriving (Show, Eq)

data LocatedToken = LocatedToken
  { tokPos :: SourcePos,
    tokValue :: SLToken
  }
  deriving (Show, Eq)

tokenParser :: Parser SLToken
tokenParser =
  choice
    [ TokFunc <$ rFunc,
      TokStruct <$ rStruct,
      TokLet <$ rLet,
      TokIf <$ rIf,
      TokElse <$ rElse,
      TokWhile <$ rWhile,
      TokFor <$ rFor,
      TokReturn <$ rReturn,
      TokNew <$ rNew,
      TokForall <$ rForall,
      TokTInt <$ rInt,
      TokTFloat <$ rFloat,
      TokTString <$ rString,
      TokTBool <$ rBool,
      TokTVoid <$ rVoid,
      TokBoolLit <$> boolLiteral,
      numberToToken <$> numberLiteral,
      TokStringLit <$> stringLiteral,
      TokIncrement <$ try (symbol "++"),
      TokDecrement <$ try (symbol "--"),
      TokEq <$ try (symbol "=="),
      TokNeq <$ try (symbol "!="),
      TokLe <$ try (symbol "<="),
      TokGe <$ try (symbol ">="),
      TokAnd <$ try (symbol "&&"),
      TokOr <$ try (symbol "||"),
      TokArrow <$ try (symbol "->"),
      TokAssign <$ symbol "=",
      TokPlus <$ symbol "+",
      TokMinus <$ symbol "-",
      TokStar <$ symbol "*",
      TokSlash <$ symbol "/",
      TokLt <$ symbol "<",
      TokGt <$ symbol ">",
      TokNot <$ symbol "!",
      TokLParen <$ lparen,
      TokRParen <$ rparen,
      TokLBrace <$ lbrace,
      TokRBrace <$ rbrace,
      TokLBracket <$ lbracket,
      TokRBracket <$ rbracket,
      TokComma <$ comma,
      TokColon <$ colon,
      TokSemicolon <$ semicolon,
      TokDot <$ dot,
      TokIdent <$> identifier
    ]
  where
    numberToToken (Left i) = TokIntLit i
    numberToToken (Right f) = TokFloatLit f

locatedToken :: Parser LocatedToken
locatedToken = LocatedToken <$> getSourcePos <*> tokenParser

lexAll :: Parser [LocatedToken]
lexAll = sc *> many locatedToken <* eof

--------------------------------------------------------------------------------
-- Formatação de tokens
--------------------------------------------------------------------------------

formatToken :: LocatedToken -> String
formatToken (LocatedToken pos tok) =
  show (unPos $ sourceLine pos) <> ":" <> show (unPos $ sourceColumn pos) <> " " <> showToken tok

showToken :: SLToken -> String
showToken = \case
  TokFunc -> "FUNC"
  TokStruct -> "STRUCT"
  TokLet -> "LET"
  TokIf -> "IF"
  TokElse -> "ELSE"
  TokWhile -> "WHILE"
  TokFor -> "FOR"
  TokReturn -> "RETURN"
  TokNew -> "NEW"
  TokForall -> "FORALL"
  TokTInt -> "TYPE_INT"
  TokTFloat -> "TYPE_FLOAT"
  TokTString -> "TYPE_STRING"
  TokTBool -> "TYPE_BOOL"
  TokTVoid -> "TYPE_VOID"
  TokIntLit n -> "INT(" <> show n <> ")"
  TokFloatLit f -> "FLOAT(" <> show f <> ")"
  TokStringLit s -> "STRING(\"" <> T.unpack s <> "\")"
  TokBoolLit b -> "BOOL(" <> show b <> ")"
  TokIdent s -> "IDENT(" <> T.unpack s <> ")"
  TokPlus -> "PLUS"
  TokMinus -> "MINUS"
  TokStar -> "STAR"
  TokSlash -> "SLASH"
  TokIncrement -> "INCREMENT"
  TokDecrement -> "DECREMENT"
  TokEq -> "EQ"
  TokNeq -> "NEQ"
  TokLt -> "LT"
  TokGt -> "GT"
  TokLe -> "LE"
  TokGe -> "GE"
  TokAnd -> "AND"
  TokOr -> "OR"
  TokNot -> "NOT"
  TokAssign -> "ASSIGN"
  TokLParen -> "LPAREN"
  TokRParen -> "RPAREN"
  TokLBrace -> "LBRACE"
  TokRBrace -> "RBRACE"
  TokLBracket -> "LBRACKET"
  TokRBracket -> "RBRACKET"
  TokComma -> "COMMA"
  TokColon -> "COLON"
  TokSemicolon -> "SEMICOLON"
  TokDot -> "DOT"
  TokArrow -> "ARROW"
