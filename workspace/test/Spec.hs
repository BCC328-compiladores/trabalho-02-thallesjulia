{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testes do compilador SL
module Main where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as T
import SL.AST
import SL.Lexer
import SL.Parser
import SL.Pretty (pp)
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (errorBundlePretty, parse)

--------------------------------------------------------------------------------
-- Infraestrutura
--------------------------------------------------------------------------------

data TestResult = Pass | Fail String deriving (Show, Eq)

runTest :: String -> Bool -> TestResult
runTest _ True = Pass
runTest msg False = Fail msg

data TestGroup = TestGroup String [TestResult]

runTestGroup :: TestGroup -> IO (Int, Int)
runTestGroup (TestGroup name results) = do
  putStrLn $ "\n=== " ++ name ++ " ==="
  let (passed, failed) = foldr count (0, 0) results
  mapM_ printRes (zip [1 :: Int ..] results)
  putStrLn $ "Resultado: " ++ show passed ++ "/" ++ show (passed + failed)
  pure (passed, failed)
  where
    count Pass (p, f) = (p + 1, f)
    count (Fail _) (p, f) = (p, f + 1)
    printRes (n, Pass) = putStrLn $ "  [OK] Teste " ++ show n
    printRes (n, Fail m) = putStrLn $ "  [FAIL] Teste " ++ show n ++ ": " ++ m

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

lexerOK, lexerFail :: Text -> Bool
lexerOK = isRight . parse lexAll ""
lexerFail = isLeft . parse lexAll ""

parserOK, parserFail :: Text -> Bool
parserOK = isRight . parse parseProgram ""
parserFail = isLeft . parse parseProgram ""

parseAST :: Text -> Either String Program
parseAST t = case parse parseProgram "" t of
  Left e -> Left (errorBundlePretty e)
  Right a -> Right a

parseExprTest :: Text -> Either String Expr
parseExprTest t = case parse (sc *> parseExpr) "" t of
  Left e -> Left (errorBundlePretty e)
  Right e -> Right e

countTokens :: Text -> Int
countTokens t = either (const 0) length (parse lexAll "" t)

hasToken :: (SLToken -> Bool) -> Text -> Bool
hasToken p t = either (const False) (any (p . tokValue)) (parse lexAll "" t)

--------------------------------------------------------------------------------
-- Testes do Lexer
--------------------------------------------------------------------------------

lexerTests :: TestGroup
lexerTests =
  TestGroup
    "Analise Lexica"
    [ runTest "Palavras reservadas" $
        all
          lexerOK
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
          ],
      runTest "Identificadores válidos" $
        all
          lexerOK
          ["x", "myVar", "_private", "camelCase", "snake_case", "Var123"],
      runTest "Literais inteiros" $ all lexerOK ["0", "1", "42", "12345"],
      runTest "Literais float" $ all lexerOK ["0.0", "1.5", "3.14159"],
      runTest "Literais string" $ all lexerOK ["\"\"", "\"hello\"", "\"escape: \\n\""],
      runTest "Literais booleanos" $ all lexerOK ["true", "false"],
      runTest "Operadores aritmeticos" $ all lexerOK ["+", "-", "*", "/", "++", "--"],
      runTest "Operadores comparacao" $ all lexerOK ["==", "!=", "<", ">", "<=", ">="],
      runTest "Operadores logicos" $ all lexerOK ["&&", "||", "!"],
      runTest "Delimitadores" $ all lexerOK ["(", ")", "{", "}", "[", "]"],
      runTest "Pontuacao" $ all lexerOK [",", ":", ";", ".", "->"],
      runTest "Comentario de linha" $ lexerOK "// comentario\nfunc",
      runTest "Comentario de bloco" $ lexerOK "/* bloco */ func",
      runTest "Programa: count tokens" $
        let prog = "func main() : int { return 0; }"
         in lexerOK prog && countTokens prog == 11,
      runTest "== vs =" $
        hasToken (== TokAssign) "=" && hasToken (== TokEq) "==",
      runTest "Float vs Int tokens" $
        hasToken (\case TokFloatLit _ -> True; _ -> False) "3.14"
          && hasToken (\case TokIntLit _ -> True; _ -> False) "42"
    ]

--------------------------------------------------------------------------------
-- Testes do Parser
--------------------------------------------------------------------------------

parserTests :: TestGroup
parserTests =
  TestGroup
    "Analise Sintatica"
    [ runTest "Programa vazio" $ parserOK "",
      runTest "Funcao void vazia" $ parserOK "func main() : void { }",
      runTest "Funcao com retorno" $ parserOK "func main() : int { return 0; }",
      runTest "Funcao com params" $ parserOK "func add(a : int, b : int) : int { return a + b; }",
      runTest "Declaracao variavel" $ parserOK "func f() : void { let x : int = 42; }",
      runTest "Decl sem inicializacao" $ parserOK "func f() : void { let x : int; }",
      runTest "Inferencia (sem tipo)" $ parserOK "func id(x) { return x; }",
      runTest "Struct simples" $ parserOK "struct Point { x : int; y : int; }",
      runTest "If-else" $ parserOK "func f() : void { if (x > 0) { return; } else { return; } }",
      runTest "If sem else" $ parserOK "func f() : void { if (c) { x = 1; } }",
      runTest "While" $ parserOK "func f() : void { while (i < 10) { i = i + 1; } }",
      runTest "For classico" $ parserOK "func f() : void { for (i = 0; i < 10; i = i + 1) { print(i); } }",
      runTest "For com ++" $ parserOK "func f() : void { for (i = 0; i < 10; i++) { x = i; } }",
      runTest "Tipo array" $ parserOK "func f() : void { let arr : int[]; }",
      runTest "New array" $ parserOK "func f() : void { let a : int[] = new int[10]; }",
      runTest "Array literal" $ parserOK "func f() : void { let a : int[] = [1, 2, 3]; }",
      runTest "Acesso a array" $ parserOK "func f() : void { let x : int = arr[0]; }",
      runTest "Struct literal" $ parserOK "func f() : void { let p : Point = Point{1, 2}; }",
      runTest "Acesso a campo" $ parserOK "func f() : void { let x : int = person.age; }",
      runTest "Chamada de funcao" $ parserOK "func f() : void { let r : int = factorial(5); }",
      runTest "Expressoes aritmeticas" $ parserOK "func f() : void { let x : int = 1 + 2 * 3 - 4 / 2; }",
      runTest "Expressoes logicas" $ parserOK "func f() : void { let b : bool = x > 0 && y < 10 || z == 5; }",
      runTest "Negacao" $ parserOK "func f() : void { let b : bool = !c; let n : int = -x; }",
      runTest "Parenteses" $ parserOK "func f() : void { let x : int = (1 + 2) * 3; }",
      runTest "Forall simples" $ parserOK "forall a . func id(x : a) : a { return x; }",
      runTest "Forall multiplos" $ parserOK "forall a b . func map(f : (a) -> b, v : a[]) : b[] { return v; }",
      runTest "Tipo funcao" $ parserOK "func apply(f : (int) -> int, x : int) : int { return f(x); }",
      runTest "Multiplas funcoes" $ parserOK "func f1() : void { } func f2() : void { }",
      runTest "Atribuicao array" $ parserOK "func f() : void { arr[0] = 42; }",
      runTest "Atribuicao campo" $ parserOK "func f() : void { person.age = 25; }",
      runTest "Expr como stmt" $ parserOK "func f() : void { print(x); }",
      runTest "Return vazio" $ parserOK "func f() : void { return; }",
      runTest "i++" $ parserOK "func f() : void { i++; }",
      runTest "i--" $ parserOK "func f() : void { i--; }",
      runTest "Factorial completo" $
        parserOK $
          T.unlines
            [ "func factorial(n : int) : int {",
              "  if (n <= 1) { return 1; }",
              "  else { return n * factorial(n - 1); }",
              "}",
              "func main() : int { return factorial(5); }"
            ],
      runTest "Comentarios" $ parserOK "// hello\nfunc f() : void { /* x */ let x : int = 0; }",
      runTest "Strings escape" $ parserOK "func f() : void { let s : string = \"hi\\n\"; }",
      runTest "Acesso encadeado" $ parserOK "func f() : void { let x : int = arr[0].field; }"
    ]

--------------------------------------------------------------------------------
-- Testes de Erro do Parser
--------------------------------------------------------------------------------

parserErrorTests :: TestGroup
parserErrorTests =
  TestGroup
    "Erros de Sintaxe (deve rejeitar)"
    [ runTest "Falta ;" $ parserFail "func f() : void { let x : int = 42 }",
      runTest "Falta }" $ parserFail "func f() : void { let x : int = 42;",
      runTest "Falta )" $ parserFail "func f( : void { }",
      runTest "Expr incompleta" $ parserFail "func f() : void { let x : int = 1 + ; }",
      runTest "Keyword como id" $ parserFail "func f() : void { let func : int = 1; }",
      runTest "Op sem operando" $ parserFail "func f() : void { let x : int = * 2; }",
      runTest "Campo inválido" $ parserFail "struct Point { x : ; }",
      runTest "For incompleto" $ parserFail "func f() : void { for (i = 0; i < 10) { } }"
    ]

--------------------------------------------------------------------------------
-- Testes da AST
--------------------------------------------------------------------------------

astTests :: TestGroup
astTests =
  TestGroup
    "Estrutura da AST"
    [ runTest "Funcao simples" $ case parseAST "func main() : void { }" of
        Right (Program [FuncDecl [] "main" [] (Just TVoid) []]) -> True
        _ -> False,
      runTest "Struct" $ case parseAST "struct Point { x : int; y : int; }" of
        Right (Program [StructDecl "Point" [Field "x" TInt, Field "y" TInt]]) -> True
        _ -> False,
      runTest "Params tipados" $ case parseAST "func add(a : int, b : int) : int { return a; }" of
        Right (Program [FuncDecl [] "add" [Param "a" (Just TInt), Param "b" (Just TInt)] (Just TInt) _]) -> True
        _ -> False,
      runTest "Tipo array" $ case parseAST "func f(arr : int[]) : void { }" of
        Right (Program [FuncDecl [] "f" [Param "arr" (Just (TArr TInt))] _ _]) -> True
        _ -> False,
      runTest "Forall" $ case parseAST "forall a b . func id(x : a) : b { return x; }" of
        Right (Program [FuncDecl ["a", "b"] "id" _ _ _]) -> True
        _ -> False,
      runTest "Expr binaria" $ case parseExprTest "1 + 2" of
        Right (EBinOp OpAdd (EInt 1) (EInt 2)) -> True
        _ -> False,
      runTest "Precedencia * > +" $ case parseExprTest "1 + 2 * 3" of
        Right (EBinOp OpAdd (EInt 1) (EBinOp OpMul (EInt 2) (EInt 3))) -> True
        _ -> False,
      runTest "Associatividade esquerda" $ case parseExprTest "1 - 2 - 3" of
        Right (EBinOp OpSub (EBinOp OpSub (EInt 1) (EInt 2)) (EInt 3)) -> True
        _ -> False,
      runTest "Negacao unaria" $ case parseExprTest "-x" of
        Right (EUnaryOp OpNeg (EVar "x")) -> True
        _ -> False,
      runTest "Chamada de funcao" $ case parseExprTest "f(1, 2, 3)" of
        Right (ECall "f" [EInt 1, EInt 2, EInt 3]) -> True
        _ -> False,
      runTest "Acesso array" $ case parseExprTest "arr[0]" of
        Right (EArrayAccess (EVar "arr") (EInt 0)) -> True
        _ -> False,
      runTest "Acesso campo" $ case parseExprTest "obj.field" of
        Right (EFieldAccess (EVar "obj") "field") -> True
        _ -> False,
      runTest "Array literal" $ case parseExprTest "[1, 2, 3]" of
        Right (EArrayLit [EInt 1, EInt 2, EInt 3]) -> True
        _ -> False,
      runTest "Struct literal" $ case parseExprTest "Point{1, 2}" of
        Right (EStructLit "Point" [EInt 1, EInt 2]) -> True
        _ -> False,
      runTest "New array" $ case parseExprTest "new int[10]" of
        Right (ENewArray TInt (EInt 10)) -> True
        _ -> False
    ]

--------------------------------------------------------------------------------
-- Testes do Pretty Printer
--------------------------------------------------------------------------------

prettyTests :: TestGroup
prettyTests =
  TestGroup
    "Pretty Printer"
    [ runTest "Tipo int" $ pp TInt == "int",
      runTest "Tipo float" $ pp TFloat == "float",
      runTest "Tipo string" $ pp TString == "string",
      runTest "Tipo bool" $ pp TBool == "bool",
      runTest "Tipo void" $ pp TVoid == "void",
      runTest "Tipo array" $ pp (TArr TInt) == "int[]",
      runTest "Tipo funcao" $ pp (TFunc [TInt] TInt) == "(int) -> int",
      runTest "Expr int" $ pp (EInt 42) == "42",
      runTest "Expr float" $ pp (EFloat 3.14) == "3.14",
      runTest "Expr string" $ pp (EString "hello") == "\"hello\"",
      runTest "Expr bool" $ pp (EBool True) == "true" && pp (EBool False) == "false",
      runTest "Expr var" $ pp (EVar "x") == "x",
      runTest "Op binario" $ pp (EBinOp OpAdd (EInt 1) (EInt 2)) == "1 + 2",
      runTest "Op unario" $ pp (EUnaryOp OpNeg (EInt 1)) == "-1",
      runTest "Chamada" $ pp (ECall "f" [EInt 1, EInt 2]) == "f(1, 2)",
      runTest "Array access" $ pp (EArrayAccess (EVar "arr") (EInt 0)) == "arr[0]",
      runTest "Field access" $ pp (EFieldAccess (EVar "obj") "x") == "obj.x",
      runTest "New array" $ pp (ENewArray TInt (EInt 10)) == "new int[10]",
      runTest "Array lit" $ pp (EArrayLit [EInt 1, EInt 2]) == "[1, 2]",
      runTest "Struct lit" $ pp (EStructLit "Point" [EInt 1, EInt 2]) == "Point{1, 2}",
      runTest "i++" $ pp (EUnaryOp OpPostInc (EVar "i")) == "i++",
      runTest "i--" $ pp (EUnaryOp OpPostDec (EVar "i")) == "i--",
      runTest "Ops comparacao" $
        pp OpEq == "=="
          && pp OpNeq == "!="
          && pp OpLt == "<"
          && pp OpGt == ">"
          && pp OpLe == "<="
          && pp OpGe == ">=",
      runTest "Ops logicos" $ pp OpAnd == "&&" && pp OpOr == "||" && pp OpNot == "!",
      runTest "Ops aritmetico" $ pp OpAdd == "+" && pp OpSub == "-" && pp OpMul == "*" && pp OpDiv == "/"
    ]

--------------------------------------------------------------------------------
-- Testes Round-Trip
--------------------------------------------------------------------------------

roundTripTests :: TestGroup
roundTripTests =
  TestGroup
    "Round-Trip (Parse -> Pretty -> Parse)"
    [ runTest "Funcao simples" $ roundTrip "func main() : void {\n}",
      runTest "Declaracao" $ roundTrip' "func f() : void { let x : int = 42; }",
      runTest "If-else" $ roundTrip' "func f() : void { if (x > 0) { return; } else { return; } }",
      runTest "While" $ roundTrip' "func f() : void { while (i < 10) { i = i + 1; } }",
      runTest "Struct" $ roundTrip' "struct Point { x : int; y : int; }",
      runTest "Expressoes" $ roundTrip' "func f() : int { let x : int = 1 + 2 * 3; return x; }",
      runTest "Arrays" $ roundTrip' "func f() : void { let a : int[] = new int[10]; a[0] = 1; }",
      runTest "Chamada" $ roundTrip' "func f() : void { print(x, y); }",
      runTest "Forall" $ roundTrip' "forall a . func id(x : a) : a { return x; }"
    ]
  where
    roundTrip src = case parseAST src of
      Left _ -> False
      Right ast1 -> case parseAST (T.pack $ pp ast1) of
        Left _ -> False
        Right ast2 -> ast1 == ast2
    roundTrip' = roundTrip'OK
    roundTrip'OK src = case parseAST src of
      Left _ -> False
      Right ast1 -> case parseAST (T.pack $ pp ast1) of
        Left _ -> False
        Right _ -> True

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "+============================================+"
  putStrLn "|     Testes do Compilador SL - BCC328      |"
  putStrLn "+============================================+"

  results <-
    mapM
      runTestGroup
      [ lexerTests,
        parserTests,
        parserErrorTests,
        astTests,
        prettyTests,
        roundTripTests
      ]

  let (passed, failed) = foldr (\(p, f) (tp, tf) -> (tp + p, tf + f)) (0, 0) results

  putStrLn "\n============================================"
  putStrLn $ "TOTAL: " ++ show passed ++ " OK, " ++ show failed ++ " FALHAS"
  putStrLn "============================================"

  if failed > 0
    then putStrLn "[FAIL] Alguns testes falharam!" >> exitFailure
    else putStrLn "[OK] Todos os testes passaram!" >> exitSuccess
