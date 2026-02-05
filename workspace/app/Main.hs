{-# LANGUAGE LambdaCase #-}

-- | CLI do compilador SL
module Main where

import Data.Text.IO qualified as TIO
import Data.Tree (Tree (..), drawTree)
import SL.AST
import SL.Lexer (formatToken, lexAll)
import SL.Parser (parseProgram)
import SL.Pretty (pp, ppBinOp, ppType, ppUnaryOp)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main =
  getArgs >>= \case
    ["--lexer", f] -> runLexer f
    ["--parser", f] -> runParser f
    ["--pretty", f] -> runPretty f
    _ -> usage

runLexer :: FilePath -> IO ()
runLexer f = do
  src <- TIO.readFile f
  case parse lexAll f src of
    Left err -> putStrLn "Erro lexico:" >> putStrLn (errorBundlePretty err) >> exitFailure
    Right tks -> mapM_ (putStrLn . formatToken) tks

runParser :: FilePath -> IO ()
runParser f = do
  src <- TIO.readFile f
  case parse parseProgram f src of
    Left err -> putStrLn "Erro sintatico:" >> putStrLn (errorBundlePretty err) >> exitFailure
    Right ast -> putStrLn "=== AST ===" >> putStrLn (drawTree $ astToTree ast)

runPretty :: FilePath -> IO ()
runPretty f = do
  src <- TIO.readFile f
  case parse parseProgram f src of
    Left err -> putStrLn "Erro sintatico:" >> putStrLn (errorBundlePretty err) >> exitFailure
    Right ast -> putStrLn "=== Pretty Print ===" >> putStrLn (pp ast)

usage :: IO ()
usage = do
  putStrLn "Compilador SL - BCC328"
  putStrLn ""
  putStrLn "Uso: slc [opcao] <arquivo>"
  putStrLn ""
  putStrLn "Opcoes:"
  putStrLn "  --lexer  <arq>   Analise lexica (tokens)"
  putStrLn "  --parser <arq>   Analise sintatica (AST)"
  putStrLn "  --pretty <arq>   Pretty-print"
  exitFailure

--------------------------------------------------------------------------------
-- Conversão AST -> Tree para visualização
--------------------------------------------------------------------------------

astToTree :: Program -> Tree String
astToTree (Program decls) = Node "Program" (map declTree decls)

declTree :: TopLevelDecl -> Tree String
declTree = \case
  FuncDecl tvs name ps ret body ->
    Node (funcLabel name tvs ret) $ map paramTree ps ++ [Node "Body" (map stmtTree body)]
  StructDecl name fs ->
    Node ("StructDecl: " ++ name) (map fieldTree fs)
  where
    funcLabel n tvs ret =
      "FuncDecl: "
        ++ n
        ++ (if null tvs then "" else " forall " ++ unwords tvs ++ " .")
        ++ maybe "" ((" : " ++) . ppType) ret

paramTree :: Param -> Tree String
paramTree (Param n mt) = Node ("Param: " ++ n ++ maybe "" ((" : " ++) . ppType) mt) []

fieldTree :: Field -> Tree String
fieldTree (Field n t) = Node ("Field: " ++ n ++ " : " ++ ppType t) []

stmtTree :: Stmt -> Tree String
stmtTree = \case
  SVarDecl n mt me ->
    Node ("VarDecl: " ++ n ++ maybe "" ((" : " ++) . ppType) mt) (maybe [] (pure . exprTree) me)
  SAssign l r -> Node "Assign" [exprTree l, exprTree r]
  SIf c th el ->
    Node "If" $ exprTree c : Node "Then" (map stmtTree th) : maybe [] (pure . Node "Else" . map stmtTree) el
  SWhile c b -> Node "While" [exprTree c, Node "Body" (map stmtTree b)]
  SFor v i c u b -> Node ("For: " ++ v) [exprTree i, exprTree c, exprTree u, Node "Body" (map stmtTree b)]
  SReturn me -> Node "Return" (maybe [] (pure . exprTree) me)
  SExpr e -> Node "ExprStmt" [exprTree e]

exprTree :: Expr -> Tree String
exprTree = \case
  EInt n -> Node ("Int: " ++ show n) []
  EFloat d -> Node ("Float: " ++ show d) []
  EString s -> Node ("String: \"" ++ s ++ "\"") []
  EBool b -> Node ("Bool: " ++ show b) []
  EVar v -> Node ("Var: " ++ v) []
  EArrayAccess a i -> Node "ArrayAccess" [exprTree a, exprTree i]
  EFieldAccess e f -> Node ("FieldAccess: ." ++ f) [exprTree e]
  EBinOp op l r -> Node ("BinOp: " ++ ppBinOp op) [exprTree l, exprTree r]
  EUnaryOp op e -> Node ("UnaryOp: " ++ ppUnaryOp op) [exprTree e]
  ECall f args -> Node ("Call: " ++ f) (map exprTree args)
  ENewArray t sz -> Node ("NewArray: " ++ ppType t) [exprTree sz]
  EArrayLit es -> Node "ArrayLit" (map exprTree es)
  EStructLit n es -> Node ("StructLit: " ++ n) (map exprTree es)
