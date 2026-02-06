{-# LANGUAGE LambdaCase #-}

-- | Interpretador para a linguagem SL
module SL.Interpreter
  ( -- * Tipos de Valor
    Value (..),
    RuntimeError (..),

    -- * Estado do Interpretador
    InterpState,
    Output,

    -- * Funções Principais
    interpret,
    interpretProgram,
    runProgram,

    -- * Utilitários
    formatRuntimeError,
    valueToString,
  )
where

import Control.Monad (foldM)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SL.AST

--------------------------------------------------------------------------------
-- Tipos de Valor
--------------------------------------------------------------------------------

data Value
  = VInt Int
  | VFloat Double
  | VString String
  | VBool Bool
  | VArray [IORef Value]
  | VStruct String (Map String (IORef Value))
  | VVoid
  | VReturn Value

instance Show Value where
  show = \case
    VInt n -> show n
    VFloat d -> show d
    VString s -> s
    VBool b -> if b then "true" else "false"
    VArray _ -> "<array>"
    VStruct name _ -> "<" ++ name ++ ">"
    VVoid -> "void"
    VReturn v -> "return " ++ show v

--------------------------------------------------------------------------------
-- Erros de Execução
--------------------------------------------------------------------------------

data RuntimeError
  = DivisionByZero
  | IndexOutOfBounds Int Int
  | UndefinedVariable String
  | UndefinedFunction String
  | UndefinedStruct String
  | UndefinedField String String
  | TypeErrorRuntime String
  | InvalidOperation String
  | NoMainFunction
  | StackOverflow
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Estado do Interpretador
--------------------------------------------------------------------------------

type VarEnv = [Map String (IORef Value)]

data FuncDef = FuncDef
  { _fdParams :: [String],
    _fdBody :: [Stmt],
    _fdEnv :: VarEnv
  }

type StructDef = [(String, Type)]

data InterpState = InterpState
  { isVars :: VarEnv,
    isFuncs :: Map String FuncDef,
    isStructs :: Map String StructDef,
    isOutput :: IORef [String],
    isCallDepth :: Int
  }

type Output = [String]

maxCallDepth :: Int
maxCallDepth = 10000

--------------------------------------------------------------------------------
-- Operações de Ambiente
--------------------------------------------------------------------------------

pushScope :: InterpState -> InterpState
pushScope st = st {isVars = Map.empty : isVars st}

popScope :: InterpState -> InterpState
popScope st = st {isVars = drop 1 (isVars st)}

addVar :: String -> Value -> InterpState -> IO InterpState
addVar name val st = do
  ref <- newIORef val
  case isVars st of
    [] -> return st
    (scope : rest) ->
      return $ st {isVars = Map.insert name ref scope : rest}

lookupVar :: String -> InterpState -> IO (Maybe (IORef Value))
lookupVar name st = go (isVars st)
  where
    go [] = return Nothing
    go (scope : rest) = case Map.lookup name scope of
      Just ref -> return $ Just ref
      Nothing -> go rest

updateVar :: String -> Value -> InterpState -> IO (Either RuntimeError ())
updateVar name val st = do
  mref <- lookupVar name st
  case mref of
    Nothing -> return $ Left $ UndefinedVariable name
    Just ref -> do
      writeIORef ref val
      return $ Right ()

lookupFunc :: String -> InterpState -> Maybe FuncDef
lookupFunc name st = Map.lookup name (isFuncs st)

lookupStruct :: String -> InterpState -> Maybe StructDef
lookupStruct name st = Map.lookup name (isStructs st)

appendOutput :: String -> InterpState -> IO ()
appendOutput s st = modifyIORef (isOutput st) (++ [s])

--------------------------------------------------------------------------------
-- Interpretador Principal
--------------------------------------------------------------------------------

interpret :: Program -> IO (Either RuntimeError Output)
interpret = interpretProgram

interpretProgram :: Program -> IO (Either RuntimeError Output)
interpretProgram prog = do
  outputRef <- newIORef []
  let initState =
        InterpState
          { isVars = [Map.empty],
            isFuncs = Map.empty,
            isStructs = Map.empty,
            isOutput = outputRef,
            isCallDepth = 0
          }
  result <- runInterp prog initState
  case result of
    Left err -> return $ Left err
    Right _ -> do
      output <- readIORef outputRef
      return $ Right output

runProgram :: Program -> IO (Either RuntimeError Output)
runProgram = interpretProgram

runInterp :: Program -> InterpState -> IO (Either RuntimeError Value)
runInterp (Program decls) st = do
  st1 <- collectDecls decls st
  case lookupFunc "main" st1 of
    Nothing -> return $ Left NoMainFunction
    Just _ -> callFunc "main" [] st1

collectDecls :: [TopLevelDecl] -> InterpState -> IO InterpState
collectDecls decls st = foldM collectDecl st decls

collectDecl :: InterpState -> TopLevelDecl -> IO InterpState
collectDecl st = \case
  StructDecl name fields ->
    let fieldDefs = [(n, t) | Field n t <- fields]
     in return $ st {isStructs = Map.insert name fieldDefs (isStructs st)}
  FuncDecl _ name params _ body ->
    let paramNames = [n | Param n _ <- params]
        funcDef = FuncDef paramNames body (isVars st)
     in return $ st {isFuncs = Map.insert name funcDef (isFuncs st)}

--------------------------------------------------------------------------------
-- Execução de Funções
--------------------------------------------------------------------------------

callFunc :: String -> [Value] -> InterpState -> IO (Either RuntimeError Value)
callFunc name args st
  | isCallDepth st >= maxCallDepth = return $ Left StackOverflow
  | otherwise =
      case lookupFunc name st of
        Nothing -> handleBuiltin name args st
        Just (FuncDef params body closure) -> do
          if length args /= length params
            then return $ Left $ InvalidOperation $ "Número incorreto de argumentos para " ++ name
            else do
              let st1 = st {isVars = Map.empty : closure, isCallDepth = isCallDepth st + 1}
              st2 <- foldM addParam st1 (zip params args)
              result <- execBlock body st2
              case result of
                Left err -> return $ Left err
                Right (VReturn v, _) -> return $ Right v
                Right (v, _) -> return $ Right v
  where
    addParam s (pname, val) = addVar pname val s

handleBuiltin :: String -> [Value] -> InterpState -> IO (Either RuntimeError Value)
handleBuiltin name args st = case name of
  "print" -> do
    case args of
      [v] -> appendOutput (valueToString v) st
      _ -> appendOutput (unwords $ map valueToString args) st
    return $ Right VVoid
  "print_int" ->
    case args of
      [VInt n] -> do
        appendOutput (show n) st
        return $ Right VVoid
      _ -> return $ Left $ TypeErrorRuntime "print_int requer argumento do tipo int"
  "print_float" ->
    case args of
      [VFloat d] -> do
        appendOutput (show d) st
        return $ Right VVoid
      _ -> return $ Left $ TypeErrorRuntime "print_float requer argumento do tipo float"
  "print_string" ->
    case args of
      [VString s] -> do
        appendOutput s st
        return $ Right VVoid
      _ -> return $ Left $ TypeErrorRuntime "print_string requer argumento do tipo string"
  "print_bool" ->
    case args of
      [VBool b] -> do
        appendOutput (if b then "true" else "false") st
        return $ Right VVoid
      _ -> return $ Left $ TypeErrorRuntime "print_bool requer argumento do tipo bool"
  _ -> return $ Left $ UndefinedFunction name

--------------------------------------------------------------------------------
-- Execução de Statements
--------------------------------------------------------------------------------

execBlock :: [Stmt] -> InterpState -> IO (Either RuntimeError (Value, InterpState))
execBlock [] st = return $ Right (VVoid, st)
execBlock (s : ss) st = do
  result <- execStmt s st
  case result of
    Left err -> return $ Left err
    Right (VReturn v, st') -> return $ Right (VReturn v, st')
    Right (_, st') -> execBlock ss st'

execStmt :: Stmt -> InterpState -> IO (Either RuntimeError (Value, InterpState))
execStmt stmt st = case stmt of
  SVarDecl name mType mExpr -> do
    valResult <- case mExpr of
      Nothing -> Right <$> defaultValueIO mType
      Just expr -> evalExpr expr st
    case valResult of
      Left err -> return $ Left err
      Right v -> do
        st' <- addVar name v st
        return $ Right (VVoid, st')
  SAssign target expr -> do
    valResult <- evalExpr expr st
    case valResult of
      Left err -> return $ Left err
      Right val -> do
        result <- assignToTarget target val st
        case result of
          Left err -> return $ Left err
          Right () -> return $ Right (VVoid, st)
  SIf cond thenBlock elseBlock -> do
    condResult <- evalExpr cond st
    case condResult of
      Left err -> return $ Left err
      Right (VBool True) -> do
        let st' = pushScope st
        result <- execBlock thenBlock st'
        case result of
          Left err -> return $ Left err
          Right (v, st'') -> return $ Right (v, popScope st'')
      Right (VBool False) -> case elseBlock of
        Nothing -> return $ Right (VVoid, st)
        Just els -> do
          let st' = pushScope st
          result <- execBlock els st'
          case result of
            Left err -> return $ Left err
            Right (v, st'') -> return $ Right (v, popScope st'')
      Right v -> return $ Left $ TypeErrorRuntime $ "Esperado bool na condição do if, encontrado " ++ show v
  SWhile cond body -> execWhile cond body st
  SFor varName initExpr condExpr updateExpr body -> do
    initResult <- evalExpr initExpr st
    case initResult of
      Left err -> return $ Left err
      Right initVal -> do
        let st' = pushScope st
        st'' <- addVar varName initVal st'
        result <- execFor condExpr updateExpr body st''
        case result of
          Left err -> return $ Left err
          Right (v, stFinal) -> return $ Right (v, popScope stFinal)
  SReturn mExpr -> do
    case mExpr of
      Nothing -> return $ Right (VReturn VVoid, st)
      Just expr -> do
        result <- evalExpr expr st
        case result of
          Left err -> return $ Left err
          Right v -> return $ Right (VReturn v, st)
  SExpr expr -> do
    result <- evalExpr expr st
    case result of
      Left err -> return $ Left err
      Right v -> return $ Right (v, st)

defaultValue :: Maybe Type -> Value
defaultValue = \case
  Nothing -> VInt 0
  Just TInt -> VInt 0
  Just TFloat -> VFloat 0.0
  Just TString -> VString ""
  Just TBool -> VBool False
  Just TVoid -> VVoid
  Just (TArr _ _) -> VArray []
  Just (TRecord _) -> VVoid
  Just (TGeneric _) -> VInt 0
  Just (TFunc _ _) -> VVoid
  Just (TMeta _) -> VInt 0

defaultValueIO :: Maybe Type -> IO Value
defaultValueIO = \case
  Just (TArr elemTy (Just size)) -> do
    let elemDefault = defaultValue (Just elemTy)
    refs <- mapM (const $ newIORef elemDefault) [1 .. size]
    return $ VArray refs
  Just (TRecord structName) -> do
    return $ VStruct structName Map.empty
  other -> return $ defaultValue other

execWhile :: Expr -> [Stmt] -> InterpState -> IO (Either RuntimeError (Value, InterpState))
execWhile cond body st = do
  condResult <- evalExpr cond st
  case condResult of
    Left err -> return $ Left err
    Right (VBool False) -> return $ Right (VVoid, st)
    Right (VBool True) -> do
      let st' = pushScope st
      bodyResult <- execBlock body st'
      case bodyResult of
        Left err -> return $ Left err
        Right (VReturn v, st'') -> return $ Right (VReturn v, popScope st'')
        Right (_, st'') -> execWhile cond body (popScope st'')
    Right v -> return $ Left $ TypeErrorRuntime $ "Esperado bool na condição do while, encontrado " ++ show v

execFor :: Expr -> Expr -> [Stmt] -> InterpState -> IO (Either RuntimeError (Value, InterpState))
execFor cond update body st = do
  condResult <- evalExpr cond st
  case condResult of
    Left err -> return $ Left err
    Right (VBool False) -> return $ Right (VVoid, st)
    Right (VBool True) -> do
      bodyResult <- execBlock body st
      case bodyResult of
        Left err -> return $ Left err
        Right (VReturn v, st') -> return $ Right (VReturn v, st')
        Right (_, st') -> do
          updateResult <- evalExpr update st'
          case updateResult of
            Left err -> return $ Left err
            Right _ -> execFor cond update body st'
    Right v -> return $ Left $ TypeErrorRuntime $ "Esperado bool na condição do for, encontrado " ++ show v

assignToTarget :: Expr -> Value -> InterpState -> IO (Either RuntimeError ())
assignToTarget target val st = case target of
  EVar name -> updateVar name val st
  EArrayAccess arrExpr idxExpr -> do
    arrResult <- evalExpr arrExpr st
    idxResult <- evalExpr idxExpr st
    case (arrResult, idxResult) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right (VArray refs), Right (VInt idx)) ->
        if idx < 0 || idx >= length refs
          then return $ Left $ IndexOutOfBounds idx (length refs)
          else do
            writeIORef (refs !! idx) val
            return $ Right ()
      (Right v, _) -> return $ Left $ TypeErrorRuntime $ "Esperado array, encontrado " ++ show v
  EFieldAccess objExpr fieldName -> do
    objResult <- evalExpr objExpr st
    case objResult of
      Left err -> return $ Left err
      Right (VStruct _ fields) ->
        case Map.lookup fieldName fields of
          Nothing -> return $ Left $ UndefinedField "struct" fieldName
          Just ref -> do
            writeIORef ref val
            return $ Right ()
      Right v -> return $ Left $ TypeErrorRuntime $ "Esperado struct, encontrado " ++ show v
  _ -> return $ Left $ InvalidOperation "Alvo de atribuição inválido"

--------------------------------------------------------------------------------
-- Avaliação de Expressões
--------------------------------------------------------------------------------

evalExpr :: Expr -> InterpState -> IO (Either RuntimeError Value)
evalExpr expr st = case expr of
  EInt n -> return $ Right $ VInt n
  EFloat d -> return $ Right $ VFloat d
  EString s -> return $ Right $ VString s
  EBool b -> return $ Right $ VBool b
  EVar name -> do
    mref <- lookupVar name st
    case mref of
      Nothing -> return $ Left $ UndefinedVariable name
      Just ref -> do
        val <- readIORef ref
        return $ Right val
  EArrayAccess arrExpr idxExpr -> do
    arrResult <- evalExpr arrExpr st
    idxResult <- evalExpr idxExpr st
    case (arrResult, idxResult) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right (VArray refs), Right (VInt idx)) ->
        if idx < 0 || idx >= length refs
          then return $ Left $ IndexOutOfBounds idx (length refs)
          else do
            val <- readIORef (refs !! idx)
            return $ Right val
      (Right v, _) -> return $ Left $ TypeErrorRuntime $ "Esperado array, encontrado " ++ show v
  EFieldAccess objExpr fieldName -> do
    objResult <- evalExpr objExpr st
    case objResult of
      Left err -> return $ Left err
      Right (VArray refs) ->
        if fieldName == "size"
          then return $ Right $ VInt (length refs)
          else return $ Left $ TypeErrorRuntime $ "Array nao tem campo '" ++ fieldName ++ "'"
      Right (VStruct structName fields) ->
        case Map.lookup fieldName fields of
          Nothing -> return $ Left $ UndefinedField structName fieldName
          Just ref -> do
            val <- readIORef ref
            return $ Right val
      Right v -> return $ Left $ TypeErrorRuntime $ "Esperado struct ou array, encontrado " ++ show v
  EBinOp op left right -> do
    leftResult <- evalExpr left st
    rightResult <- evalExpr right st
    case (leftResult, rightResult) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right l, Right r) -> return $ evalBinOp op l r
  EUnaryOp op operand -> do
    result <- evalExpr operand st
    case result of
      Left err -> return $ Left err
      Right v -> do
        evalUnaryOp op v operand st
  ECall funcName args -> do
    argResults <- mapM (`evalExpr` st) args
    case sequence argResults of
      Left err -> return $ Left err
      Right vals -> callFunc funcName vals st
  ENewArray elemType sizeExpr -> do
    sizeResult <- evalExpr sizeExpr st
    case sizeResult of
      Left err -> return $ Left err
      Right (VInt size) -> do
        refs <- mapM (const $ newIORef (defaultValue (Just elemType))) [1 .. size]
        return $ Right $ VArray refs
      Right v -> return $ Left $ TypeErrorRuntime $ "Esperado int para o tamanho do array, encontrado " ++ show v
  EArrayLit elems -> do
    results <- mapM (`evalExpr` st) elems
    case sequence results of
      Left err -> return $ Left err
      Right vals -> do
        refs <- mapM newIORef vals
        return $ Right $ VArray refs
  EStructLit structName fieldExprs -> do
    case lookupStruct structName st of
      Nothing -> return $ Left $ UndefinedStruct structName
      Just structDef -> do
        results <- mapM (`evalExpr` st) fieldExprs
        case sequence results of
          Left err -> return $ Left err
          Right vals -> do
            let fieldNames = map fst structDef
            refs <- mapM newIORef vals
            let fieldMap = Map.fromList $ zip fieldNames refs
            return $ Right $ VStruct structName fieldMap

evalBinOp :: BinOp -> Value -> Value -> Either RuntimeError Value
evalBinOp op left right = case op of
  OpAdd -> case (left, right) of
    (VInt a, VInt b) -> Right $ VInt (a + b)
    (VFloat a, VFloat b) -> Right $ VFloat (a + b)
    (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a + b)
    (VFloat a, VInt b) -> Right $ VFloat (a + fromIntegral b)
    (VString a, VString b) -> Right $ VString (a ++ b)
    _ -> Left $ InvalidOperation $ "Não é possível somar " ++ show left ++ " e " ++ show right
  OpSub -> case (left, right) of
    (VInt a, VInt b) -> Right $ VInt (a - b)
    (VFloat a, VFloat b) -> Right $ VFloat (a - b)
    (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a - b)
    (VFloat a, VInt b) -> Right $ VFloat (a - fromIntegral b)
    _ -> Left $ InvalidOperation $ "Não é possível subtrair " ++ show left ++ " e " ++ show right
  OpMul -> case (left, right) of
    (VInt a, VInt b) -> Right $ VInt (a * b)
    (VFloat a, VFloat b) -> Right $ VFloat (a * b)
    (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a * b)
    (VFloat a, VInt b) -> Right $ VFloat (a * fromIntegral b)
    _ -> Left $ InvalidOperation $ "Não é possível multiplicar " ++ show left ++ " e " ++ show right
  OpDiv -> case (left, right) of
    (VInt _, VInt 0) -> Left DivisionByZero
    (VFloat _, VFloat 0.0) -> Left DivisionByZero
    (VInt a, VInt b) -> Right $ VInt (a `div` b)
    (VFloat a, VFloat b) -> Right $ VFloat (a / b)
    (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a / b)
    (VFloat a, VInt b) -> Right $ VFloat (a / fromIntegral b)
    _ -> Left $ InvalidOperation $ "Não é possível dividir " ++ show left ++ " e " ++ show right
  OpEq -> Right $ VBool $ valuesEqual left right
  OpNeq -> Right $ VBool $ not $ valuesEqual left right
  OpLt -> case (left, right) of
    (VInt a, VInt b) -> Right $ VBool (a < b)
    (VFloat a, VFloat b) -> Right $ VBool (a < b)
    (VInt a, VFloat b) -> Right $ VBool (fromIntegral a < b)
    (VFloat a, VInt b) -> Right $ VBool (a < fromIntegral b)
    (VString a, VString b) -> Right $ VBool (a < b)
    _ -> Left $ InvalidOperation $ "Não é possível comparar " ++ show left ++ " e " ++ show right
  OpGt -> case (left, right) of
    (VInt a, VInt b) -> Right $ VBool (a > b)
    (VFloat a, VFloat b) -> Right $ VBool (a > b)
    (VInt a, VFloat b) -> Right $ VBool (fromIntegral a > b)
    (VFloat a, VInt b) -> Right $ VBool (a > fromIntegral b)
    (VString a, VString b) -> Right $ VBool (a > b)
    _ -> Left $ InvalidOperation $ "Não é possível comparar " ++ show left ++ " e " ++ show right
  OpLe -> case (left, right) of
    (VInt a, VInt b) -> Right $ VBool (a <= b)
    (VFloat a, VFloat b) -> Right $ VBool (a <= b)
    (VInt a, VFloat b) -> Right $ VBool (fromIntegral a <= b)
    (VFloat a, VInt b) -> Right $ VBool (a <= fromIntegral b)
    (VString a, VString b) -> Right $ VBool (a <= b)
    _ -> Left $ InvalidOperation $ "Não é possível comparar " ++ show left ++ " e " ++ show right
  OpGe -> case (left, right) of
    (VInt a, VInt b) -> Right $ VBool (a >= b)
    (VFloat a, VFloat b) -> Right $ VBool (a >= b)
    (VInt a, VFloat b) -> Right $ VBool (fromIntegral a >= b)
    (VFloat a, VInt b) -> Right $ VBool (a >= fromIntegral b)
    (VString a, VString b) -> Right $ VBool (a >= b)
    _ -> Left $ InvalidOperation $ "Não é possível comparar " ++ show left ++ " e " ++ show right
  OpAnd -> case (left, right) of
    (VBool a, VBool b) -> Right $ VBool (a && b)
    _ -> Left $ InvalidOperation $ "Não é possível aplicar && a " ++ show left ++ " e " ++ show right
  OpOr -> case (left, right) of
    (VBool a, VBool b) -> Right $ VBool (a || b)
    _ -> Left $ InvalidOperation $ "Não é possível aplicar || a " ++ show left ++ " e " ++ show right

valuesEqual :: Value -> Value -> Bool
valuesEqual v1 v2 = case (v1, v2) of
  (VInt a, VInt b) -> a == b
  (VFloat a, VFloat b) -> a == b
  (VInt a, VFloat b) -> fromIntegral a == b
  (VFloat a, VInt b) -> a == fromIntegral b
  (VString a, VString b) -> a == b
  (VBool a, VBool b) -> a == b
  (VVoid, VVoid) -> True
  _ -> False

evalUnaryOp :: UnaryOp -> Value -> Expr -> InterpState -> IO (Either RuntimeError Value)
evalUnaryOp op val operand st = case op of
  OpNeg -> case val of
    VInt n -> return $ Right $ VInt (-n)
    VFloat d -> return $ Right $ VFloat (-d)
    _ -> return $ Left $ InvalidOperation $ "Não é possível negar " ++ show val
  OpNot -> case val of
    VBool b -> return $ Right $ VBool (not b)
    _ -> return $ Left $ InvalidOperation $ "Não é possível negar " ++ show val
  OpPostInc -> case (val, operand) of
    (VInt n, EVar name) -> do
      _ <- updateVar name (VInt (n + 1)) st
      return $ Right $ VInt n
    _ -> return $ Left $ InvalidOperation "++ requer variável inteira"
  OpPostDec -> case (val, operand) of
    (VInt n, EVar name) -> do
      _ <- updateVar name (VInt (n - 1)) st
      return $ Right $ VInt n
    _ -> return $ Left $ InvalidOperation "-- requer variável inteira"

--------------------------------------------------------------------------------
-- Utilitários
--------------------------------------------------------------------------------

valueToString :: Value -> String
valueToString = \case
  VInt n -> show n
  VFloat d -> show d
  VString s -> s
  VBool b -> if b then "true" else "false"
  VArray refs -> "[array de " ++ show (length refs) ++ " elementos]"
  VStruct name _ -> "<" ++ name ++ ">"
  VVoid -> "void"
  VReturn v -> valueToString v

formatRuntimeError :: RuntimeError -> String
formatRuntimeError = \case
  DivisionByZero -> "Erro de execucao: divisao por zero"
  IndexOutOfBounds idx len ->
    "Erro de execucao: indice " ++ show idx ++ " fora dos limites (tamanho: " ++ show len ++ ")"
  UndefinedVariable name -> "Erro de execucao: variavel indefinida '" ++ name ++ "'"
  UndefinedFunction name -> "Erro de execucao: funcao indefinida '" ++ name ++ "'"
  UndefinedStruct name -> "Erro de execucao: struct indefinida '" ++ name ++ "'"
  UndefinedField struct field ->
    "Erro de execucao: campo '" ++ field ++ "' indefinido em struct '" ++ struct ++ "'"
  TypeErrorRuntime msg -> "Erro de tipo em tempo de execucao: " ++ msg
  InvalidOperation msg -> "Operacao invalida: " ++ msg
  NoMainFunction -> "Erro de execucao: funcao 'main' nao encontrada"
  StackOverflow -> "Erro de execucao: estouro de pilha (recursao muito profunda)"
