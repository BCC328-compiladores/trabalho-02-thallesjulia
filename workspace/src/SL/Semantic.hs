{-# LANGUAGE LambdaCase #-}

-- | Analisador semântico para a linguagem SL
module SL.Semantic
  ( -- * Tipos de Erro
    SemanticError (..),
    TypeError (..),
    ScopeError (..),

    -- * Ambiente de Tipos
    TypeEnv,
    StructEnv,
    FuncSig (..),

    -- * Funções Principais
    checkProgram,
    typeCheck,
    inferType,

    -- * Utilitários
    formatSemanticError,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import SL.AST

--------------------------------------------------------------------------------
-- Tipos de Erro
--------------------------------------------------------------------------------

data SemanticError
  = TypeError TypeError
  | ScopeError ScopeError
  deriving (Show, Eq)

data TypeError
  = TypeMismatch Type Type String
  | InvalidOperator BinOp Type Type
  | InvalidUnaryOperator UnaryOp Type
  | NotAnArray Type
  | NotAStruct Type
  | NotAFunction String
  | WrongNumberOfArgs String Int Int
  | InvalidArrayIndex Type
  | CannotInferType String
  | VoidInExpression
  | InvalidReturnType Type Type
  | MissingReturn String
  | InvalidCondition Type
  deriving (Show, Eq)

data ScopeError
  = UndefinedVariable String
  | UndefinedFunction String
  | UndefinedStruct String
  | UndefinedField String String
  | DuplicateVariable String
  | DuplicateFunction String
  | DuplicateStruct String
  | DuplicateField String
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Ambiente de Tipos
--------------------------------------------------------------------------------

data FuncSig = FuncSig
  { funcParams :: [Type],
    funcReturn :: Type,
    funcTypeVars :: [TypeVar]
  }
  deriving (Show, Eq)

type TypeEnv = [Map String Type]

type StructEnv = Map String [(String, Type)]

type FuncEnv = Map String FuncSig

data CheckerState = CheckerState
  { csVars :: TypeEnv,
    csStructs :: StructEnv,
    csFuncs :: FuncEnv,
    csCurrentFunc :: Maybe (String, Type),
    csErrors :: [SemanticError]
  }

initState :: CheckerState
initState =
  CheckerState
    { csVars = [Map.empty],
      csStructs = Map.empty,
      csFuncs = builtinFuncs,
      csCurrentFunc = Nothing,
      csErrors = []
    }

builtinFuncs :: FuncEnv
builtinFuncs =
  Map.fromList
    [ ("print_int", FuncSig [TInt] TVoid []),
      ("print_float", FuncSig [TFloat] TVoid []),
      ("print_string", FuncSig [TString] TVoid []),
      ("print_bool", FuncSig [TBool] TVoid [])
    ]

--------------------------------------------------------------------------------
-- Operações de Ambiente
--------------------------------------------------------------------------------

pushScope :: CheckerState -> CheckerState
pushScope st = st {csVars = Map.empty : csVars st}

popScope :: CheckerState -> CheckerState
popScope st = st {csVars = drop 1 (csVars st)}

addVar :: String -> Type -> CheckerState -> Either SemanticError CheckerState
addVar name ty st =
  case csVars st of
    [] -> Left $ ScopeError $ UndefinedVariable name
    (scope : rest) ->
      if Map.member name scope
        then Left $ ScopeError $ DuplicateVariable name
        else Right $ st {csVars = Map.insert name ty scope : rest}

lookupVar :: String -> CheckerState -> Maybe Type
lookupVar name st = go (csVars st)
  where
    go [] = Nothing
    go (scope : rest) = case Map.lookup name scope of
      Just t -> Just t
      Nothing -> go rest

lookupStruct :: String -> CheckerState -> Maybe [(String, Type)]
lookupStruct name st = Map.lookup name (csStructs st)

lookupFunc :: String -> CheckerState -> Maybe FuncSig
lookupFunc name st = Map.lookup name (csFuncs st)

lookupField :: String -> String -> CheckerState -> Maybe Type
lookupField structName fieldName st = do
  fields <- lookupStruct structName st
  lookup fieldName fields

addError :: SemanticError -> CheckerState -> CheckerState
addError err st = st {csErrors = err : csErrors st}

--------------------------------------------------------------------------------
-- Verificação de Tipos Principal
--------------------------------------------------------------------------------

checkProgram :: Program -> Either [SemanticError] ()
checkProgram prog =
  let st = checkProgramState prog initState
   in if null (csErrors st)
        then Right ()
        else Left (reverse $ csErrors st)

typeCheck :: Program -> Either [SemanticError] ()
typeCheck = checkProgram

checkProgramState :: Program -> CheckerState -> CheckerState
checkProgramState (Program decls) st =
  let st1 = collectDeclarations decls st
   in foldl checkTopLevel st1 decls

collectDeclarations :: [TopLevelDecl] -> CheckerState -> CheckerState
collectDeclarations decls st = foldl collectDecl st decls
  where
    collectDecl s = \case
      StructDecl name fields ->
        if Map.member name (csStructs s)
          then addError (ScopeError $ DuplicateStruct name) s
          else s {csStructs = Map.insert name (map fieldToTuple fields) (csStructs s)}
      FuncDecl tvs name params retType _ ->
        if Map.member name (csFuncs s) && name /= "print"
          then addError (ScopeError $ DuplicateFunction name) s
          else
            let paramTypes = map paramType params
                hasTypeVars = not (null tvs)
                hasUntypedParams = any (\(Param _ mt) -> isNothing mt) params
                ret = case retType of
                  Just t -> t
                  Nothing ->
                    if hasTypeVars || hasUntypedParams
                      then TGeneric "__inferred__"
                      else TVoid
                sig = FuncSig paramTypes ret tvs
             in s {csFuncs = Map.insert name sig (csFuncs s)}

    fieldToTuple (Field n t) = (n, t)
    paramType (Param _ mt) = case mt of
      Just t -> t
      Nothing -> TGeneric "__inferred_param__"

checkTopLevel :: CheckerState -> TopLevelDecl -> CheckerState
checkTopLevel st = \case
  StructDecl _ fields -> checkStructFields st fields
  FuncDecl tvs name params retType body ->
    let hasTypeVars = not (null tvs)
        hasUntypedParams = any (\(Param _ mt) -> isNothing mt) params
        ret = case retType of
          Just t -> t
          Nothing ->
            if hasTypeVars || hasUntypedParams
              then TGeneric "__inferred__"
              else TVoid
        st1 = st {csCurrentFunc = Just (name, ret)}
        st2 = pushScope st1
        st3 = foldl (addParamWithTypeVars tvs) st2 params
        st4 = foldl checkStmt st3 body
        st5 = checkReturnPaths name ret body st4
     in popScope $ st5 {csCurrentFunc = Nothing}

addParamWithTypeVars :: [TypeVar] -> CheckerState -> Param -> CheckerState
addParamWithTypeVars _tvs s (Param n mt) =
  let ty = case mt of
        Just t -> t
        Nothing -> TGeneric ("__param_" ++ n ++ "__")
   in case addVar n ty s of
        Left err -> addError err s
        Right s' -> s'

checkStructFields :: CheckerState -> [Field] -> CheckerState
checkStructFields st fields =
  foldl checkField st (zip ([0 ..] :: [Int]) fields)
  where
    checkField s (_, Field n t) =
      if any (\(i, Field n' _) -> n' == n && i < fst (head $ filter (\(_, Field n'' _) -> n'' == n) $ zip ([0 ..] :: [Int]) fields)) (zip ([0 ..] :: [Int]) fields)
        then addError (ScopeError $ DuplicateField n) s
        else checkTypeExists t s

checkTypeExists :: Type -> CheckerState -> CheckerState
checkTypeExists ty st = case ty of
  TRecord name ->
    if Map.member name (csStructs st)
      then st
      else addError (ScopeError $ UndefinedStruct name) st
  TArr elemT _ -> checkTypeExists elemT st
  TFunc params ret -> foldl (flip checkTypeExists) (checkTypeExists ret st) params
  _ -> st

checkReturnPaths :: String -> Type -> [Stmt] -> CheckerState -> CheckerState
checkReturnPaths name retType body st
  | retType == TVoid = st
  | hasReturn body = st
  | otherwise = addError (TypeError $ MissingReturn name) st
  where
    hasReturn [] = False
    hasReturn stmts = case last stmts of
      SReturn _ -> True
      SIf _ thenB (Just elseB) -> hasReturn thenB && hasReturn elseB
      _ -> False

--------------------------------------------------------------------------------
-- Verificação de Statements
--------------------------------------------------------------------------------

checkStmt :: CheckerState -> Stmt -> CheckerState
checkStmt st = \case
  SVarDecl name mType mExpr ->
    case (mType, mExpr) of
      (Just ty, Just expr) ->
        let (exprTy, st1) = inferExpr st expr
         in if isCompatible ty exprTy
              then case addVar name ty st1 of
                Left err -> addError err st1
                Right s -> s
              else addError (TypeError $ TypeMismatch ty exprTy "variable declaration") st1
      (Just ty, Nothing) ->
        case addVar name ty st of
          Left err -> addError err st
          Right s -> s
      (Nothing, Just expr) ->
        let (exprTy, st1) = inferExpr st expr
         in if exprTy == TVoid
              then addError (TypeError VoidInExpression) st1
              else case addVar name exprTy st1 of
                Left err -> addError err st1
                Right s -> s
      (Nothing, Nothing) ->
        addError (TypeError $ CannotInferType name) st
  SAssign target expr ->
    let (targetTy, st1) = inferExpr st target
        (exprTy, st2) = inferExpr st1 expr
     in if isCompatible targetTy exprTy
          then st2
          else addError (TypeError $ TypeMismatch targetTy exprTy "assignment") st2
  SIf cond thenB elseB ->
    let (condTy, st1) = inferExpr st cond
        st2 =
          if condTy == TBool
            then st1
            else addError (TypeError $ InvalidCondition condTy) st1
        st3 = foldl checkStmt (pushScope st2) thenB
        st4 = popScope st3
     in case elseB of
          Nothing -> st4
          Just els ->
            let st5 = foldl checkStmt (pushScope st4) els
             in popScope st5
  SWhile cond body ->
    let (condTy, st1) = inferExpr st cond
        st2 =
          if condTy == TBool
            then st1
            else addError (TypeError $ InvalidCondition condTy) st1
        st3 = foldl checkStmt (pushScope st2) body
     in popScope st3
  SFor varName initE condE updateE body ->
    let st1 = pushScope st
        (initTy, st2) = inferExpr st1 initE
        st3 = case addVar varName initTy st2 of
          Left err -> addError err st2
          Right s -> s
        (condTy, st4) = inferExpr st3 condE
        st5 =
          if condTy == TBool
            then st4
            else addError (TypeError $ InvalidCondition condTy) st4
        (_, st6) = inferExpr st5 updateE
        st7 = foldl checkStmt st6 body
     in popScope st7
  SReturn mExpr ->
    case csCurrentFunc st of
      Nothing -> st
      Just (_, expectedRet) ->
        case mExpr of
          Nothing ->
            if expectedRet == TVoid
              then st
              else addError (TypeError $ InvalidReturnType TVoid expectedRet) st
          Just expr ->
            let (exprTy, st1) = inferExpr st expr
             in if isCompatible expectedRet exprTy
                  then st1
                  else addError (TypeError $ InvalidReturnType exprTy expectedRet) st1
  SExpr expr ->
    let (_, st1) = inferExpr st expr
     in st1

--------------------------------------------------------------------------------
-- Inferência de Tipos de Expressões
--------------------------------------------------------------------------------

inferExpr :: CheckerState -> Expr -> (Type, CheckerState)
inferExpr st = \case
  EInt _ -> (TInt, st)
  EFloat _ -> (TFloat, st)
  EString _ -> (TString, st)
  EBool _ -> (TBool, st)
  EVar name ->
    case lookupVar name st of
      Just ty -> (ty, st)
      Nothing -> (TInt, addError (ScopeError $ UndefinedVariable name) st)
  EArrayAccess arrE idxE ->
    let (arrTy, st1) = inferExpr st arrE
        (idxTy, st2) = inferExpr st1 idxE
        st3 =
          if idxTy == TInt
            then st2
            else addError (TypeError $ InvalidArrayIndex idxTy) st2
     in case arrTy of
          TArr elemTy _ -> (elemTy, st3)
          _ -> (TInt, addError (TypeError $ NotAnArray arrTy) st3)
  EFieldAccess objE fieldName ->
    let (objTy, st1) = inferExpr st objE
     in case objTy of
          TRecord structName ->
            case lookupField structName fieldName st1 of
              Just fieldTy -> (fieldTy, st1)
              Nothing -> (TInt, addError (ScopeError $ UndefinedField structName fieldName) st1)
          TArr _ _ ->
            if fieldName == "size"
              then (TInt, st1)
              else (TInt, addError (ScopeError $ UndefinedField "array" fieldName) st1)
          TGeneric _ ->
            (TInt, st1)
          _ -> (TInt, addError (TypeError $ NotAStruct objTy) st1)
  EBinOp op left right ->
    let (leftTy, st1) = inferExpr st left
        (rightTy, st2) = inferExpr st1 right
     in inferBinOp op leftTy rightTy st2
  EUnaryOp op expr ->
    let (exprTy, st1) = inferExpr st expr
     in inferUnaryOp op exprTy st1
  ECall funcName args ->
    case lookupFunc funcName st of
      Nothing ->
        if funcName == "print"
          then
            let st1 = foldl (\s e -> snd $ inferExpr s e) st args
             in (TVoid, st1)
          else case lookupVar funcName st of
            Just (TFunc paramTypes retType) ->
              let (argTypes, st1) = inferArgs st args
                  st2 =
                    if length argTypes /= length paramTypes
                      then addError (TypeError $ WrongNumberOfArgs funcName (length paramTypes) (length argTypes)) st1
                      else checkArgTypes funcName paramTypes argTypes st1
               in (retType, st2)
            Just _ -> (TInt, addError (ScopeError $ UndefinedFunction funcName) st)
            Nothing -> (TInt, addError (ScopeError $ UndefinedFunction funcName) st)
      Just (FuncSig paramTypes retType _) ->
        let (argTypes, st1) = inferArgs st args
            st2 =
              if length argTypes /= length paramTypes
                then addError (TypeError $ WrongNumberOfArgs funcName (length paramTypes) (length argTypes)) st1
                else checkArgTypes funcName paramTypes argTypes st1
         in (retType, st2)
  ENewArray elemType sizeE ->
    let (sizeTy, st1) = inferExpr st sizeE
        st2 =
          if sizeTy == TInt
            then st1
            else addError (TypeError $ InvalidArrayIndex sizeTy) st1
     in (TArr elemType Nothing, st2)
  EArrayLit elems ->
    case elems of
      [] -> (TArr TInt Nothing, st)
      (e : es) ->
        let (firstTy, st1) = inferExpr st e
            (_, st2) = foldl inferAndCheck (firstTy, st1) es
         in (TArr firstTy Nothing, st2)
    where
      inferAndCheck (expectedTy, s) expr =
        let (ty, s') = inferExpr s expr
         in if isCompatible expectedTy ty
              then (expectedTy, s')
              else (expectedTy, addError (TypeError $ TypeMismatch expectedTy ty "array literal") s')
  EStructLit structName fieldExprs ->
    case lookupStruct structName st of
      Nothing -> (TRecord structName, addError (ScopeError $ UndefinedStruct structName) st)
      Just fields ->
        if length fieldExprs /= length fields
          then (TRecord structName, addError (TypeError $ WrongNumberOfArgs structName (length fields) (length fieldExprs)) st)
          else
            let (_, st1) = foldl checkFieldExpr (fields, st) fieldExprs
             in (TRecord structName, st1)
    where
      checkFieldExpr ([], s) _ = ([], s)
      checkFieldExpr ((_, expectedTy) : rest, s) expr =
        let (exprTy, s') = inferExpr s expr
         in if isCompatible expectedTy exprTy
              then (rest, s')
              else (rest, addError (TypeError $ TypeMismatch expectedTy exprTy "struct literal") s')

inferArgs :: CheckerState -> [Expr] -> ([Type], CheckerState)
inferArgs st = foldl go ([], st)
  where
    go (types, s) e =
      let (t, s') = inferExpr s e
       in (types ++ [t], s')

checkArgTypes :: String -> [Type] -> [Type] -> CheckerState -> CheckerState
checkArgTypes _ [] [] st = st
checkArgTypes funcName (p : ps) (a : as) st =
  let st' =
        if isCompatible p a
          then st
          else addError (TypeError $ TypeMismatch p a ("argument to " ++ funcName)) st
   in checkArgTypes funcName ps as st'
checkArgTypes _ _ _ st = st

inferBinOp :: BinOp -> Type -> Type -> CheckerState -> (Type, CheckerState)
inferBinOp op leftTy rightTy st
  | isArithOp op =
      case (leftTy, rightTy) of
        (TInt, TInt) -> (TInt, st)
        (TFloat, TFloat) -> (TFloat, st)
        (TInt, TFloat) -> (TFloat, st)
        (TFloat, TInt) -> (TFloat, st)
        _ -> (TInt, addError (TypeError $ InvalidOperator op leftTy rightTy) st)
  | isRelOp op =
      case (leftTy, rightTy) of
        (TInt, TInt) -> (TBool, st)
        (TFloat, TFloat) -> (TBool, st)
        (TInt, TFloat) -> (TBool, st)
        (TFloat, TInt) -> (TBool, st)
        (TString, TString) -> (TBool, st)
        (TBool, TBool) -> (TBool, st)
        _ -> (TBool, addError (TypeError $ InvalidOperator op leftTy rightTy) st)
  | isLogicOp op =
      if leftTy == TBool && rightTy == TBool
        then (TBool, st)
        else (TBool, addError (TypeError $ InvalidOperator op leftTy rightTy) st)
  | otherwise = (TInt, addError (TypeError $ InvalidOperator op leftTy rightTy) st)

inferUnaryOp :: UnaryOp -> Type -> CheckerState -> (Type, CheckerState)
inferUnaryOp op exprTy st = case op of
  OpNeg ->
    if exprTy `elem` [TInt, TFloat]
      then (exprTy, st)
      else (TInt, addError (TypeError $ InvalidUnaryOperator op exprTy) st)
  OpNot ->
    if exprTy == TBool
      then (TBool, st)
      else (TBool, addError (TypeError $ InvalidUnaryOperator op exprTy) st)
  OpPostInc ->
    if exprTy == TInt
      then (TInt, st)
      else (TInt, addError (TypeError $ InvalidUnaryOperator op exprTy) st)
  OpPostDec ->
    if exprTy == TInt
      then (TInt, st)
      else (TInt, addError (TypeError $ InvalidUnaryOperator op exprTy) st)

isCompatible :: Type -> Type -> Bool
isCompatible TInt TInt = True
isCompatible TFloat TFloat = True
isCompatible TFloat TInt = True
isCompatible TString TString = True
isCompatible TBool TBool = True
isCompatible TVoid TVoid = True
isCompatible (TArr t1 _) (TArr t2 _) = isCompatible t1 t2
isCompatible (TRecord n1) (TRecord n2) = n1 == n2
isCompatible (TFunc p1 r1) (TFunc p2 r2) =
  length p1 == length p2
    && all (uncurry isCompatible) (zip p1 p2)
    && isCompatible r1 r2
isCompatible (TGeneric _) _ = True
isCompatible _ (TGeneric _) = True
isCompatible _ _ = False

--------------------------------------------------------------------------------
-- Inferência de Tipos
--------------------------------------------------------------------------------

inferType :: Program -> Expr -> Either [SemanticError] Type
inferType prog expr =
  let st = checkProgramState prog initState
   in if null (csErrors st)
        then
          let (ty, st') = inferExpr st expr
           in if null (csErrors st')
                then Right ty
                else Left (reverse $ csErrors st')
        else Left (reverse $ csErrors st)

--------------------------------------------------------------------------------
-- Formatação de Erros
--------------------------------------------------------------------------------

formatSemanticError :: SemanticError -> String
formatSemanticError = \case
  TypeError te -> formatTypeError te
  ScopeError se -> formatScopeError se

formatTypeError :: TypeError -> String
formatTypeError = \case
  TypeMismatch expected actual ctx ->
    "Erro de tipo: esperado " ++ showType expected ++ ", encontrado " ++ showType actual ++ " em " ++ ctx
  InvalidOperator op t1 t2 ->
    "Operador invalido: " ++ show op ++ " nao pode ser aplicado a " ++ showType t1 ++ " e " ++ showType t2
  InvalidUnaryOperator op t ->
    "Operador unario invalido: " ++ show op ++ " nao pode ser aplicado a " ++ showType t
  NotAnArray t ->
    "Esperado array, encontrado " ++ showType t
  NotAStruct t ->
    "Esperado struct, encontrado " ++ showType t
  NotAFunction name ->
    "'" ++ name ++ "' nao e uma funcao"
  WrongNumberOfArgs name expected actual ->
    "Numero incorreto de argumentos para '" ++ name ++ "': esperado " ++ show expected ++ ", recebido " ++ show actual
  InvalidArrayIndex t ->
    "Indice de array invalido: esperado int, encontrado " ++ showType t
  CannotInferType name ->
    "Nao foi possivel inferir o tipo de '" ++ name ++ "'"
  VoidInExpression ->
    "Expressao void nao pode ser usada como valor"
  InvalidReturnType actual expected ->
    "Tipo de retorno invalido: esperado " ++ showType expected ++ ", encontrado " ++ showType actual
  MissingReturn name ->
    "Funcao '" ++ name ++ "' pode nao retornar valor"
  InvalidCondition t ->
    "Condicao invalida: esperado bool, encontrado " ++ showType t

formatScopeError :: ScopeError -> String
formatScopeError = \case
  UndefinedVariable name ->
    "Variavel indefinida: '" ++ name ++ "'"
  UndefinedFunction name ->
    "Funcao indefinida: '" ++ name ++ "'"
  UndefinedStruct name ->
    "Struct indefinida: '" ++ name ++ "'"
  UndefinedField struct field ->
    "Campo indefinido: '" ++ field ++ "' em struct '" ++ struct ++ "'"
  DuplicateVariable name ->
    "Variavel duplicada: '" ++ name ++ "'"
  DuplicateFunction name ->
    "Funcao duplicada: '" ++ name ++ "'"
  DuplicateStruct name ->
    "Struct duplicada: '" ++ name ++ "'"
  DuplicateField name ->
    "Campo duplicado: '" ++ name ++ "'"

showType :: Type -> String
showType = \case
  TInt -> "int"
  TFloat -> "float"
  TString -> "string"
  TBool -> "bool"
  TVoid -> "void"
  TArr t _ -> showType t ++ "[]"
  TRecord name -> name
  TGeneric name -> name
  TFunc params ret -> "(" ++ unwords (map showType params) ++ ") -> " ++ showType ret
  TMeta n -> "?T" ++ show n
