{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}

-- | AST da linguagem SL, parametrizada para suportar anotações (tipos, spans, etc.)
module SL.AST
  ( -- * Posição no código fonte
    Pos (..),
    Span (..),
    HasSpan (..),
    dummySpan,

    -- * Tipos
    Type (..),
    TypeVar,

    -- * Operadores
    BinOp (..),
    UnaryOp (..),
    isArithOp,
    isRelOp,
    isLogicOp,

    -- * Expressões
    Expr' (..),
    Expr,
    pattern EInt,
    pattern EFloat,
    pattern EString,
    pattern EBool,
    pattern EVar,
    pattern EArrayAccess,
    pattern EFieldAccess,
    pattern EBinOp,
    pattern EUnaryOp,
    pattern ECall,
    pattern ENewArray,
    pattern EArrayLit,
    pattern EStructLit,

    -- * Statements
    Stmt' (..),
    Stmt,
    Block,
    pattern SVarDecl,
    pattern SAssign,
    pattern SIf,
    pattern SWhile,
    pattern SFor,
    pattern SReturn,
    pattern SExpr,

    -- * Parâmetros e Campos
    Param' (..),
    Param,
    pattern Param,
    Field' (..),
    Field,
    pattern Field,

    -- * Declarações
    TopLevelDecl' (..),
    TopLevelDecl,
    pattern FuncDecl,
    pattern StructDecl,
    Program' (..),
    Program,
    pattern Program,
  )
where

--------------------------------------------------------------------------------
-- Posição e Span no código fonte
--------------------------------------------------------------------------------

data Pos = Pos {posLine :: !Int, posCol :: !Int}
  deriving (Show, Eq, Ord)

data Span = Span {spanStart :: !Pos, spanEnd :: !Pos}
  deriving (Show, Eq)

dummySpan :: Span
dummySpan = Span (Pos 0 0) (Pos 0 0)

class HasSpan a where
  getSpan :: a -> Span

--------------------------------------------------------------------------------
-- Sistema de Tipos
--------------------------------------------------------------------------------

data Type
  = TInt
  | TFloat
  | TString
  | TBool
  | TVoid
  | TArr Type (Maybe Int) -- element type, optional size
  | TRecord String
  | TGeneric String
  | TFunc [Type] Type
  | TMeta Int
  deriving (Show, Eq)

type TypeVar = String

--------------------------------------------------------------------------------
-- Operadores
--------------------------------------------------------------------------------

data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEq
  | OpNeq
  | OpLt
  | OpGt
  | OpLe
  | OpGe
  | OpAnd
  | OpOr
  deriving (Show, Eq, Ord, Enum, Bounded)

data UnaryOp
  = OpNeg
  | OpNot
  | OpPostInc
  | OpPostDec
  deriving (Show, Eq, Ord, Enum, Bounded)

isArithOp, isRelOp, isLogicOp :: BinOp -> Bool
isArithOp op = op `elem` [OpAdd, OpSub, OpMul, OpDiv]
isRelOp op = op `elem` [OpEq, OpNeq, OpLt, OpGt, OpLe, OpGe]
isLogicOp op = op `elem` [OpAnd, OpOr]

--------------------------------------------------------------------------------
-- Expressões (parametrizadas por anotação)
--------------------------------------------------------------------------------

data Expr' a
  = EInt' a Int
  | EFloat' a Double
  | EString' a String
  | EBool' a Bool
  | EVar' a String
  | EArrayAccess' a (Expr' a) (Expr' a)
  | EFieldAccess' a (Expr' a) String
  | EBinOp' a BinOp (Expr' a) (Expr' a)
  | EUnaryOp' a UnaryOp (Expr' a)
  | ECall' a String [Expr' a]
  | ENewArray' a Type (Expr' a)
  | EArrayLit' a [Expr' a]
  | EStructLit' a String [Expr' a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Expr = Expr' ()

pattern EInt :: Int -> Expr
pattern EInt n = EInt' () n

pattern EFloat :: Double -> Expr
pattern EFloat d = EFloat' () d

pattern EString :: String -> Expr
pattern EString s = EString' () s

pattern EBool :: Bool -> Expr
pattern EBool b = EBool' () b

pattern EVar :: String -> Expr
pattern EVar s = EVar' () s

pattern EArrayAccess :: Expr -> Expr -> Expr
pattern EArrayAccess arr idx = EArrayAccess' () arr idx

pattern EFieldAccess :: Expr -> String -> Expr
pattern EFieldAccess e f = EFieldAccess' () e f

pattern EBinOp :: BinOp -> Expr -> Expr -> Expr
pattern EBinOp op l r = EBinOp' () op l r

pattern EUnaryOp :: UnaryOp -> Expr -> Expr
pattern EUnaryOp op e = EUnaryOp' () op e

pattern ECall :: String -> [Expr] -> Expr
pattern ECall f args = ECall' () f args

pattern ENewArray :: Type -> Expr -> Expr
pattern ENewArray t sz = ENewArray' () t sz

pattern EArrayLit :: [Expr] -> Expr
pattern EArrayLit es = EArrayLit' () es

pattern EStructLit :: String -> [Expr] -> Expr
pattern EStructLit n es = EStructLit' () n es

{-# COMPLETE
  EInt,
  EFloat,
  EString,
  EBool,
  EVar,
  EArrayAccess,
  EFieldAccess,
  EBinOp,
  EUnaryOp,
  ECall,
  ENewArray,
  EArrayLit,
  EStructLit
  #-}

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Stmt' a
  = SVarDecl' a String (Maybe Type) (Maybe (Expr' a))
  | SAssign' a (Expr' a) (Expr' a)
  | SIf' a (Expr' a) [Stmt' a] (Maybe [Stmt' a])
  | SWhile' a (Expr' a) [Stmt' a]
  | SFor' a String (Expr' a) (Expr' a) (Expr' a) [Stmt' a]
  | SReturn' a (Maybe (Expr' a))
  | SExpr' a (Expr' a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Stmt = Stmt' ()

type Block = [Stmt]

pattern SVarDecl :: String -> Maybe Type -> Maybe Expr -> Stmt
pattern SVarDecl n t e = SVarDecl' () n t e

pattern SAssign :: Expr -> Expr -> Stmt
pattern SAssign l r = SAssign' () l r

pattern SIf :: Expr -> [Stmt] -> Maybe [Stmt] -> Stmt
pattern SIf c t e = SIf' () c t e

pattern SWhile :: Expr -> [Stmt] -> Stmt
pattern SWhile c b = SWhile' () c b

pattern SFor :: String -> Expr -> Expr -> Expr -> [Stmt] -> Stmt
pattern SFor v i c u b = SFor' () v i c u b

pattern SReturn :: Maybe Expr -> Stmt
pattern SReturn e = SReturn' () e

pattern SExpr :: Expr -> Stmt
pattern SExpr e = SExpr' () e

{-# COMPLETE SVarDecl, SAssign, SIf, SWhile, SFor, SReturn, SExpr #-}

--------------------------------------------------------------------------------
-- Parâmetros e Campos
--------------------------------------------------------------------------------

data Param' a = Param' a String (Maybe Type)
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Param = Param' ()

pattern Param :: String -> Maybe Type -> Param
pattern Param n t = Param' () n t

{-# COMPLETE Param #-}

data Field' a = Field' a String Type
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Field = Field' ()

pattern Field :: String -> Type -> Field
pattern Field n t = Field' () n t

{-# COMPLETE Field #-}

--------------------------------------------------------------------------------
-- Declarações Top-Level
--------------------------------------------------------------------------------

data TopLevelDecl' a
  = FuncDecl' a [TypeVar] String [Param' a] (Maybe Type) [Stmt' a]
  | StructDecl' a String [Field' a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type TopLevelDecl = TopLevelDecl' ()

pattern FuncDecl :: [TypeVar] -> String -> [Param] -> Maybe Type -> Block -> TopLevelDecl
pattern FuncDecl tvs n ps rt b = FuncDecl' () tvs n ps rt b

pattern StructDecl :: String -> [Field] -> TopLevelDecl
pattern StructDecl n fs = StructDecl' () n fs

{-# COMPLETE FuncDecl, StructDecl #-}

--------------------------------------------------------------------------------
-- Programa
--------------------------------------------------------------------------------

newtype Program' a = Program' [TopLevelDecl' a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Program = Program' ()

pattern Program :: [TopLevelDecl] -> Program
pattern Program ds = Program' ds

{-# COMPLETE Program #-}
