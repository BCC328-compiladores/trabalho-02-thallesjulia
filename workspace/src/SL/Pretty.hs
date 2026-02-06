{-# LANGUAGE LambdaCase #-}

-- | Pretty printer para a linguagem SL
module SL.Pretty
  ( Pretty (..),
    pp,
    ppType,
    ppBinOp,
    ppUnaryOp,
  )
where

import SL.AST
import Text.PrettyPrint
import Prelude hiding ((<>))

--------------------------------------------------------------------------------
-- Classe
--------------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> Doc

pp :: (Pretty a) => a -> String
pp = render . pretty

--------------------------------------------------------------------------------
-- Tipos
--------------------------------------------------------------------------------

instance Pretty Type where
  pretty = \case
    TInt -> text "int"
    TFloat -> text "float"
    TString -> text "string"
    TBool -> text "bool"
    TVoid -> text "void"
    TArr t Nothing -> pretty t <> text "[]"
    TArr t (Just n) -> pretty t <> text "[" <> int n <> text "]"
    TRecord n -> text n
    TGeneric n -> text n
    TMeta i -> text "?" <> int i
    TFunc ps r -> parens (commaSep $ map pretty ps) <+> text "->" <+> pretty r

ppType :: Type -> String
ppType = pp

--------------------------------------------------------------------------------
-- Operadores
--------------------------------------------------------------------------------

instance Pretty BinOp where
  pretty = \case
    OpAdd -> text "+"
    OpSub -> text "-"
    OpMul -> text "*"
    OpDiv -> text "/"
    OpEq -> text "=="
    OpNeq -> text "!="
    OpLt -> text "<"
    OpGt -> text ">"
    OpLe -> text "<="
    OpGe -> text ">="
    OpAnd -> text "&&"
    OpOr -> text "||"

instance Pretty UnaryOp where
  pretty = \case
    OpNeg -> text "-"
    OpNot -> text "!"
    OpPostInc -> text "++"
    OpPostDec -> text "--"

ppBinOp :: BinOp -> String
ppBinOp = pp

ppUnaryOp :: UnaryOp -> String
ppUnaryOp = pp

--------------------------------------------------------------------------------
-- Expressões
--------------------------------------------------------------------------------

instance Pretty Expr where
  pretty = \case
    EInt n -> int n
    EFloat d -> double d
    EString s -> text (show s)
    EBool True -> text "true"
    EBool False -> text "false"
    EVar name -> text name
    EArrayAccess a i -> pretty a <> brackets (pretty i)
    EFieldAccess e f -> pretty e <> text "." <> text f
    EBinOp op l r -> prettyBin l <+> pretty op <+> prettyBin r
    EUnaryOp OpPostInc e -> prettyAtom e <> text "++"
    EUnaryOp OpPostDec e -> prettyAtom e <> text "--"
    EUnaryOp op e -> pretty op <> prettyAtom e
    ECall f args -> text f <> parens (commaSep $ map pretty args)
    ENewArray t sz -> text "new" <+> pretty t <> brackets (pretty sz)
    EArrayLit es -> brackets (commaSep $ map pretty es)
    EStructLit n es -> text n <> braces (commaSep $ map pretty es)

prettyAtom :: Expr -> Doc
prettyAtom e@EBinOp {} = parens (pretty e)
prettyAtom e = pretty e

prettyBin :: Expr -> Doc
prettyBin e@EBinOp {} = parens (pretty e)
prettyBin e = pretty e

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

instance Pretty Stmt where
  pretty = \case
    SVarDecl name mty mval ->
      text "let"
        <+> text name
        <> maybe empty (\t -> text " :" <+> pretty t) mty
        <> maybe empty (\e -> text " =" <+> pretty e) mval
        <> text ";"
    SAssign lhs rhs ->
      pretty lhs <+> text "=" <+> pretty rhs <> text ";"
    SIf cond th mel ->
      text "if" <+> parens (pretty cond) <+> text "{"
        $$ nest 2 (vcat $ map pretty th)
        $$ text "}"
          <> maybe empty (\el -> text " else {" $$ nest 2 (vcat $ map pretty el) $$ text "}") mel
    SWhile cond body ->
      text "while" <+> parens (pretty cond) <+> text "{"
        $$ nest 2 (vcat $ map pretty body)
        $$ text "}"
    SFor var ini cond inc body ->
      text "for"
        <+> parens (text var <+> text "=" <+> pretty ini <> semi' <+> pretty cond <> semi' <+> pretty inc)
        <+> text "{"
        $$ nest 2 (vcat $ map pretty body)
        $$ text "}"
    SReturn mexpr ->
      text "return" <> maybe empty (\e -> space <> pretty e) mexpr <> text ";"
    SExpr e -> pretty e <> text ";"
    where
      semi' = text ";"

--------------------------------------------------------------------------------
-- Parâmetros e Campos
--------------------------------------------------------------------------------

instance Pretty Param where
  pretty (Param name mty) =
    text name <> maybe empty (\t -> text " :" <+> pretty t) mty

instance Pretty Field where
  pretty (Field name ty) = text name <+> text ":" <+> pretty ty <> text ";"

--------------------------------------------------------------------------------
-- Declarações Top-Level
--------------------------------------------------------------------------------

instance Pretty TopLevelDecl where
  pretty = \case
    FuncDecl tvs name params mret body ->
      (if null tvs then empty else text "forall" <+> hsep (map text tvs) <+> text ".")
        <+> text "func"
        <+> text name
        <> parens (commaSep $ map pretty params)
        <+> maybe empty (\t -> text ":" <+> pretty t) mret
        <+> text "{"
        $$ nest 2 (vcat $ map pretty body)
        $$ text "}"
    StructDecl name fields ->
      text "struct" <+> text name <+> text "{"
        $$ nest 2 (vcat $ map pretty fields)
        $$ text "}"

--------------------------------------------------------------------------------
-- Programa
--------------------------------------------------------------------------------

instance Pretty Program where
  pretty (Program decls) = vcat (map pretty decls)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma
