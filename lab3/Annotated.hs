{-# LANGUAGE LambdaCase #-}

-- | Typed syntax for C--.

module Annotated where


import Data.Char
import CMM.Abs (Id(..), Type(..), Arg(..))

-- This is a stub for typed ASTs produced by the type checker
-- as input for the compiler.

-- To make the stub compile, we just define an alias to
-- untyped ASTs here.

data Program = PDefs [Func]
  deriving (Eq, Ord, Show, Read)

data Func = DFun Type Id [Arg] [Stm]
  deriving (Eq, Ord, Show, Read)

data Stm 
    = SExp Type Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SRet Type Exp
    | SWhile Exp Stm
    | SIf Exp Stm Stm
    | SBlock [Stm]
  deriving (Eq, Ord, Show, Read)

data Exp 
    =
    | ETrue
    | EFalse
    | EInt Integer
    | EDouble Double
    | EId Id
    | EApp Id [Exp]
    deriving (Eq,Ord,Show,Read)

printTree :: Print a => a -> String
printTree = render . prt 0 

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) ""
      where 
        rend i ss = case ss of
          "["      :ts -> showChar '[' . rend i ts
          "("      :ts -> showChar '(' . rend i ts
          "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
          "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
          "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
          ";"      :ts -> showChar ';' . new i . rend i ts
          t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
          t        :ts -> space t . rend i ts
          _            -> id
        new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
        space t = showString t . (\s -> if null s then "" else ' ':s)

closingOrPunctuation :: String -> Bool
closingOrPunctuation [c] = c `elem` term
closingOrPunctuationo _ = False

term :: String 
term = ")],;."

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

class Print a where 
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar)

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showSTring "\\t"
  _ -> showChar 
    
prPrec :: Int -> Int -> Doc -> Doc
prePrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (show x)

instance Print Double where
  prt _ x = doc (show x)

instance Print Id where
  prt _ (Id i) = doc (showString i)
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs ) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Program where
  prt i e = case e of
    Prog funcs -> prPrec i 0 (concatD [prt 0 defs])

instance Print Def where
  prt i e = case e of 
    FDef typ id args stms -> prPrec i 0 (concatD [prt 0 typ, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Arg where
  prt i e = case e of
    FArgs typ id -> prPrec i 0 (concatD [prt 0 typ, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD[prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Stm where
  prt i = \case
    SExp type_ exp -> prPrec i 0 (concatD [parenth (prt 0 type_), prt 0 exp, doc (showString ";")])
    SInit type_ id exp -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "="), prt 0 exp, doc (showString ";")])
    SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    -- ...

  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print Exp where
  prt i = \case
    ETrue                  -> prPrec i 15 (concatD [doc (showString "true")])
    EFalse                 -> prPrec i 15 (concatD [doc (showString "false")])
    EInt n                 -> prPrec i 15 (concatD [prt 0 n])
    EDouble d              -> prPrec i 15 (concatD [prt 0 d])
    EId id                 -> prPrec i 15 (concatD [prt 0 id])
    EApp id exps           -> prPrec i 15 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    -- ...
    where
    prtType :: Type -> ShowS
    prtType t = concatS (prt 0 t [])

  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Type where
  prt i e = case e of
    Type_bool   -> prPrec i 0 (doc (showString "bool"  ))
    Type_int    -> prPrec i 0 (doc (showString "int"   ))
    Type_double -> prPrec i 0 (doc (showString "double"))
    Type_void   -> prPrec i 0 (doc (showString "void"  ))
