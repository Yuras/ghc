
module MiniStg
where

import Data.Char
import qualified Data.List as List
--import Debug.Trace

import StgSyn
import Outputable
import Id
import Name
import Module
import DataCon
import Literal
import PrimOp
import qualified Var
import CoreSyn (AltCon(..))

type Program = [Decl]

data Decl = Decl Variable Object

type Variable = String

data Object
  = Fun [Variable] Exp
  | Con Con [Atom]
  | Thunk Exp

data Atom
  = Lit Lit
  | Var Variable

type Con = String

data Exp
  = Atom Atom
  | App Variable [Atom]
  | PrimApp Prim [Atom]
  | Let Variable Object Exp
  | Case Exp [Alt]

data Lit
  = Int Int

data Prim
  = Add
  | Subtract
  | Multiply
  | Equal
  | LessThen
  | GreaterThen
  | LessThenEq
  | GreaterThenEq
  | IntToBool

data Alt
  = Pat Con [Variable] Exp
  | Default Variable Exp

pprMiniStgBindings :: [StgBinding] -> SDoc
pprMiniStgBindings binds
  = vcat
  . List.intersperse blankLine
  . map pprDecl
  . convertStgBindings
  $ binds

pprDecl :: Decl -> SDoc
pprDecl (Decl var o) = hsep [text var, text "=", pprObject o] <> text ";"

pprObject :: Object -> SDoc
pprObject (Fun vars body) =
  text "FUN" <> parens (hsep [hsep (map text vars), text "->", pprExp body])
pprObject (Con con args) =
  text "CON" <> parens (hsep [text con, hsep (map pprAtom args)])
pprObject (Thunk body) =
  text "THUNK" <> parens (pprExp body)

pprAtom :: Atom -> SDoc
pprAtom (Lit (Int i)) = int i
pprAtom (Var v) = text v

pprExp :: Exp -> SDoc
pprExp (Atom a) = pprAtom a
pprExp (App f args) = hsep (text f : map pprAtom args)
pprExp (Let b e body) = vcat
  [ text "let {"
  , hsep [text b, text "=", pprObject e]
  , text "} in"
  , pprExp body
  ]
pprExp (Case e alts) = vcat
  [ hsep [text "case", pprExp e, text "of {"]
  , vcat (map pprAlt alts)
  , text "}"
  ]
pprExp (PrimApp p args) = hsep (text f : map pprAtom args)
  where
  f = case p of
    Add -> "plus#"
    Subtract -> "sub#"
    Multiply -> "mult#"
    Equal -> "eq#"
    LessThen -> "lt#"
    GreaterThen -> "gt#"
    LessThenEq -> "lte#"
    GreaterThenEq -> "gte#"
    IntToBool -> "intToBool#"

pprAlt :: Alt -> SDoc
pprAlt (Pat con vars body) =
  hsep ([text con] ++ map text vars ++ [text "->", pprExp body]) <> text ";"
pprAlt (Default var body) =
  hsep [text var, text "->", pprExp body] <> text ";"

convertStgBindings :: [StgBinding] -> Program
convertStgBindings = concatMap convertStgBinding . filter good

good :: StgBinding -> Bool
good (StgNonRec _ rhs) = goodRhs rhs
good (StgRec bs) = all (goodRhs . snd) bs
goodRhs :: StgRhs -> Bool
goodRhs (StgRhsCon _ con _) = case occNameString (getOccName con) of
  "TrNameS" -> False
  "Module" -> False
  "TyCon" -> False
  _ -> True
goodRhs _ = True

convertStgBinding :: StgBinding -> [Decl]
convertStgBinding (StgNonRec b rhs) =
  [Decl (idToVar b) (convertStgRhs rhs)]
convertStgBinding (StgRec bindings) =
  concatMap (\(b, rhs) -> convertStgBinding (StgNonRec b rhs)) bindings

convertStgRhs :: StgRhs -> Object
convertStgRhs (StgRhsClosure _ _ _ _ [] body) = convertThunk body
convertStgRhs (StgRhsClosure _ _ _ _ args body) = convertFunc args body
convertStgRhs (StgRhsCon _ con args) = convertCon con args

convertThunk :: StgExpr -> Object
convertThunk e = Thunk (convertStgExpr e)

convertStgExpr :: StgExpr -> Exp
convertStgExpr (StgLit lit) = Atom (Lit (convertLit lit))
convertStgExpr (StgApp f args) = App (idToVar f) (convertArgs args)
convertStgExpr (StgLet (StgNonRec b e) body) =
  Let (idToVar b) (convertStgRhs e) (convertStgExpr body)
convertStgExpr (StgLet (StgRec [(b, e)]) body) =
  Let (idToVar b) (convertStgRhs e) (convertStgExpr body)
convertStgExpr (StgLetNoEscape (StgNonRec b e) body) =
  Let (idToVar b) (convertStgRhs e) (convertStgExpr body)
convertStgExpr (StgLetNoEscape (StgRec [(b, e)]) body) =
  Let (idToVar b) (convertStgRhs e) (convertStgExpr body)
convertStgExpr (StgTick _ e) = convertStgExpr e
convertStgExpr (StgCase e b _ alts) =
  let alts' = convertAlts b alts
      b' = idToVar b
      inner = Default b' (Case (Atom $ Var b') alts')
  in Case (convertStgExpr e) [inner]
convertStgExpr (StgOpApp op args _) = convertPrim op args
convertStgExpr _ = Atom (Lit (Int 999)) --error "convertStgExpr"

convertStgExpr' :: StgExpr -> Exp
convertStgExpr' (StgApp f args) =
  Let "res_var_"
      (Thunk (App (idToVar f) (convertArgs args)))
      (Atom (Var "res_var_"))
convertStgExpr' (StgConApp con args) =
  Let "tmp_var_"
      (convertCon con args)
      (Atom (Var "tmp_var_"))
convertStgExpr' e = convertStgExpr e

convertPrim :: StgOp -> [StgArg] -> Exp
convertPrim (StgPrimOp IntAddOp) args = PrimApp Add (convertArgs args)
convertPrim (StgPrimOp IntSubOp) args = PrimApp Subtract (convertArgs args)
convertPrim (StgPrimOp IntMulOp) args = PrimApp Multiply (convertArgs args)
convertPrim _ _ = error "convertPrim"

convertAlts :: Id -> [StgAlt] -> [Alt]
convertAlts b alts
  | Just (maybe_def, lits) <- findLits
  = convertLitAlts b maybe_def lits
  where
  findLits = go Nothing [] alts
    where
    go _ [] [] = Nothing
    go d r [] = Just (d, reverse r)
    go d r (a:as)
      | (LitAlt _, _, _) <- a
      = go d (a:r) as
      | (DEFAULT, _, _) <- a
      = go (Just a) r as
      | otherwise
      = Nothing
convertAlts b alts = map (convertAlt b) alts

convertAlt :: Id -> StgAlt -> Alt
convertAlt b (DEFAULT, _, e) = Default (idToVar b) (convertStgExpr' e)
convertAlt _ (DataAlt con, args, e) =
  Pat (dataConToVar con) (map idToVar args) (convertStgExpr' e)
convertAlt _ _ = error "convertAlt"

convertLitAlts :: Id -> Maybe StgAlt -> [StgAlt] -> [Alt]
convertLitAlts b md as =
  let b' = idToVar b
      e = makeLitAlts b' (filter notNeg as)
      notNeg (LitAlt l, _, _) =
        case convertLit l of
          Int i -> i >= 0
      notNeg _ = True
  in [Default b' e]
  where
  makeLitAlts b' [] = case md of
    Nothing -> Atom (Lit (Int 0))
    Just (DEFAULT, _, e) ->
      Case (Atom (Var b')) [Default b' (convertStgExpr' e)]
    _ -> error "makeLitAlts.case"
  makeLitAlts b' ((LitAlt lit, _, e) : as') =
    let l = convertLit lit
        trueAlt = Pat "True" [] (convertStgExpr' e)
        falseAlt = Pat "False" [] (makeLitAlts b' as')
        expr = Case (PrimApp Equal [Var b', Lit l]) [Default "tmp_var" expr']
        expr' = PrimApp IntToBool [Var "tmp_var"]
    in Case (expr) [trueAlt, falseAlt]
  makeLitAlts _ _ = error "makeLitAlts"

convertLit :: Literal -> Lit
convertLit (LitInteger i _) = Int (fromInteger i)
convertLit (MachInt i) = Int (fromInteger i)
convertLit _ = error "convertLit"

convertCon :: DataCon -> [StgArg] -> Object
convertCon con args = Con (dataConToVar con) (convertArgs args)

convertFunc :: [Var] -> StgExpr -> Object
convertFunc vars body = Fun (varsToVars vars) (convertStgExpr' body)

convertArgs :: [StgArg] -> [Atom]
convertArgs = map convertArg

convertArg :: StgArg -> Atom
convertArg (StgVarArg v) = Var (idToVar v)
convertArg (StgLitArg l) = Lit (convertLit l)

idToVar :: Id -> Variable
idToVar id = case nameToVar (isGlobalId id) (idName id) of
  (c:cs) -> (toLower c : cs)
  v -> v

nameToVar :: Bool -> Name -> Variable
nameToVar glob nm = encodeVar $ getOccString nm ++ pref ++ uniq
  where
  uniq = if glob
    then ""
    else "_" ++ show (nameUnique nm)
  pref = case nameModule_maybe nm of
    Nothing -> ""
    Just m -> "." ++ moduleNameString (moduleName m)

dataConToVar :: DataCon -> Variable
dataConToVar = nameToVar True . dataConName

varsToVars :: [Var] -> [Variable]
varsToVars = map varToVar

varToVar :: Var -> Variable
varToVar var = nameToVar (isGlobalId var) (Var.varName var)

encodeVar :: String -> String
encodeVar = go []
  where
  go res [] = reverse res
  go res ('z':ss) = go ('0':'z':res) ss
  go res ('.':ss) = go ('1':'z':res) ss
  go res ('#':ss) = go ('2':'z':res) ss
  go res ('$':ss) = go ('3':'z':res) ss
  go res ('*':ss) = go ('4':'z':res) ss
  go res (':':ss) = go ('5':'z':res) ss
  go res ('\'':ss) = go ('6':'z':res) ss
  go res (s:ss) = go (s:res) ss
