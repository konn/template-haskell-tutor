{-# LANGUAGE TemplateHaskell #-}
module BinTH where
import Bin
import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import TupTH
import Data.List

deriveBin :: Name -> Q [Dec]
deriveBin dName = do
  DataD cxts name vars cons _ <- normalizeInfo <$> reify dName
  (cs, pats, exps) <- unzip3 <$> mapM implForCon cons
  let cxts = return $ map (ClassP ''Bin . pure) $ nub $ concat cs
      funs = genFuns pats exps
  return <$> instanceD cxts (appT (conT ''Bin) (appsT $ conT name : map (varT . tvName) vars))
                       [funD 'encode funs]

genFuns :: [PatQ] -> [ExpQ] -> [ClauseQ]
genFuns []     []     = [ ]
genFuns [p]    [e]    = [ clause [p] (normalB e) []]
genFuns (p:ps) (e:es) =
  clause [p] (normalB $ [| O : $(e) |]) [] : map modifyBody (genFuns ps es)
  where
    modifyBody :: ClauseQ -> ClauseQ
    modifyBody cq = do
      Clause pat (NormalB e) [] <- cq
      clause (map return pat) (normalB [| I : $(return e) |]) []

appsT :: [TypeQ] -> TypeQ
appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT ( (appT x y) : zs )
      

implForCon :: Con -> Q ([Type], PatQ, ExpQ)
implForCon con = do
  let (name, typs) = conTypesAndName con
  vars <- replicateM (length typs) $ newName "x"
  let pats = map varP vars
      exps = map varE vars
      expr = [| concat $(listE $ map (appE [| encode |]) exps) |]
  return (filter isVarType typs, conP name pats, expr)

isVarType :: Type -> Bool
isVarType (VarT _) = True
isVarType _        = False

conTypesAndName :: Con -> (Name, [Type])
conTypesAndName (NormalC name styps)  = (name, map snd styps)
conTypesAndName (RecC name vstyps)    = (name, map $(sel 3 3) vstyps)
conTypesAndName (InfixC st1 name st2) = (name, [snd st1, snd st2])
conTypesAndName (ForallC _ _ _)       = error "data constructor with forall is not supported yet."

hasVars :: Type -> Bool
hasVars = not . null . typeVars

conVars :: Con -> [Name]
conVars = conVars' []

conVars' :: [Name] -> Con -> [Name]
conVars' excs (NormalC _ typs)   = concatMap (typeVars' excs . snd) typs
conVars' excs (RecC _ vstys)     = concatMap (typeVars' excs . $(sel 3 3)) vstys
conVars' excs (InfixC st1 _ st2) = typeVars' excs (snd st1) ++ typeVars' excs (snd st2)
conVars' excs (ForallC tvbs _ c) = conVars' (excs ++ map tvName tvbs) c

typeVars :: Type -> [Name]
typeVars = typeVars' []

tvName :: TyVarBndr -> Name
tvName (PlainTV n) = n
tvName (KindedTV n _) = n


typeVars' :: [Name] -> Type -> [Name]
typeVars' excs (VarT v) | v `notElem` excs = []
                   | otherwise             = [v]
typeVars' excs (AppT t1 t2)                = typeVars' excs t1 ++ typeVars' excs t2
typeVars' excs (SigT t _)                  = typeVars' excs t
typeVars' excs (ForallT tvbs cxt t)        = typeVars' (excs ++ map tvName tvbs) t
typeVars' excs _                           = []

normalizeInfo :: Info -> Dec
normalizeInfo (TyConI datDec) =
  case datDec of
    d@(DataD _ _ _ _ _) -> d
    NewtypeD cxt name vars con derivs -> DataD cxt name vars [con] derivs
normalizeInfo _               = error "please pass type name"

