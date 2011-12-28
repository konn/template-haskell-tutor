module TupTH where
import Language.Haskell.TH
import Control.Monad

sel :: Int -> Int -> ExpQ
sel count nth = do
  vars <- replicateM count $ newName "x"
  lamE [tupP $ map varP vars] $ varE $ vars !! (nth-1)

sel' :: Int -> Int -> ExpQ
sel' count nth | count >= nth = do
  var <- newName "x"
  let pats = replicate (nth - 1) wildP ++ [varP var] ++ replicate (count - nth) wildP
  lamE [conP (tupleDataName count) pats] (varE var)
