{-# LANGUAGE TemplateHaskell #-}
import Bin
import BinTH
import Language.Haskell.TH

data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a)
data Unit a = Unit a

data Phantom a = Phantom Int

deriveBin ''Tree
deriveBin ''Unit
