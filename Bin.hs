{-# LANGUAGE TemplateHaskell #-}
module Bin where
import Data.List

data Bit = O | I deriving (Show, Read, Eq, Ord)

class Bin a where
  encode :: a -> [Bit]
  decode :: [Bit] -> (a, [Bit])

instance Bin Int where
  encode i = let sign   = if i < 0 then I else O
                 i'     = abs i
                 binary = toBits i'
             in take (bitCount + 1) (sign:binary ++ replicate bitCount O)
  decode a = let (s:as, bs) = splitAt (1+bitCount) a
                 sign = if s == O then 1 else -1
             in (sign*fromBits as, bs)

instance Bin Bool where
  encode False  = [O]
  encode True   = [I]
  decode (O:xs) = (False, xs)
  decode (I:xs) = (True,  xs)

instance Bin a => Bin [a] where
  encode []     = [I]
  encode (a:as) = O : encode a ++ encode as

  decode (I:xs)    = ([], xs)
  decode (O:xs)    = let (a,  xs')  = decode xs
                         (as, xs'') = decode xs'
                     in (a:as, xs'')

instance Bin Bit where
  encode = return
  decode (x:xs) = (x, xs)

unfoldrStep :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> Maybe (a, b)
unfoldrStep p f g x | p x       = Nothing
                    | otherwise = Just (f x, g x)

bitCount = ceiling $ logBase 2 $ fromIntegral (maxBound::Int)
toBits   = map readBits . unfoldr (unfoldrStep (==0) (`mod`2) (`div`2))
  where
    readBits 0 = O
    readBits _ = I

fromBits = foldr (\a b -> a+2*b) 0 . map fromBits
  where
    fromBits O = 0
    fromBits I = 1
