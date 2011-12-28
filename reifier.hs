{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Bin

$(do info <- reify 'curry
     runIO $ print info
     return [])
