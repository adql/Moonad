module Moo
  ( COWExpression(..)
  , moos
  )
  where

data COWExpression = COWGoBack
                   | COWMemBack
                   | COWMemForward
                   | COWMemExec
                   | COWStdAscii
                   | COWMemDec
                   | COWMemInc
                   | COWGoForward
                   | COWMemReset
                   | COWRegister
                   | COWStdPrintInt
                   | COWStdReadInt
                   deriving (Eq, Show, Enum)

moos :: [String]
moos = [ "moo"
       , "mOo"
       , "moO"
       , "mOO"
       , "Moo"
       , "MOo"
       , "MoO"
       , "MOO"
       , "OOO"
       , "MMM"
       , "OOM"
       , "oom"
       ]
 
