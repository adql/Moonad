module Eval.Types
  ( Register(..)
  , MooState(..)
  , EvalCow
  )
where

import Control.Monad.Reader
import Control.Monad.State

import Moo

data Register = Null | Val Int deriving Show

data MooState = MooState
  { mooEvalPos :: Int
  , mooMemory :: [Int]
  , mooMemPos :: Int
  , mooRegister :: Register
  } deriving Show

type EvalCow a  = ReaderT [COWExpression] (StateT MooState IO) a
