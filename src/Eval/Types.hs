module Eval.Types
  ( MooEvalPos
  , MooMemory
  , MooMemPos
  , MooRegister(..)
  , MooLoopMemoizer
  , MooState(..)
  , EvalCow
  )
where

import Control.Monad.Reader
import Control.Monad.State

import Moo

type MooEvalPos = Int
type MooMemory = [Int]
type MooMemPos = Int
data MooRegister = RegNull | RegVal Int deriving Show
type MooLoopMemoizer = [(MooMemPos, MooMemPos)]

data MooState = MooState
  { mooEvalPos :: MooEvalPos
  , mooMemory :: MooMemory
  , mooMemPos :: MooMemPos
  , mooRegister :: MooRegister
  , mooForward :: MooLoopMemoizer
  , mooBack :: MooLoopMemoizer
  } deriving Show

type EvalCow a  = ReaderT COW (StateT MooState IO) a
