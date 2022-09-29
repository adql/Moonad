module Eval
  ( runCow
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Either (fromRight)
import Data.List (elemIndices, elemIndex)
import Text.Parsec

import Eval.Types
import Moo

initialMooState :: MooState
initialMooState = MooState 0 [0] 0 Null

evalMoo :: EvalCow Bool
evalMoo = do
  evalPos <- gets mooEvalPos
  expr <- asks (!!evalPos)
  evalResult <- eval expr
  case evalResult of
    Nothing -> return False
    Just newEvalPos -> do
      exprs <- ask
      if newEvalPos >= length exprs
        then return $ False
        else do
        modify $ \s -> s { mooEvalPos = newEvalPos }
        return True

runCow :: [COWExpression] -> IO MooState
runCow = go initialMooState
  where
    go s cow = do
      iter <- runStateT (runReaderT evalMoo cow) s
      case iter of
        (True, s') -> go s' cow
        (False, s') -> return s'

eval :: COWExpression -> EvalCow (Maybe Int)

-- 0 -- moo
eval COWGoBack = do
  evalPos <- gets mooEvalPos
  exprs <- take (evalPos - 1) <$> ask
  let matches = elemIndices COWGoForward exprs
  return $ Just $ case matches of
    [] -> 0
    _  -> last matches

-- 1 -- mOo
eval COWMemBack = do
  let move pos = if pos == 0 then pos else pos - 1
  modify $ \s -> s { mooMemPos = move $ mooMemPos s }
  returnIncEvalPos

-- 2 -- moO
eval COWMemForward = do
  memory <- gets mooMemory
  newPos <- (+ 1) <$> gets mooMemPos
  let extendedMem = if newPos == length memory
                  then memory ++ [0]
                  else memory
  modify $ \s -> s { mooMemory = extendedMem
                   , mooMemPos = newPos
                   }
  returnIncEvalPos

-- 3 -- mOO
eval COWMemExec = do
  pos <- gets mooMemPos
  val <- (!!pos) <$> gets mooMemory
  if val < 0 || val == 3 || val > 11
    then return Nothing
    else eval $ toEnum val

-- 4 -- Moo
eval COWStdAscii = do
  pos <- gets mooMemPos
  val <- (!!pos) <$> gets mooMemory
  val' <- case val of
            0 -> do
              c <- liftIO $ getChar
              return $ fromEnum c
            _ -> do
              liftIO $ putChar $ toEnum val
              return val
  modifyMemoryAtPos $ \_ -> val'

-- 5 -- MOo
eval COWMemDec = modifyMemoryAtPos $ \x -> x - 1

-- 6 -- MoO
eval COWMemInc = modifyMemoryAtPos (+ 1)

-- 7 -- MOO
eval COWGoForward = do
  pos <- gets mooMemPos
  val <- (!!pos) <$> gets mooMemory
  case val of
    0 -> do
      evalPos <- gets mooEvalPos
      exprs <- drop (evalPos + 1) <$> ask
      let match = elemIndex COWGoBack exprs
      return $ (+ (evalPos + 2)) <$> match
    _ -> returnIncEvalPos

-- 8 -- OOO
eval COWMemReset = modifyMemoryAtPos $ \_ -> 0

-- 9 -- MMM
eval COWRegister = do
  register <- gets mooRegister
  case register of
    Null -> do
      pos <- gets mooMemPos
      val <- (!!pos) <$> gets mooMemory
      modify $ \s -> s { mooRegister = Val val }
      returnIncEvalPos
    Val val -> do
      modify $ \s -> s { mooRegister = Null }
      modifyMemoryAtPos $ \_ -> val

-- 10 -- OOM
eval COWStdPrintInt = do
  pos <- gets mooMemPos
  val <- (!!pos) <$> gets mooMemory
  liftIO $ print val
  returnIncEvalPos

-- 11 -- oom
eval COWStdReadInt = do
  str <- liftIO getLine
  let int = fromRight 0 $ parse intParser "" str
  modifyMemoryAtPos $ \_ -> int

returnIncEvalPos :: EvalCow (Maybe Int)
returnIncEvalPos = (Just . (+ 1)) <$> gets mooEvalPos

modifyMemoryAtPos :: (Int -> Int) -> EvalCow (Maybe Int)
modifyMemoryAtPos f = do
  pos <- gets mooMemPos
  mem <- gets mooMemory
  case modifyMemoryAt pos f mem of
    Just newMem -> do
      modify $ \s -> s { mooMemory = newMem }
      returnIncEvalPos
    Nothing -> return Nothing

modifyMemoryAt :: Int -> (Int -> Int) -> [Int] -> Maybe [Int]
modifyMemoryAt pos f mem =
  case splitAt pos mem of
    (xs, x:xs') -> Just $ xs ++ (f x) : xs'
    _ -> Nothing

intParser :: Parsec String () Int
intParser = many digit <* many anyChar >>= \digits ->
  case digits of
    "" -> return 0
    _  -> return $ read digits
