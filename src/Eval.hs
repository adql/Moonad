module Eval
  ( runCow
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Either (fromRight)
import Text.Parsec

import Eval.Types
import Moo

initialMooState :: MooState
initialMooState = MooState 0 [0] 0 RegNull [] []

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

runCow :: COW -> IO MooState
runCow = go initialMooState
  where
    go s cow = do
      iter <- runStateT (runReaderT evalMoo cow) s
      case iter of
        (True, s') -> go s' cow
        (False, s') -> return s'

eval :: COWExpression -> EvalCow (Maybe MooEvalPos)

-- 0 -- moo
eval COWGoBack = do
  evalPos <- gets mooEvalPos
  back <- gets mooBack
  let savedMatch = lookup evalPos back
  case savedMatch of
    Just _ -> return savedMatch
    Nothing -> do
      exprs <- take (evalPos - 1) <$> ask
      case findLoop exprs True of
        Nothing -> return Nothing
        Just match -> do
          modify $ \s -> s { mooBack = (evalPos,match) : back }
          return $ Just match

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
      forward <- gets mooForward
      let savedMatch = lookup evalPos forward
      case savedMatch of
        Just _ -> return savedMatch
        Nothing -> do
          exprs <- drop (evalPos + 2) <$> ask
          case findLoop exprs False of
            Nothing -> return Nothing
            Just match' -> do
              let match = match' + evalPos + 3
              modify $ \s -> s { mooForward = (evalPos,match) : forward }
              return $ Just match
    _ -> returnIncEvalPos

-- 8 -- OOO
eval COWMemReset = modifyMemoryAtPos $ \_ -> 0

-- 9 -- MMM
eval COWRegister = do
  register <- gets mooRegister
  case register of
    RegNull -> do
      pos <- gets mooMemPos
      val <- (!!pos) <$> gets mooMemory
      modify $ \s -> s { mooRegister = RegVal val }
      returnIncEvalPos
    RegVal val -> do
      modify $ \s -> s { mooRegister = RegNull }
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

returnIncEvalPos :: EvalCow (Maybe MooEvalPos)
returnIncEvalPos = (Just . (+ 1)) <$> gets mooEvalPos

modifyMemoryAtPos :: (Int -> Int) -> EvalCow (Maybe MooEvalPos)
modifyMemoryAtPos f = do
  pos <- gets mooMemPos
  mem <- gets mooMemory
  case modifyMemoryAt pos f mem of
    Just newMem -> do
      modify $ \s -> s { mooMemory = newMem }
      returnIncEvalPos
    Nothing -> return Nothing

modifyMemoryAt :: MooMemPos -> (Int -> Int) -> MooMemory -> Maybe MooMemory
modifyMemoryAt pos f mem =
  case splitAt pos mem of
    (xs, x:xs') -> Just $ xs ++ (f x) : xs'
    _ -> Nothing

intParser :: Parsec String () Int
intParser = many digit <* many anyChar >>= \digits ->
  case digits of
    "" -> return 0
    _  -> return $ read digits

findLoop :: COW -> Bool -> Maybe MooMemPos
findLoop exprs0 backwards =
  go 0 0 $ if backwards then reverse exprs0 else exprs0
  where
    go :: Int -> MooMemPos -> COW -> Maybe MooMemPos
    go _ _ [] = Nothing
    go nest pos (expr:exprs)
      | expr == COWGoForward =
        if backwards then matchOrNestOut else nestIn
      | expr == COWGoBack =
        if backwards then nestIn else matchOrNestOut
      | otherwise = go nest (pos + 1) exprs
      where
        nestIn = go (nest + 1) (pos + 1) exprs
        matchOrNestOut =
          if nest > 0 then go (nest - 1) (pos + 1) exprs
          else Just $ if backwards then (len - 1 - pos) else pos
    len = length exprs0
