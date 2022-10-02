module Parser
  (cowParser)
where

import Text.Parsec
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import Moo

mooParser :: Parsec String () COWExpression
mooParser = choice mooParsers >>= return . toMooExpression
  where
    mooParsers = map (try . string) moos :: [Parsec String () String]
    toMooExpression = toEnum . fromJust . flip elemIndex moos

cowParser :: Parsec String () COW
cowParser = optional notMooParser *> mooParser `sepEndBy` notMooParser
  where
    notMooParser = manyTill anyChar (eof <|>
                                      (lookAhead mooParser *> return ()))
