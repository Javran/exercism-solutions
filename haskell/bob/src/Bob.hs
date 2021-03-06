{-# LANGUAGE ViewPatterns #-}

module Bob
  ( responseFor
  )
where

import Control.Arrow
import Data.Char
import Data.List

-- | have question mark at the end
isQuestion :: String -> Bool
isQuestion = ("?" `isSuffixOf`)

-- | contain some alpha, and all alphas are uppers
isYell :: String -> Bool
isYell =
  (any isAlpha
     --  at least contains some alphas
     &&& (filter isAlpha >>> all isUpper))
    -- all alphas being uppers
    >>> uncurry (&&)

-- both conds are required

responseFor :: String -> String
responseFor (dropWhileEnd isSpace -> s)
  | all isSpace s = "Fine. Be that way!"
  | isYell s =
    if isQuestion s
      then "Calm down, I know what I'm doing!"
      else "Whoa, chill out!"
  | isQuestion s = "Sure."
  | otherwise = "Whatever."
