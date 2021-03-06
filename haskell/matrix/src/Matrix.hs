{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Matrix
  ( Matrix
  , row
  , column
  , rows
  , cols
  , shape
  , transpose
  , reshape
  , flatten
  , fromString
  , fromList
  )
where

import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

type Matrix a = V.Vector (V.Vector a)

row, column :: Int -> Matrix a -> V.Vector a
row (pred -> i) = (V.! i)
column (pred -> i) = foldMap (V.singleton . (V.! i))

rows, cols :: Matrix a -> Int
rows = fst . shape
cols = snd . shape

shape :: Matrix a -> (Int, Int)
shape m = (rs, cs)
  where
    rs = V.length m
    cs = if rs == 0 then 0 else length (m V.! 0)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (rs, cs) m =
  V.fromList
    (take
       rs
       (map
          (\beginInd -> V.slice beginInd cs flattened)
          [0, cs ..]))
  where
    flattened = flatten m

transpose :: Matrix a -> Matrix a
transpose m =
  V.generate
    originCs
    (\cInd ->
       V.generate
         originRs
         (\rInd -> m V.! rInd V.! cInd))
  where
    (originRs, originCs) = shape m

flatten :: Matrix a -> V.Vector a
flatten = foldMap id

fromString :: Read a => String -> Matrix a
fromString raw = fromList (map parseLine rawRows)
  where
    rawRows = lines raw

fromList :: [[a]] -> Matrix a
fromList = V.fromList . map V.fromList

parseLine :: forall a. Read a => String -> [a]
parseLine raw = result
  where
    element :: ReadP a
    element = readS_to_P reads

    line = element `sepBy` skipSpaces
    ((result, "") : _) =
      readP_to_S
        (between
           skipSpaces
           skipSpaces
           line
           <* eof)
        raw
