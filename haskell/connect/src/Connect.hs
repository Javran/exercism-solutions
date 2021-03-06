{-# LANGUAGE MultiWayIf #-}

module Connect
  ( winner
  , Mark (..)
  )
where

import Data.Array -- TODO: probably do a Vector version of this?
import qualified Data.Set as S

{-
  first of all we assume there should at most be one winner
  because paths got to have intersections so it will be impossible
  to connect 2 opposite sides at the same time.

  the gameboard is different from other games because one cell might
  might have up to 6 neighborhoods. for example, consider the following board:

  . O . X .
   . X X O .
    O O ? X .
     . X O X O
      X O O O X

  cell '?' has 6 neighboring cells.

  we would like to implement a search algorthm, visiting from one side
  and spreading to find out if the oppsite side is reachable.
  then we perform this search twice: one searches from top to bottom
  and another searches from left to right. if any of these two searches
  have reached its opposite side, we know immediately who is the winner
  otherwise there is no winner.

-}

data Mark = Cross | Nought deriving (Eq, Show)

type Coord = (Int, Int)

type GameBoard = Array Coord (Maybe Mark)

-- | calculate neighbor coordinates, note that bounds are ignored
--   so there might be invalid coordinates in results
neighborCoords :: Coord -> [Coord]
neighborCoords (x, y) =
  [ (x, y -1)
  , (x + 1, y -1)
  , (x -1, y)
  , (x + 1, y)
  , (x -1, y + 1)
  , (x, y + 1)
  ]

-- INVARIANT: coords in todoSet must be vaild (within range, color matches)
search :: GameBoard -> Mark -> S.Set Coord -> S.Set Coord -> S.Set Coord
search board color todoSet visitedSet
  | S.null todoSet = visitedSet
  | otherwise =
    let newVisitedSet = visitedSet `S.union` todoSet
        expandedSet =
          S.filter ((== Just color) . (board !))
            . S.filter (inRange (bounds board))
            . foldMap (S.fromList . neighborCoords)
            $ todoSet
        freshCoords = expandedSet `S.difference` newVisitedSet
     in search board color freshCoords newVisitedSet

-- | convert raw data to game board representation
toGameBoard :: [String] -> GameBoard
toGameBoard raw = array ((1, 1), (cols, rows)) (zip coords (map toMark $ concat raw))
  where
    coords = [(x, y) | y <- [1 .. rows], x <- [1 .. cols]]
    cols = length (head raw)
    rows = length raw
    toMark '.' = Nothing
    toMark 'O' = Just Nought
    toMark 'X' = Just Cross
    toMark _ = error "invalid game board"

-- | calculate coordiates gameboard sides for each color, cell value is not yet
--   taken into account
sideCoordsOf :: GameBoard -> Mark -> (S.Set Coord, S.Set Coord)
sideCoordsOf board color = case color of
  Nought -> (tops, bottoms)
  Cross -> (lefts, rights)
  where
    ((1, 1), (cols, rows)) = bounds board
    tops = S.fromList [(x, 1) | x <- [1 .. cols]]
    bottoms = S.fromList [(x, rows) | x <- [1 .. cols]]
    lefts = S.fromList [(1, y) | y <- [1 .. rows]]
    rights = S.fromList [(cols, y) | y <- [1 .. rows]]

winner :: [String] -> Maybe Mark
winner raw =
  if
      | not (S.null blackResult) -> Just Cross
      | not (S.null whiteResult) -> Just Nought
      | otherwise -> Nothing
  where
    board = toGameBoard $ fmap (filter (/= ' ')) raw
    (blackBegins, blackEnds) = sideCoordsOf board Cross
    (whiteBegins, whiteEnds) = sideCoordsOf board Nought

    -- search Cross side: begin with the set of valid left side cells (within bound &
    -- the cell itself is black), we expand the set of reachable coordinates that contains
    -- Cross, and see if we can reach the other side
    blackValidBegins = S.filter (\c -> board ! c == Just Cross) blackBegins
    blackResult = search board Cross blackValidBegins S.empty `S.intersection` blackEnds

    -- search Nought side again, similar to how we search for Cross color
    whiteValidBegins = S.filter (\c -> board ! c == Just Nought) whiteBegins
    whiteResult = search board Nought whiteValidBegins S.empty `S.intersection` whiteEnds
