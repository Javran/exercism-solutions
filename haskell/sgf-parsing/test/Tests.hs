{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (for_)
import Data.Map (fromList)
import Data.Tree (Tree (Node))
import Sgf (parseSgf)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "parseSgf" $ for_ cases test
  where
    test (input, expected) = it description assertion
      where
        description = unwords ["parse", show input]
        assertion = parseSgf input `shouldBe` fmap fromList <$> expected

    cases =
      [ ("", Nothing)
      , ("()", Nothing)
      , (";", Nothing)
      , ("(;)", Just $ Node [] [])
      , ("(;A[B])", Just $ Node [("A", ["B"])] [])
      , ("(;A[b]C[d])", Just $ Node [("A", ["b"]), ("C", ["d"])] [])
      , ("(;A)", Nothing)
      , ("(;a[b])", Nothing)
      , ("(;Aa[b])", Nothing)
      , ("(;A[B];B[C])", Just $ Node [("A", ["B"])] [Node [("B", ["C"])] []])
      , ( "(;A[B](;B[C])(;C[D]))"
        , Just $
            Node
              [("A", ["B"])]
              [ Node [("B", ["C"])] []
              , Node [("C", ["D"])] []
              ]
        )
      , ("(;A[b][c][d])", Just $ Node [("A", ["b", "c", "d"])] [])
      , ("(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])", Just $ Node [("A", ["]b cd  e\\ ]"])] [])
      ]

-- b74debc3be24b5c81650189935c9bbfa019b367e
