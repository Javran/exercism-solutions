{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds #-}

module Lib
  ( main
  )
where

import Control.Applicative
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Foldl as Fold
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS hiding (null)
import System.Environment
import System.Exit hiding (die)
import qualified System.IO
import qualified System.Process as Process
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)

{-
  Following environment variables are required for some of those scripts to work:

  - EXERCISM_WORKSPACE
  - TARGET_RESOLVER

  example setup in a shell script:

```
export EXERCISM_WORKSPACE=$(exercism workspace)
export TARGET_RESOLVER=lts-16.31
```

 -}

fpToText :: FilePath -> T.Text
fpToText = either id id . Filesystem.Path.CurrentOS.toText -- TEST

getExercismWorkspace :: (MonadIO m, MonadFail m) => m FilePath
getExercismWorkspace = do
  Just fp <- fmap fromText <$> need "EXERCISM_WORKSPACE"
  pure fp

getHaskellRepo :: (MonadIO m, MonadFail m) => m FilePath
getHaskellRepo = (</> "haskell") <$> getExercismWorkspace

ltsUpdater :: IO ()
ltsUpdater = sh $ do
  repo <- getHaskellRepo
  Just targetResolver <- need "TARGET_RESOLVER"
  xs <- reduce Fold.list $ do
    stackYaml <- find (suffix "stack.yaml") repo
    let replacer = do
          r <- "resolver: "
          (r <> targetResolver) <$ (many anyChar >> eof)
    inplace replacer stackYaml
  liftIO $ putStrLn $ show (length xs) <> " files visited."

runAllTests :: IO ()
runAllTests = sh $ do
  let skipping =
        -- set of exercise names that we'll skip.
        S.fromList []
  repo <- getHaskellRepo
  exers <-
    catMaybes
      <$> reduce
        Fold.list
        (do
           exerPath <- ls repo
           let exerName = fpToText $ filename exerPath
               isExerciseName = not ("." `T.isPrefixOf` exerName)
           if isExerciseName && S.notMember exerName skipping
             then pure $ Just (exerName, exerPath)
             else pure Nothing)
  liftIO $ putStrLn $ "Found " <> show (length exers) <> " exercises."
  let testExercise (exerName, exerPath) sem = do
        liftIO $ MSem.wait sem
        let mkCp cmd args =
              (Process.proc cmd args)
                { Process.cwd = Just (encodeString exerPath)
                }
        -- ignore formatting result even if it fails.
        _ <- systemStrictWithErr (mkCp "ew" ["fmt"]) ""
        result@(ec, _, _) <- systemStrictWithErr (mkCp "ew" ["test"]) ""
        liftIO $ MSem.signal sem
        liftIO $ do
          if ec == ExitSuccess
            then putStr "."
            else do
              putStrLn $ "\nFailed: " <> T.unpack exerName
              putStr "!"
          System.IO.hFlush System.IO.stdout
        pure (exerName, result)
  sem <- liftIO $ MSem.new (10 :: Int)
  tasks <- forM exers $ \p ->
    fork (testExercise p sem)
  results <- mapM wait tasks
  liftIO $ putStrLn "\nall done."
  let failedResults = filter (\(_, (ec, _, _)) -> ec /= ExitSuccess) results
  liftIO $
    unless (null failedResults) $ do
      putStrLn $ "Found " <> show (length failedResults) <> " failed results, in following exercises:"
      forM_ failedResults $ \(exerName, (ec, _, _)) -> do
        putStrLn $ T.unpack exerName <> ": " <> show ec

main :: IO ()
main =
  getArgs >>= \case
    "ltsupdate" : _ -> ltsUpdater
    "testall" : _ -> runAllTests
    xs -> do
      putStrLn $ "Unknown: " <> show xs
      exitFailure
