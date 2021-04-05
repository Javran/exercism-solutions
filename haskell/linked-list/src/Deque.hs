{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Deque
  ( mkDeque
  , push
  , pop
  , shift
  , unshift
  , visitNodes
  )
where

import Control.Monad.Fix
import Data.IORef
import Lens.Micro

-- INVARIANT: "Deque a" always point to a guard element
type Deque a = IORef (Element a)

type NodeRef a = IORef (Element a)

data Element a
  = Guard
      { _ePrev :: NodeRef a
      , _eNext :: NodeRef a
      }
  | Item
      { _ePrev :: NodeRef a
      , _eNext :: NodeRef a
      , _eContent :: a
      }

type RefLens a = Lens' (Element a) (NodeRef a)

ePrev, eNext :: RefLens a
ePrev f e = (\newPrev -> e {_ePrev = newPrev}) <$> f (_ePrev e)
eNext f e = (\newNext -> e {_eNext = newNext}) <$> f (_eNext e)

eContent :: SimpleFold (Element a) a
eContent =
  folding
    (\case
       Guard {} -> Nothing
       Item {_eContent = v} -> Just v)

mkDeque :: IO (Deque a)
mkDeque = mfix $ \r -> newIORef (Guard r r)

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref modify = atomicModifyIORef' ref ((,()) . modify)

dqInsert :: RefLens a -> RefLens a -> NodeRef a -> a -> IO ()
dqInsert dir revDir refCurrent v = do
  curNode <- readIORef refCurrent
  -- guard's next pointer points to the head
  let refNext = curNode ^. dir
      {-
        Due to the fact that `dir` and `revDir` are abstracted,
        we cannot initiate Item with _ePrev and _eNext set properly,
        this is fine since we'll use lens to immediate replace those
        two fields with the correct value.
       -}
      newNode =
        Item undefined undefined v
          & dir .~ refNext
          & revDir .~ refCurrent
  refNew <- newIORef newNode
  -- there are subtle issues regarding using "writeIORef":
  -- when refGuard and refNext is pointing to the same location,
  -- writeIORef will change its content and we have to retrieve
  -- the updated data (otherwise another writeIORef will overwrite
  -- our update)
  -- therefore here we choose to use "modifyIORef" in an atomic manner
  atomicModifyIORef_ refCurrent (& dir .~ refNew)
  atomicModifyIORef_ refNext (& revDir .~ refNew)

dqDelete :: RefLens a -> RefLens a -> NodeRef a -> IO (Maybe a)
dqDelete dir revDir refCurrent = do
  curNode <- readIORef refCurrent
  let refXNode = curNode ^. dir
  xNode <- readIORef refXNode
  let refNewNext = xNode ^. dir
  atomicModifyIORef_ refCurrent (& dir .~ refNewNext)
  atomicModifyIORef_ refNewNext (& revDir .~ refCurrent)
  pure (xNode ^? eContent)

unshift, push :: Deque a -> a -> IO ()
unshift = dqInsert eNext ePrev
push = dqInsert ePrev eNext

shift, pop :: Deque a -> IO (Maybe a)
shift dq = do
  -- for readability
  b <- nullDeque dq
  if b
    then pure Nothing
    else -- note that the meaning of "Maybe a" from dqDelete and that of pure type "Maybe a"
    -- from this function are different, but
    -- since at this point deque is not empty, the result of dqDelete
    -- happens to be the correct pure value of this function
    -- (same for "pop" function)
      dqDelete eNext ePrev dq
pop dq = do
  b <- nullDeque dq
  if b
    then pure Nothing
    else dqDelete ePrev eNext dq

-- a null deque contains nothing but the guard element
-- which means guard's next item is still guard itself
-- assume this deque is properly maintained, this check
-- should be sufficient
nullDeque :: Deque a -> IO Bool
nullDeque dq = readIORef dq >>= \node -> pure (_eNext node == dq)

-- | perform action on each non-guard nodes, debugging function
visitNodes :: (a -> IO b) -> Deque a -> IO [b]
visitNodes action refGuard = do
  headNode <- readIORef refGuard
  visitNodes' (_eNext headNode)
  where
    visitNodes' refCurrent =
      if refGuard == refCurrent
        then pure []
        else do
          node <- readIORef refCurrent
          result <- action (_eContent node)
          let refNext = _eNext node
          rs <- visitNodes' refNext
          pure (result : rs)
