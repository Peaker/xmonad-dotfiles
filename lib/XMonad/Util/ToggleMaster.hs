{-# OPTIONS -Wall -O2 #-}

module XMonad.Util.ToggleMaster(newToggleMasterContext
                               ,toggleMasterFocus
                               ,toggleSwapMasterFocus) where

import XMonad.StackSet(StackSet, Stack(Stack), swapMaster, focusMaster, modify')
import XMonad.StackSet(stack, workspace, current) -- for with
import XMonad.Operations(windows)
import XMonad.Core(io, WindowSet, withWindowSet, X)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)

with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current

withStack :: Monad m => (Stack a -> m ()) -> StackSet i l a s sd -> m ()
withStack = with (return ())

type ToggleMasterContext = IORef (Maybe Int)

newToggleMasterContext :: IO ToggleMasterContext
newToggleMasterContext = newIORef Nothing

toggleMasterFocus :: ToggleMasterContext -> X ()
toggleMasterFocus = swapRemember focusMaster focusTo

toggleSwapMasterFocus :: ToggleMasterContext -> X ()
toggleSwapMasterFocus = swapRemember swapMaster swapBack

swapBack :: Int -> WindowSet -> WindowSet
swapBack x = focusTo x . swapMaster . focusTo x

swapRemember :: (WindowSet -> WindowSet) ->
                (Int -> WindowSet -> WindowSet) ->
                IORef (Maybe Int) -> X ()
swapRemember toMaster andBack r = withWindowSet (withStack useStack)
  where
    useStack (Stack _ ls _) = do
      w <- if null ls
           then io $ readIORef r
           else return Nothing
      case w of
        Nothing -> do
          io $ writeIORef r (Just (length ls))
          windows toMaster
        Just x -> do
          io $ writeIORef r Nothing
          windows . andBack $ x

focusTo :: Int -> StackSet i l a s sd -> StackSet i l a s sd
focusTo = modify' . focusTo'

focusTo' :: Int -> Stack a -> Stack a
focusTo' x (Stack t [] rs) = Stack t' (reverse before) after
  where (before, t':after) = splitAt x (t:rs)
focusTo' _ c               = c
