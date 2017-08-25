module Mouse where

import Prelude
import Control.Monad.ST
import Math
import DOM
import Data.Array
import Data.Array.ST
import Element as J

mouseDown input drag oldx oldy e _ =  do
  pagex <- J.getPageX e
  pagey <- J.getPageY e
  _ <- writeSTRef drag true
  _ <- writeSTRef oldx pagex
  _ <- writeSTRef oldy pagey
  r <- emptySTArray
  void $ pushSTArray r 1

mouseUp input drag e _ =  do
   _ <- writeSTRef drag false
   r <- emptySTArray
   void $ pushSTArray r 1

mouseMove input drag oldx oldy dx dy xangle yangle e _ = void $ do
   drag_val <- readSTRef drag
   if drag_val == true
    then do
      pagex <- J.getPageX e
      pagey <- J.getPageY e
      oldx_val <- readSTRef oldx
      oldy_val <- readSTRef oldy
      _ <- writeSTRef dx (modify pagex oldx_val 600.0)
      _ <- writeSTRef dy (modify pagey oldy_val 600.0)
      dx_val <- readSTRef dx
      dy_val <- readSTRef dx
      x_val <- readSTRef xangle
      y_val <- readSTRef yangle

      _ <- writeSTRef xangle (add x_val dx_val)
      _ <- writeSTRef yangle (add y_val dy_val)
      _ <- writeSTRef oldx pagex
      _ <- writeSTRef oldy pagey
      r <- emptySTArray
      void $ pushSTArray r 1
    else do
      r <- emptySTArray
      void $ pushSTArray r 1


modify a b c = do
  ((a-b)*2.0*pi)/c
