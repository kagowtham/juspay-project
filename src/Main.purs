module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Data.Maybe
import Math
import Global.Unsafe
import Data.Int
import Data.Traversable (for)

import Data.Array
import Data.Array.ST
import Graphics.Canvas
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Console
import Data.Foldable (for_)
import Control.Monad.Except (runExcept)
import Data.Foreign (readString)
import Control.Monad.Eff.Ref
import DOM.HTML (window)
import DOM.HTML.Types (Window())
import DOM (DOM())
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import DOM.RequestAnimationFrame
import Element as J
import Mouse
import Rotate
import Others



main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  body <- J.body

  canvas_jq <- J.getElementById "canvas"

  _ <- setFillStyle "rgb(120, 140, 21)" ctx
  drag <- newSTRef false
  old_x <- newSTRef 0.0
  old_y <- newSTRef 0.0
  dx <- newSTRef 0.0
  dy <- newSTRef 0.0
  dz <- newSTRef 0.0
  x_angle <- newSTRef 0.0
  y_angle <- newSTRef 0.0
  let sz = 50.0
  let msz = -50.0
  let vertices = [
        [msz, sz, msz],
        [sz, sz, msz],
        [sz, msz, msz],
        [msz, msz, msz],
        [msz, sz, sz],
        [sz, sz, sz],
        [sz, msz, sz],
        [msz, msz, sz]
    ]
  let faces = [[0,1,2,3],[1,5,6,2],[5,4,7,6],[4,0,3,7],[0,4,5,1],[3,2,6,7]]

  st_vertices <- emptySTArray

  void $ forE 0 8 $ \i ->  do
   let xx = (vertices !! i)
   let yy = fromMaybe [] xx
   void $ pushSTArray st_vertices yy

  let updateCube = do
        t <- emptySTArray
        ddrag <- readSTRef drag
        dx_val <- readSTRef dx
        dy_val <- readSTRef dy
        if ddrag == false
          then do
            _ <- writeSTRef dx (multiply dx_val deceleration)
            _ <- writeSTRef dy (multiply dy_val deceleration)
            dxx <- readSTRef dx
            dyy <- readSTRef dy
            x_a <- readSTRef x_angle
            y_a <- readSTRef y_angle
            _ <- writeSTRef x_angle (oadd x_a dxx)
            xangle <- readSTRef x_angle
            _ <- writeSTRef y_angle (add y_a dyy)
            r <- emptySTArray
            void $ pushSTArray r 1
          else do
            r <- emptySTArray
            void $ pushSTArray r 1
        void $ forE 0 8 $ \i ->  do
          mvi <- peekSTArray st_vertices i
          let vi = fromMaybe [] mvi
          let mx = vi !! 0
          let my = vi !! 1
          let mz = vi !! 2
          let x = fromMaybe 0.0 mx
          let y = fromMaybe 0.0 my
          let z = fromMaybe 0.0 mz
          yangle <- readSTRef y_angle
          xangle <- readSTRef x_angle
          let v = rotateX (yangle*50.0) x y z
          let mvx = v !! 0
          let mvy = v !! 1
          let mvz = v !! 2
          let vx = fromMaybe 0.0 mvx
          let vy = fromMaybe 0.0 mvy
          let vz = fromMaybe 0.0 mvz
          let vv = rotateY (50.0*xangle) vx vy vz
          void $ pushSTArray t vv
          r <- emptySTArray
          void $ pushSTArray r 1
        drawCube t faces ctx
        requestAnimationFrame updateCube

  drawCube st_vertices faces ctx
  J.on "mousedown" (mouseDown canvas drag old_x old_y) canvas_jq
  J.on "mouseup" (mouseUp canvas drag) body
  J.on "mousemove" (mouseMove canvas drag old_x old_y dx dy x_angle y_angle) canvas_jq
  J.on "mouseout" (mouseMove canvas drag old_x old_y dx dy x_angle y_angle) canvas_jq
  updateCube
