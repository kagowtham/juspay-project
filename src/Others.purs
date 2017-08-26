module Others where

import Prelude
import Data.Array
import Data.Array.ST
import Graphics.Canvas
import Control.Monad.Eff
import Data.Maybe
import Math

qx = pi/4.0
qy = pi/3.0
qz = pi/4.0

multiply a b = a * b

oadd a b = a + b

stroke v00 v01 v10 v11 v20 v21 v30 v31 ctx = do
  r <- emptySTArray
  _ <- strokePath ctx $ do
    _ <- moveTo ctx v00 v01
    _ <- lineTo ctx v10 v11
    _ <- lineTo ctx v20 v21
    _ <- lineTo ctx v30 v31
    _ <- closePath ctx
    void $ pushSTArray r 1
  void $ pushSTArray r 1

project x y z = do
    let xRotQz = x*cos(qz)+y*sin(qz)
    let yRotQz = y*cos(qz)-x*sin(qz)
    let zRotQz = z
    let xRotQzQx = xRotQz
    let yRotQzQx = yRotQz*cos(qx)+zRotQz*sin(qx)
    let zRotQzQx = zRotQz*cos(qx)-yRotQz*sin(qx)
    let xRotQzQxQy = xRotQzQx*cos(qy)+zRotQzQx*sin(qy)
    let yRotQzQxQy = yRotQzQx
    [xRotQzQxQy, yRotQzQxQy] :: Array Number



drawCube vertices faces ctx = do
    _ <- clearRect ctx { x: 0.0, y: 0.0, w: 650.0, h: 650.0 }
    verticesPixLoc <- emptySTArray
    void $ forE 0 8 $ \i ->  do
      xx <- peekSTArray vertices i
      let yy = fromMaybe [] xx
      let zz = (yy !! 0)
      let aa = fromMaybe 0.0 zz
      let zz = (yy !! 1)
      let bb = fromMaybe 0.0 zz
      let zz = (yy !! 2)
      let cc = fromMaybe 0.0 zz
      let xyLoc = project aa bb cc :: Array Number
      let m_xy0 = xyLoc !! 0
      let m_xy1 = xyLoc !! 1
      let xy0 = fromMaybe 0.0 m_xy0
      let xy1 = fromMaybe 0.0 m_xy1
      let pix0 = xy0 + 650.0/2.0
      let pix1 = -1.0*xy1 + 400.0/2.0

      void $ pushSTArray verticesPixLoc [pix0,pix1]

    _ <- setStrokeStyle "rgb(66, 134, 244)" ctx
    void $ forE 0 6 $ \i ->  do

      let m_i_face = faces !! i
      let i_face = fromMaybe [] m_i_face
      let mf0 = i_face !! 0
      let mf1 = i_face !! 1
      let mf2 = i_face !! 2
      let mf3 = i_face !! 3
      let f0 = fromMaybe 0 mf0
      let f1 = fromMaybe 0 mf1
      let f2 = fromMaybe 0 mf2
      let f3 = fromMaybe 0 mf3
      mv0 <- peekSTArray verticesPixLoc f0
      mv1 <- peekSTArray verticesPixLoc f1
      mv2 <- peekSTArray verticesPixLoc f2
      mv3 <- peekSTArray verticesPixLoc f3
      let v0 = fromMaybe [] mv0
      let v1 = fromMaybe [] mv1
      let v2 = fromMaybe [] mv2
      let v3 = fromMaybe [] mv3
      let mv00 = v0 !! 0
      let mv01 = v0 !! 1
      let mv10 = v1 !! 0
      let mv11 = v1 !! 1
      let mv20 = v2 !! 0
      let mv21 = v2 !! 1
      let mv30 = v3 !! 0
      let mv31 = v3 !! 1
      let v00 = fromMaybe 0.0 mv00
      let v01 = fromMaybe 0.0 mv01
      let v10 = fromMaybe 0.0 mv10
      let v11 = fromMaybe 0.0 mv11
      let v20 = fromMaybe 0.0 mv20
      let v21 = fromMaybe 0.0 mv21
      let v30 = fromMaybe 0.0 mv30
      let v31 = fromMaybe 0.0 mv31
      stroke v00 v01 v10 v11 v20 v21 v30 v31 ctx
      r <- emptySTArray
      void $ pushSTArray r 1
deceleration = 0.92
