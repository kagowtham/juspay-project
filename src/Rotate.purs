module Rotate where

import  Prelude
import Math

rotateX angle x y z = do
  let rad = angle * pi / 100.0
  let cosa = cos rad
  let sina = sin rad
  let yy = y * cosa - z * sina
  let zz = y * sina + z * cosa
  [x,yy,zz]

rotateY angle x y z = do
  let rad = angle * pi / 100.0
  let cosa = cos rad
  let sina = sin rad
  let xx = z * sina + x * cosa
  let zz = z * cosa - x * sina
  [xx,y,zz]
