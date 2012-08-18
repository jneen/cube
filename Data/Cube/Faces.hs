module Data.Cube.Faces (
    Face(L, R, F, B, U, D),
    oppositeFace,
    faceRing,
    rotateFace,
    initColor
  ) where

  import Data.Cube.Color

  data Face = L | R | F | B | U | D deriving(Show, Eq)

  oppositeFace :: Face -> Face
  oppositeFace = undefined

  faceRing :: Face -> [Face]
  faceRing = undefined

  rotateFace :: Face -> Face -> Face
  rotateFace = undefined

  initColor :: Face -> Color
  initColor U = White
  initColor D = Yellow
  initColor F = Green
  initColor B = Blue
  initColor R = Red
  initColor L = Orange
