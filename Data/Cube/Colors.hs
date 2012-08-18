module Data.Cube.Colors (
    Color(
      White,
      Yellow,
      Red,
      Orange,
      Green,
      Blue
    ),
    colorToAnsi,
    printSwatch
  ) where

  import qualified System.Console.ANSI as ANSI


  data Color = White | Yellow | Red | Orange | Green | Blue
               deriving(Show)

  allColors = [White, Yellow, Red, Orange, Green, Blue]

  cubeColor = ANSI.SetColor ANSI.Background

  colorToAnsi :: Color -> ANSI.SGR
  colorToAnsi White  = cubeColor ANSI.Vivid ANSI.White
  colorToAnsi Yellow = cubeColor ANSI.Dull  ANSI.Yellow
  colorToAnsi Red    = cubeColor ANSI.Dull  ANSI.Red
  colorToAnsi Orange = cubeColor ANSI.Vivid ANSI.Red
  colorToAnsi Blue   = cubeColor ANSI.Dull  ANSI.Blue
  colorToAnsi Green  = cubeColor ANSI.Dull  ANSI.Green

  printSwatch :: Color -> IO ()
  printSwatch color = do
    ANSI.setSGR [ANSI.Reset, colorToAnsi color]
    putStr "  "
    ANSI.setSGR [ANSI.Reset]

  printSwatches :: IO ()
  printSwatches = mapM printSwatch allColors >> putChar '\n'

