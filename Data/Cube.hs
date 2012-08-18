module Data.Cube (
  -- colors
    Color(
      White,
      Yellow,
      Red,
      Orange,
      Green,
      Blue
    ),
    allColors,
    printSwatch,
    printSwatches,

  -- faces
    Face(L, R, F, B, U, D),
    allFaces,
    oppositeFace,
    faceRing,
    rotateFace,
    initColor,

  -- cubies
    Sticker(Sticker),
    Cubie(Edge, Corner),

  -- cube
    Cube(Cube),
    printCube,

  -- turns
    Turn(NullTurn, Forward, Reverse, Double, Multi),
    readTurn,
    showTurn,
    applyTurn,

  -- algs
    sexy,
    tperm,
    sune
  ) where

  import Data.Monoid
  import Data.List (intersperse, (\\), sort)
  -- import qualified Text.ParserCombinators.Parsec as Parsec
  import Text.ParserCombinators.Parsec

  {- COLORS -}
  data Color = White | Yellow | Red | Orange | Green | Blue
               deriving(Show, Eq)

  allColors = [White, Yellow, Red, Orange, Green, Blue]

  printSwatches :: [Color] -> IO ()
  printSwatches colors = do
    putStr clearSeq
    mapM printSingle colors
    putStr clearSeq
    where
      printSingle color = do
        putStr $ colorToEsc color
        putStr "  "

      clearSeq = "\ESC[0;38m"

      colorToEsc :: Color -> String
      colorToEsc color = case color of
        White  -> esc 15
        Yellow -> esc 190
        Red    -> esc 160
        Orange -> esc 202
        Blue   -> esc 21
        Green  -> esc 22
        where
          esc n = "\ESC[48;5;" ++ lpad '0' 3 (show n) ++ "m"
          lpad :: a -> Int -> [a] -> [a]
          lpad el len list = replicate (len - length list) el ++ list

  printSwatch :: Color -> IO ()
  printSwatch c = printSwatches [c]

  {- FACES -}
  data Face = L | R | F | B | U | D deriving(Show, Eq, Read, Ord)
  allFaces = [L, R, F, B, U, D]

  oppositeFace :: Face -> Face
  oppositeFace L = R
  oppositeFace R = L
  oppositeFace F = B
  oppositeFace B = F
  oppositeFace U = D
  oppositeFace D = U

  faceRing :: Face -> [Face]
  faceRing U = [F, L, B, R]
  faceRing R = [F, U, B, D]
  faceRing F = [U, R, D, L]
  faceRing f = reverse $ faceRing $ oppositeFace f

  nextFaceRing :: Face -> Face -> [Face]
  nextFaceRing direction face = take 4 nextCycle
    where
      nextCycle = tail $ dropWhile (/= face) $ cycle (faceRing direction)

  initColor :: Face -> Color
  initColor U = White
  initColor D = Yellow
  initColor F = Green
  initColor B = Blue
  initColor R = Red
  initColor L = Orange

  rotateFace :: Face -> Face -> Face
  rotateFace direction face
    -- noop if the rotating face is the same face or the opposite
    | direction == face                 = face
    | direction == oppositeFace face    = face
    | otherwise                         = head $ nextFaceRing direction face

  {- CUBIES -}

  data Sticker = Sticker { stickerFace :: Face, stickerColor :: Color }
                 deriving(Show, Eq)
  data Cubie = Edge (Sticker, Sticker) | Corner (Sticker, Sticker, Sticker)
               deriving(Show, Eq)

  fromStickers :: [Sticker] -> Cubie
  fromStickers [s1, s2] = Edge (s1, s2)
  fromStickers [s1, s2, s3] = Corner (s1, s2, s3)
  fromStickers _ = undefined

  stickers :: Cubie -> [Sticker]
  stickers cubie = case cubie of
    Edge (s1, s2) -> [s1, s2]
    Corner (s1, s2, s3) -> [s1, s2, s3]

  cubieFaces :: Cubie -> [Face]
  cubieFaces = map stickerFace . stickers

  mapStickers :: (Sticker -> Sticker) -> Cubie -> Cubie
  mapStickers f = fromStickers . map f . stickers

  mapCubieFaces :: (Face -> Face) -> Cubie -> Cubie
  mapCubieFaces f = mapStickers faceMap
    where faceMap (Sticker face color) = Sticker (f face) color

  rotateCubie :: Face -> Cubie -> Cubie
  rotateCubie f = mapCubieFaces (rotateFace f)

  colorOnFace :: Face -> Cubie -> Color
  colorOnFace f = stickerColor . stickerByFace f
    where
      stickerByFace f = head . filter (\s -> stickerFace s == f) . stickers

  onFace :: Face -> Cubie -> Bool
  onFace face cubie = face `elem` cubieFaces cubie

  onFaces :: [Face] -> Cubie -> Bool
  onFaces faces cubie = all (\f -> onFace f cubie) faces

  onExactFaces :: [Face] -> Cubie -> Bool
  onExactFaces faces cubie = faces `same` map stickerFace (stickers cubie)
    where same l1 l2 = sort l1 == sort l2

  {- CUBE -}
  data Cube = Cube [Cubie]
    deriving(Show, Eq)

  initCube :: Cube
  initCube = Cube (initEdges ++ initCorners)
    where
      -- for the edges, we take the edges around the three rings
      -- to get all twelve
      initEdges = do
        rotatingFace <- [U, R, F]
        s1 <- faceRing rotatingFace
        let s2 = rotateFace rotatingFace s1
        return $ Edge (initSticker s1, initSticker s2)

      -- for corners, we take the rings round the top and bottom
      -- to get all eight
      initCorners = do
        ud <- [U, D]
        side1 <- faceRing ud
        let side2 = rotateFace ud side1
        return $ Corner (initSticker side1, initSticker side2, initSticker ud)

      initSticker :: Face -> Sticker
      initSticker f = Sticker f (initColor f)

  cubiesOnFaces :: [Face] -> Cube -> [Cubie]
  cubiesOnFaces faces (Cube cubies) = filter (onFaces faces) cubies

  cubieByFaces :: [Face] -> Cube -> Cubie
  cubieByFaces faces (Cube cubies) = head $ filter (onExactFaces faces) cubies

  faceColors :: Face -> Face -> Cube -> [[Color]]
  faceColors face topFace cube = [topRow, middleRow, bottomRow]
    where
      rightFace = rotateFace face topFace
      bottomFace = rotateFace face rightFace
      leftFace = rotateFace face bottomFace

      c :: [Face] -> Color
      c faces = colorOnFace face $ cubieByFaces faces cube
      topRow = [ c [topFace, face, leftFace]
               , c [topFace, face]
               , c [topFace, face, rightFace]
               ]

      middleRow = [ c [face, leftFace]
                  , initColor face -- middle sticker
                  , c [face, rightFace]
                  ]

      bottomRow = [ c [bottomFace, face, leftFace]
                  , c [bottomFace, face]
                  , c [bottomFace, face, rightFace]
                  ]

  -- TODO: print the cube in the sideways-cross.
  printCube :: Cube -> IO ()
  printCube cube = do
    printBlock 2 $ faceColors U B cube
    printBlock 0 $ concatColors $ [ faceColors B U cube
                                  , faceColors L U cube
                                  , faceColors F U cube
                                  , faceColors R U cube
                                  ]
    printBlock 2 $ faceColors D F cube

    where
      printBlock indent = mapM_ printLine
        where
          printLine line = do
            putStr $ replicate (6*indent) ' '
            printSwatches line
            putChar '\n'

      concatColors = foldl1 (zipWith (++))

  printCubie :: Cubie -> IO ()
  printCubie cubie = do
    let st = stickers cubie
    let location = concat $ map (show . stickerFace) st
    putStr $ location ++ ": "
    printSwatches $ map stickerColor st
    putChar '\n'

  {- TURNS -}
  data Turn = NullTurn
            | Forward Face
            | Reverse Face
            | Double Face
            | Multi Turn Turn

  turnForward :: Face -> Cube -> Cube
  turnForward f (Cube cubies) =
    Cube (map turnCubie cubies) where
      turnCubie :: Cubie -> Cubie
      turnCubie cubie
        | onFace f cubie = rotateCubie f cubie
        | otherwise      = cubie

  applyTurn :: Turn -> Cube -> Cube
  applyTurn turn = case turn of
    NullTurn    -> id
    Forward f   -> turnForward f
    Double f    -> turnForward f . turnForward f
    Reverse f   -> turnForward f . turnForward f . turnForward f
    Multi t1 t2 -> applyTurn t2 . applyTurn t1

  applyTurns :: [Turn] -> Cube -> Cube
  applyTurns = applyTurn . mconcat

  instance Monoid Turn where
    mappend = Multi
    mempty = NullTurn

  showTurn :: Turn -> String
  showTurn turn = case turn of
    NullTurn    -> ""
    Forward f   -> show f
    Reverse f   -> show f ++ "'"
    Double f    -> show f ++ "2"
    Multi t1 t2 -> showTurn t1 ++ showTurn t2

  instance Show Turn where show = ("readTurn " ++) . show . showTurn
  -- instance Read Turn where read = readTurn

  readTurn :: String -> Turn
  readTurn input = case parse parser "input" input of
    Left _       -> undefined
    Right result -> result
    where
      parser = fmap mconcat $ many $ do
        face <- parseFace
        modifier <- parseModifier
        return $ modifier face
        where
          parseFace = fmap (read . return) $ oneOf $ map (head . show) allFaces
          parseModifier = (char '\'' >> return Reverse)
                      <|> (char '2'  >> return Double)
                      <|> (return Forward)

  infixr 5 +++
  (+++) :: Turn -> Turn -> Turn
  (+++) = mappend

  result :: Turn -> Cube
  result turn = applyTurn turn initCube

  inverse :: Turn -> Turn
  inverse turn = case turn of
    NullTurn    -> NullTurn
    Forward f   -> Reverse f
    Reverse f   -> Forward f
    Double f    -> Double f
    Multi t1 t2 -> Multi (inverse t2) (inverse t1)

  turnSeq :: Turn -> [Turn]
  turnSeq NullTurn    = []
  turnSeq (Multi t1 t2) = turnSeq t1 ++ turnSeq t2
  turnSeq x = [x]

  sexy = readTurn "RUR'U'"
  tperm = sexy +++ readTurn "R'FR2U'R'U'RUR'F'"
  sune = readTurn "RUR'URU2R'"

  -- TODO: rotations, thick turns, etc.
