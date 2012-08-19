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

  -- orientations
    Orientation(Orientation),
    faceOn,
    rotateOrientation,

  -- cubies
    Sticker(Sticker),
    Cubie(Edge, Corner),

  -- cube
    Cube(Cube),
    printCube,

  -- turns
  --  Turn(NullTurn, Forward, Reverse, Double, Multi),
  --  readTurn,
  --  showTurn,
  --  applyTurn,

  -- algs
  --  sexy,
  --  tperm,
  --  sune
  ) where

  import Data.Monoid
  import Data.List (intersperse, (\\), sort)
  -- import qualified Text.ParserCombinators.Parsec as Parsec
  import Text.ParserCombinators.Parsec
  import Data.Char (toUpper, toLower)

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

  initColor :: Face -> Color
  initColor U = White
  initColor D = Yellow
  initColor F = Green
  initColor B = Blue
  initColor R = Red
  initColor L = Orange

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

  rotateFace :: Face -> Face -> Face
  rotateFace direction face
    -- noop if the rotating face is the same face or the opposite
    | direction == face                 = face
    | direction == oppositeFace face    = face
    | otherwise                         = head $ nextFaceRing direction face

  mirrorFace :: Face -> Face -> Face
  mirrorFace mirror face
    | mirror == face              = oppositeFace face
    | mirror == oppositeFace face = oppositeFace face
    | otherwise                   = face

  {- ORIENTATIONS -}
  data Orientation = Orientation { topFace :: Face, frontFace :: Face }
    deriving(Show, Eq)

  faceOn :: Face -> Orientation -> Face
  faceOn U o = topFace o
  faceOn D o = oppositeFace $ topFace o
  faceOn L o = rotateFace (topFace o) $ frontFace o
  faceOn R o = rotateFace (topFace o) $ oppositeFace $ frontFace o
  faceOn F o = frontFace o
  faceOn B o = oppositeFace $ frontFace o

  rotateOrientation :: Face -> Orientation -> Orientation
  rotateOrientation face o@(Orientation t f) = Orientation (rotate t) (rotate f)
    where rotate = rotateFace $ faceOn (oppositeFace face) o

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

  onExactFaces :: [Face] -> Cubie -> Bool
  onExactFaces faces cubie = faces `same` map stickerFace (stickers cubie)
    where same xs ys = sort xs == sort ys

  {- CUBE -}
  data Cube = Cube [Cubie] Orientation
    deriving(Show, Eq)

  rotateCube :: Face -> Cube -> Cube
  rotateCube face (Cube cubies orientation) = Cube newCubies newOrientation
    where
      newCubies = map (rotateCubie face) cubies
      newOrientation = rotateOrientation face orientation

  getOrientation :: Cube -> Orientation
  getOrientation (Cube _ o) = o

  initCube :: Cube
  initCube = Cube (initEdges ++ initCorners) initOrientation
    where
      initOrientation = Orientation U F
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

  cubieByFaces :: [Face] -> Cube -> Cubie
  cubieByFaces faces (Cube cubies _) = head $ filter (onExactFaces faces) cubies

  middleColorOnFace :: Face -> Cube -> Color
  middleColorOnFace f (Cube _ orientation) = initColor (faceOn f orientation)

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
                  , middleColorOnFace face cube
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

  {- TURNS -}
  data Direction = Forward | Reverse | Double
  data Operation = Turn | Rotate | Thick | Middle
  data Move = Move Operation Direction Face
  newtype Alg = Alg [Move]

  oppositeDirection :: Direction -> Direction
  oppositeDirection Double = Double
  oppositeDirection Forward = Reverse
  oppositeDirection Reverse = Forward

  applyOperation :: Operation -> Face -> Cube -> Cube
  applyOperation op f = case op of
    Turn   -> turnForward f
    Rotate -> rotateCube f
    Thick  -> turnThick f
    Middle -> turnThick f . turnReverse f
    where
      turnReverse f = doTimes 3 (turnForward f)
      turnThick f = rotateCube f . turnForward (oppositeFace f)

  turnForward :: Face -> Cube -> Cube
  turnForward f (Cube cubies o) =
    Cube (map turnCubie cubies) o where
      turnCubie :: Cubie -> Cubie
      turnCubie cubie
        | onFace f cubie = rotateCubie f cubie
        | otherwise      = cubie

  doTimes :: Int -> (a -> a) -> (a -> a)
  doTimes n = foldl1 (.) . replicate n

  applyMove :: Move -> Cube -> Cube
  applyMove (Move op dir face) = case dir of
    Forward -> doMove
    Double  -> doTimes 2 doMove
    Reverse -> doTimes 3 doMove
    where
      doMove = applyOperation op face

  applyAlg :: Alg -> Cube -> Cube
  applyAlg (Alg moves) cube = foldl (flip applyMove) cube moves

  rotateMove :: Face -> Move -> Move
  rotateMove face (Move op dir moveFace) =
    Move op dir (rotateFace face moveFace)

  rotateAlg :: Face -> Alg -> Alg
  rotateAlg = mapAlg . rotateMove

  mirrorMove :: Face -> Move -> Move
  mirrorMove mirror (Move op dir face) =
    Move op (oppositeDirection dir) (mirrorFace mirror face)

  mirrorAlg :: Face -> Alg -> Alg
  mirrorAlg = mapAlg . mirrorMove

  mirror = mirrorAlg R

  orientMove :: Orientation -> Move -> Move
  orientMove o (Move op dir face) = Move op dir (faceOn face o)

  orientAlg :: Orientation -> Alg -> Alg
  orientAlg = mapAlg . orientMove

  mapAlg :: (Move -> Move) -> Alg -> Alg
  mapAlg f (Alg moves) = Alg (map f moves)

  inverseMove :: Move -> Move
  inverseMove (Move op dir face) = Move op (oppositeDirection dir) face

  inverse :: Alg -> Alg
  inverse (Alg moves) = Alg $ map inverseMove $ reverse moves

  showMove :: Move -> String
  showMove (Move op dir face) = case op of
    Turn  -> show face ++ showDirection dir
    Thick -> map toLower (show face) ++ showDirection dir
    Rotate ->
      case face of
           R -> "x" ++ showDirection dir
           U -> "y" ++ showDirection dir
           F -> "z" ++ showDirection dir

           -- these won't be generated by readAlg, but may
           -- be created by rotateMove, so we still have to
           -- show them correctly.
           L -> "x" ++ showDirection (oppositeDirection dir)
           D -> "y" ++ showDirection (oppositeDirection dir)
           B -> "z" ++ showDirection (oppositeDirection dir)

    Middle ->
      case face of
           L -> "M" ++ showDirection dir
           R -> "M" ++ showDirection (oppositeDirection dir)

           D -> "E" ++ showDirection dir
           U -> "E" ++ showDirection (oppositeDirection dir)

           F -> "S" ++ showDirection dir
           B -> "S" ++ showDirection (oppositeDirection dir)


  showDirection :: Direction -> String
  showDirection dir = case dir of
    Forward -> ""
    Reverse -> "'"
    Double  -> "2"

  showAlg :: Alg -> String
  showAlg (Alg moves) = concat . intersperse " " . map showMove $ moves

  instance Show Alg where show = ("readAlg " ++) . show . showAlg

  readAlg :: String -> Alg
  readAlg input = case parse parser "input" input of
    Left _       -> undefined
    Right result -> result
    where
      parser = fmap Alg $ many $ do
        spaces
        (op, face) <- parseLetter
        dir <- parseDirection
        spaces
        return $ Move op dir face
        where
          faceLetters = map (head . show) allFaces

          parseTurn :: Parser (Operation, Face)
          parseTurn = do
            face <- fmap (read . return) $ oneOf $ faceLetters
            return (Turn, face)

          parseThick :: Parser (Operation, Face)
          parseThick = do
            face <- fmap (read . return . toUpper) $ oneOf $ map toLower faceLetters
            return (Thick, face)

          parseRotation :: Parser (Operation, Face)
          parseRotation = do
            face <- (char 'x' >> return R)
                <|> (char 'y' >> return U)
                <|> (char 'z' >> return F)

            return (Rotate, face)

          parseMiddle :: Parser (Operation, Face)
          parseMiddle = do
            face <- (char 'M' >> return L)
                <|> (char 'E' >> return D)
                <|> (char 'S' >> return F)

            return (Middle, face)

          parseLetter :: Parser (Operation, Face)
          parseLetter = parseTurn
                    <|> parseRotation
                    <|> parseThick
                    <|> parseMiddle

          parseDirection :: Parser Direction
          parseDirection = (char '\'' >> return Reverse)
                       <|> (char '2'  >> return Double)
                       <|> (return Forward)

  instance Monoid Alg where
    mempty = Alg []
    mappend (Alg xs) (Alg ys) = Alg (xs ++ ys)

  infixr 5 +++
  (+++) :: (Monoid a) => a -> a -> a
  (+++) = mappend

  result :: Alg -> Cube
  result alg = applyAlg alg initCube

  solveCase :: Alg -> Cube
  solveCase = result . inverse

  normalize :: Alg -> Alg -> Alg
  normalize a b = a +++ b +++ inverse a

  intertwine :: Alg -> Alg -> Alg
  intertwine a b = normalize a b +++ inverse b

  sexy = intertwine (readAlg "R") (readAlg "U")

  startT = intertwine (readAlg "R") (readAlg "U")
       +++ intertwine (readAlg "R'") (readAlg "F")

  endT = readAlg "FRU'R'U' RUR'F'"

  tperm = startT +++ endT
  yperm = endT +++ startT
  uperm = readAlg "R'U R'U'  R'U' R'U  RUR2"

  vperm = readAlg "R'U R'd' R'F' R2U' R'U R'F RF y'"

  zperm = readAlg "M2U' M2U' M'U2 M2U2 M'U2"
  hperm = half +++ (readAlg "U2") +++ half
    where half = normalize (readAlg "R2U2") (readAlg "R")

  jperm = readAlg "RU2R'U' RU2L'U R'U'L"

  gperm = readAlg "RUR' y' R2u'RU'R'UR'u R2 y"

  sune = readAlg "RUR'U RU2R'"

  {- NORMALIZATION -}
  -- turns a single move into an array of Turns and Rotations
  normalizeMove :: Move -> [Move]
  normalizeMove move@(Move op dir face) = case op of
    Thick -> [ Move Rotate dir face
             , Move Turn dir (oppositeFace face)
             ]
    Middle -> [ Move Rotate dir face
              , Move Turn dir (oppositeFace face)
              , Move Turn (oppositeDirection dir) face
              ]
    _ -> [move]

  normalizeAlg :: Alg -> Alg
  normalizeAlg (Alg moves) = Alg $ do
    move <- moves
    normalizeMove move
