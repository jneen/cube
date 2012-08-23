{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
module Data.Cube (
  -- groups
    Group,
    (|*|),
    inverse,
    identity,
    compound,
    conjugate,
    commutator,
    gconcat,
    gconcat',

  -- faces
    Face(L, R, F, B, U, D),
    allFaces,
    oppositeFace,
    faceRing,
    rotateFace,

  -- orientations
    Orientation(Orientation),
    faceOn,
    rotateOrientation,

  -- permutations
    Permutable,
    permutationList,
    makePermutation,

  -- cube
    Cube(Cube),
    makeTurn,
    makeRotation,
    printCube,

  -- algs
    Alg,
    makeAlg,
    applyAlg,
    readAlg,
    normalizeAlg,
    mirror,

  -- algs
    sexy,
    tperm,
    zperm,
    hperm,
    eperm,
    aperm,
    gperm,
    vperm,
    yperm,
    uperm,
    sune,
  ) where

  import Prelude hiding(id, (.))
  import Control.Category
  import Data.List (intersperse, sort, elemIndex, foldl')
  -- import qualified Text.ParserCombinators.Parsec as Parsec
  import Text.ParserCombinators.Parsec
  import Data.Char (toUpper, toLower)
  import Data.Array.Unboxed

  {- UTILS -}
  forceMaybe :: Maybe a -> a
  forceMaybe (Just x) = x
  forceMaybe Nothing = error "forced maybe!"

  {- GROUPS -}
  class Group a where
    infixl 7 |*|
    -- | the group operation
    (|*|)    :: a -> a -> a
    inverse  :: a -> a
    identity :: a

    -- | multiply an element by itself n times
    compound :: Int -> a -> a
    compound 0 _ = identity
    compound n x = x |*| compound (n-1) x

    -- | returns the conjugate (xyx\')
    conjugate :: a -> a -> a
    conjugate x y = x |*| y |*| inverse x

    -- | returns the commutator (xyx\'y\')
    commutator :: a -> a -> a
    commutator x y = x |*| y |*| inverse x |*| inverse y

    -- | multiply together a list of elements
    gconcat :: [a] -> a
    gconcat = foldl (|*|) identity

    -- | multiply together a list of elements, strictly
    gconcat' :: [a] -> a
    gconcat' = foldl' (|*|) identity

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
  -- | a face on the cube
  data Face = L | R | F | B | U | D deriving(Show, Eq, Read, Ord)
  -- | a list of all the available faces
  allFaces = [L, R, F, B, U, D]

  initColor :: Face -> Color
  initColor U = White
  initColor D = Yellow
  initColor F = Green
  initColor B = Blue
  initColor R = Red
  initColor L = Orange

  -- | the face on the other side of the cube
  oppositeFace :: Face -> Face
  oppositeFace L = R
  oppositeFace R = L
  oppositeFace F = B
  oppositeFace B = F
  oppositeFace U = D
  oppositeFace D = U

  -- | a clockwise list of faces forming a ring around the given face.
  --
  -- The rule is that, for any given face 'f', the sets
  -- @[f]@, @'faceRing' f@, and @['oppositeFace' f]@ are disjoint,
  -- and include all of the faces.
  faceRing :: Face -> [Face]
  faceRing U = [F, L, B, R]
  faceRing R = [F, U, B, D]
  faceRing F = [U, R, D, L]
  faceRing f = reverse $! faceRing $! oppositeFace f

  nextFaceRing :: Face -> Face -> [Face]
  nextFaceRing direction face = take 4 nextCycle
    where
      nextCycle = tail $ dropWhile (/= face) $ cycle (faceRing direction)


  -- | @'rotateFace' rotatingFace face@ will return the face obtained by
  -- rotating the cube clockwise about @rotatingFace@.  Note that
  -- rotateFace is a noop if the rotating face is the same face or the
  -- opposite of the given face.  Note also that
  --
  -- > rotateFace ('oppositeface' f)
  --
  -- is the inverse of @rotateFace f@.
  rotateFace :: Face -> Face -> Face
  rotateFace direction face
    | direction == face                 = face
    | direction == oppositeFace face    = face
    | otherwise                         = head $ nextFaceRing direction face

  mirrorFace :: Face -> Face -> Face
  mirrorFace mirror face
    | mirror == face              = oppositeFace face
    | mirror == oppositeFace face = oppositeFace face
    | otherwise                   = face

  {- CUBIES -}
  data Cubie = Edge (Face, Face) | Corner (Face, Face, Face)
               deriving(Show)

  getFaces :: Cubie -> [Face]
  getFaces (Edge (x, y)) = [x, y]
  getFaces (Corner (x, y, z)) = [x, y, z]

  fromFaces :: [Face] -> Cubie
  fromFaces (x:y:[]) = Edge (x, y)
  fromFaces (x:y:z:[]) = Corner (x, y, z)
  fromFaces _ = error "a cubie has two or three faces!"

  mapCubieFaces :: (Face -> Face) -> Cubie -> Cubie
  mapCubieFaces f = fromFaces . map f . getFaces

  rotateCubie :: Face -> Cubie -> Cubie
  rotateCubie = mapCubieFaces . rotateFace

  instance Eq Cubie where
    (==) c1 c2 = f c1 == f c2
                 where f = sort . getFaces

  {- FACETS -}
  newtype Facet = Facet (Cubie, Face) deriving (Eq, Show)
  getFacetFace (Facet (_, f)) = f
  getFacetCubie (Facet (c, _)) = c

  onFace :: Face -> Facet -> Bool
  onFace f (Facet (c, _)) = f `elem` getFaces c

  rotateFacet :: Face -> Facet -> Facet
  rotateFacet r (Facet (c, f)) = Facet (rotateCubie r c, rotateFace r f)

  mapFacetFaces :: (Face -> Face) -> Facet -> Facet
  mapFacetFaces fn (Facet (c, f)) = Facet (mapCubieFaces fn c, fn f)

  {- ORIENTATIONS -}
  -- | a representation of a particular orientation of the cube.
  --
  -- As a group, the identity has 'U' on the top and 'F' on the front,
  -- and composition is as in the group of isometries of the cube.
  data Orientation = Orientation { topFace :: Face, frontFace :: Face }
    deriving(Show, Eq)

  -- | returns the face in a particular orientation of the cube.
  --
  -- for example, @faceOn U orientation@ will return the face on top
  -- when the cube is held in @orientation@.
  faceOn :: Face -> Orientation -> Face
  faceOn U o = topFace o
  faceOn D o = oppositeFace $ topFace o
  faceOn L o = rotateFace (topFace o) $ frontFace o
  faceOn R o = rotateFace (topFace o) $ oppositeFace $ frontFace o
  faceOn F o = frontFace o
  faceOn B o = oppositeFace $ frontFace o

  -- | returns the orientation obtained by rotating 
  rotateOrientation :: Face -> Orientation -> Orientation
  rotateOrientation face o@(Orientation t f) = Orientation (rotate t) (rotate f)
    where rotate = rotateFace $ faceOn (oppositeFace face) o

  orientOrientation :: Orientation -> Orientation -> Orientation
  orientOrientation o1 o2 =
    Orientation (faceOn (topFace o1) o2) (faceOn (frontFace o1) o2)

  invertOrientation :: Orientation -> Orientation
  invertOrientation o
    | o2 == identity = o
    | o3 == identity = o2
    | otherwise = o3
      where
        o2 = o |*| o
        o3 = o2 |*| o

  initOrientation = Orientation U F

  instance Group Orientation where
    identity = initOrientation
    (|*|) = orientOrientation
    inverse = invertOrientation

  {- PERMUTATIONS -}
  -- | A permutation of a given set, determined by the type.
  data Permutation a = Permutation (Array Int Int) deriving(Show, Eq)
  -- TODO: make Eq more general

  mapArrayAssocs :: (Ix i) => ((i, a) -> (i, a)) -> Array i a -> Array i a
  mapArrayAssocs f arr = array (bounds arr) (map f $ assocs arr)

  arrayFromList :: [a] -> Array Int a
  arrayFromList l = array (0, length l - 1) (zip [0..] l)

  composePerm :: (Eq a, Permutable a) =>
    Permutation a -> Permutation a -> Permutation a
  composePerm (Permutation p1) (Permutation p2) = Permutation p3
    where
      p3 = fmap (p1 !) p2

  inversePerm :: Permutation a -> Permutation a
  inversePerm (Permutation arr) = Permutation (mapArrayAssocs invert arr)
    where invert (x, y) = (y, x)

  runPerm :: (Eq a, Permutable a) => a -> Permutation a -> a
  runPerm el (Permutation arr) = permutationArray ! (arr ! idx)
    where
      idx = (permutationIndex el)

  class (Eq a) => Permutable a where
    permutationList :: [a]
    permutationArray :: Array Int a
    permutationArray = arrayFromList permutationList

    permutationIndex :: a -> Int
    permutationIndex = forceMaybe . flip elemIndex permutationList

    makePermutation :: (a -> a) -> Permutation a
    makePermutation f = Permutation $ fmap (permutationIndex . f) permutationArray

  instance (Eq a, Permutable a) => Group (Permutation a) where
    identity = makePermutation id
    inverse = inversePerm
    (|*|) = composePerm

  instance Permutable Facet where
    permutationList = do
      cubie <- allEdges ++ allCorners
      face <- getFaces cubie
      return $ Facet (cubie, face)
      where
        allEdges = do
          rotatingFace <- [U, R, F]
          s1 <- faceRing rotatingFace
          let s2 = rotateFace rotatingFace s1
          return $ Edge (s1, s2)

        allCorners = do
          ud <- [U, D]
          side1 <- faceRing ud
          let side2 = rotateFace ud side1
          return $ Corner (side1, side2, ud)

  makeOrientationPerm :: Orientation -> Permutation Facet
  makeOrientationPerm = makePermutation . mapFacetFaces . flip faceOn

  {- CUBE -}
  data Cube = Cube { getOrientation :: Orientation
                   , getPermutation :: Permutation Facet
                   }
    deriving(Show, Eq)

  combineCubes :: Cube -> Cube -> Cube
  combineCubes (Cube o1 p1) (Cube o2 p2) = Cube o3 p3
    where
      o3 = o1 |*| o2
      -- permutations are multiplied in compositional order,
      -- but cubes are in pipelining order, which is reversed.
      p3 = reorient o1 p2 |*| p1
      reorient = conjugate . makeOrientationPerm

  invertCube :: Cube -> Cube
  invertCube (Cube orientation permutation) =
    Cube (inverse orientation) (inverse permutation)

  initCube = Cube identity identity

  instance Group Cube where
    identity = initCube
    inverse = invertCube
    (|*|) = combineCubes

  makeTurn :: Face -> Cube
  makeTurn f = Cube identity (makePermutation turn)
    where
      turn facet
        | onFace f facet = rotateFacet f facet
        | otherwise      = facet

  makeRotation :: Face -> Cube
  makeRotation f = Cube (rotateOrientation f identity) identity

  colorAt :: Facet -> Cube -> Color
  colorAt facet cube = initColor face
    where
      face = getFacetFace $ runPerm facet $ inverse $ getPermutation cube

  faceColors :: Orientation -> Cube -> [[Color]]
  faceColors or cube = [topRow, middleRow, bottomRow]
    where
      orientation = or |*| getOrientation cube
      shownFace = topFace orientation

      -- helpers for building the color lists
      f face = faceOn face orientation
      c pos = colorAt (Facet (pos, topFace orientation)) cube

      topRow = [ c (Corner (f B, shownFace, f L))
               , c (Edge (f B, shownFace))
               , c (Corner (f B, shownFace, f R))
               ]

      middleRow = [ c (Edge (shownFace, f L))
                  , initColor shownFace
                  , c (Edge (shownFace, f R))
                  ]

      bottomRow = [ c (Corner (f F, shownFace, f L))
                  , c (Edge (f F, shownFace))
                  , c (Corner (f F, shownFace, f R))
                  ]

  printCube :: Cube -> IO ()
  printCube cube = do
    printBlock 2 $ faceColors (Orientation U F) cube
    printBlock 0 $ concatColors $ [ faceColors (Orientation B D) cube
                                  , faceColors (Orientation L D) cube
                                  , faceColors (Orientation F D) cube
                                  , faceColors (Orientation R D) cube
                                  ]
    printBlock 2 $ faceColors (Orientation D B) cube
    where
      printBlock indent = mapM_ printLine
        where
          printLine line = do
            putStr $ replicate (6*indent) ' '
            printSwatches line
            putChar '\n'

      concatColors = foldl1 (zipWith (++))

  {- ALGORITHMS -}
  data Direction = Forward | Reverse | Double
  data Operation = Turn | Rotate | Thick | Middle
  data Move = Move Operation Direction Face
  newtype Alg = Alg ([Move], Cube)

  applyAlg :: Alg -> Cube
  applyAlg (Alg (_, cube)) = cube

  algMoves :: Alg -> [Move]
  algMoves (Alg (m, _)) = m

  makeAlg :: [Move] -> Alg
  makeAlg moves = Alg (moves, applyMoves moves)

  oppositeDirection :: Direction -> Direction
  oppositeDirection Double = Double
  oppositeDirection Forward = Reverse
  oppositeDirection Reverse = Forward

  applyOperation :: Operation -> Face -> Cube
  applyOperation op f = case op of
    Turn   -> makeTurn f
    Rotate -> makeRotation f
    Thick  -> turnThick f
    Middle -> turnThick f |*| turnReverse f
    where
      turnReverse f = applyDirection Reverse (makeTurn f)
      turnThick f = makeRotation f |*| makeTurn (oppositeFace f)

  applyDirection :: (Group a) => Direction -> a -> a
  applyDirection dir c = case dir of
    Forward -> c
    Double -> compound 2 c
    Reverse -> compound 3 c

  applyMove :: Move -> Cube
  applyMove (Move op dir face) =
    applyDirection dir $ applyOperation op face

  applyMoves :: [Move] -> Cube
  applyMoves moves = gconcat' $ map applyMove moves

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
  mapAlg f = makeAlg . map f . algMoves

  inverseMove :: Move -> Move
  inverseMove (Move op dir face) = Move op (oppositeDirection dir) face

  inverseMoves :: [Move] -> [Move]
  inverseMoves = map inverseMove . reverse

  instance Group Alg where
    identity = Alg ([], identity)
    Alg (xs, rx) |*| Alg (ys, ry) = Alg ((xs ++ ys), rx |*| ry)
    inverse (Alg (moves, res)) = Alg (inverseMoves moves, inverse res)

  normalizeAlg :: Alg -> Alg
  normalizeAlg (Alg (moves, result)) = Alg (normalizedMoves, result)
    where
      startOrientation = Orientation U F
      normalizeRotations :: Orientation -> [Move] -> [Move]
      normalizeRotations _ [] = []
      normalizeRotations orientation (Move op dir face:tail) = case op of
        Rotate -> normalizeRotations (rot |*| orientation) tail
          where rot = applyDirection dir (rotateOrientation face identity)
        other -> Move op dir newFace : normalizeRotations orientation tail
          where newFace = faceOn face orientation

      normalizedMoves = normalizeRotations identity (moves >>= normalizeMove)

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
  showAlg = concat . intersperse " " . map showMove . algMoves

  instance Show Alg where show = ("readAlg " ++) . show . showAlg

  readAlg :: String -> Alg
  readAlg input = case parse parser "input" input of
    Left _       -> undefined
    Right result -> result
    where
      parser = fmap makeAlg $ many $ do
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

  printResult :: Alg -> IO ()
  printResult = printCube . applyAlg

  showLL :: Alg -> IO ()
  showLL alg = printResult $ gconcat [readAlg "z2", alg, readAlg "x'"]

  solveCase :: Alg -> Cube
  solveCase = applyAlg . inverse

  printCase :: Alg -> IO ()
  printCase = printCube . solveCase

  sexy = commutator (readAlg "R") (readAlg "U")

  startT = commutator (readAlg "R") (readAlg "U")
       |*| commutator (readAlg "R'") (readAlg "F")

  endT = readAlg "FRU'R'U' RUR'F'"

  tperm = startT |*| endT
  yperm = endT |*| startT
  uperm = readAlg "R'U R'U'  R'U' R'U  RUR2"
  vperm = readAlg "R'U R'd' R'F' R2U' R'U R'F RF y'"
  zperm = readAlg "M2U' M2U' M'U2 M2U2 M'U2"
  hperm = half |*| (readAlg "U2") |*| half
    where half = conjugate (readAlg "R2U2") (readAlg "R")

  jperm = readAlg $! "RU2R'U' RU2L'U R'U'L"
  aperm = conjugate (readAlg "x R2") (commutator (readAlg "RUR'") (readAlg "D2"))

  gperm = readAlg "RUR' y' R2u'RU'R'UR'u R2 y"

  eperm = conjugate (readAlg "x'") (
          commutator (conjugate (readAlg "R") (readAlg "U'")) (readAlg "D")
      |*| commutator (conjugate (readAlg "R") (readAlg "U")) (readAlg "D")
      )

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

  -- optimizeAlg :: Alg -> Alg
  -- optimizeAlg = doOptimize . normalizeAlg
  --   where
  --     doOptimize (Alg moves) = Alg (optimizeMoves moves)
  --     isIdentity moves = identity == applyAlg (Alg moves)
  --     optimizeMoves [] = []
  --     optimizeMoves all@(x:xs)
  --       | isIdentity (xs)  = return x
  --       | isIdentity (tail rev) = return $ head rev
  --       | otherwise = x : optimizeMoves xs
  --         where rev = reverse all

  -- scoreAlg :: Alg -> Int
  -- scoreAlg = length . algMoves . normalizeAlg
