import Test.QuickCheck
import Data.Cube
import Text.Printf

instance Arbitrary Face where
  arbitrary = elements allFaces

instance Arbitrary Color where
  arbitrary = elements allColors

instance Arbitrary Sticker where
  arbitrary = do
    f <- arbitrary
    c <- arbitrary
    return $ Sticker f c

instance Arbitrary Edge where
  arbitrary = do
    s1 <- arbitrary
    s2 <- arbitrary
    return $ Edge s1 s2

instance Arbitrary Corner where
  arbitrary = do
    s1 <- arbitrary
    s2 <- arbitrary
    s3 <- arbitrary
    return $ Corner s1 s2 s3

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

testRing :: Face -> Bool
testRing f = not $ f `elem` faceRing f

tests = [("a face is not in its ring", quickCheck testRing)]
