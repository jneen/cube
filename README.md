# Data.RubiksCube

Try this:

```
ghci> :m + Data.RubiksCube
ghci> zperm
readAlg "M2 U' M2 U' M' U2 M2 U2 M' U2"
ghci> inverse zperm
readAlg "U2 M U2 M2 U2 M U M2 U M2"
ghci> printCube (applyAlg zperm)
[[ a really awesome printout of the result of a zperm ]]
ghci> readAlg "R" |*| readAlg "U"
readAlg "R U"
ghci> conjugate (readAlg "R") (readAlg "U")
readAlg "R U R'"
ghci> inverse (conjugate (readAlg "R") (readAlg "U"))
readAlg "R U' R'"
ghci> commutator (readAlg "D") (readAlg "RUR'")
readAlg "D R U R' D' R U R'"
```
