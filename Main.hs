import Data.Cube
import System.Environment (getArgs)

main = do
  args <- fmap concat getArgs
  printCube $ applyAlg $ readAlg args
