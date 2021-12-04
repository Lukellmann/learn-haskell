import Data.Bits as B
import Data.Bool as B
import Data.List
import qualified Data.Set as Set
import Data.Set (Set, empty, insert)
import qualified Data.Text
import Data.Tree (Tree (Node))
import Data.Tuple hiding (snd, swap)
import MyModule (f)

main :: IO ()
main = do
  input <- getLine
  print (f (read input :: Double))
