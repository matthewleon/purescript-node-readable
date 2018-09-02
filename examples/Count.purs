module Count where

import Prelude

import Effect (Effect)
import Data.Tuple (Tuple(..))
import Node.Process (stdout)
import Node.Stream.Readable (iterate, pipe)

main :: Effect Unit
main = void
  $ iterate (\i -> Tuple (i + 1) (show i <> "\n")) 0 >>= (_ `pipe` stdout)
