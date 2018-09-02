module Count where

import Prelude

import Effect (Effect)
import Data.Tuple (Tuple(..))
import Node.Stream (Writable)
import Node.Stream.Readable (iterate, pipe)

foreign import stdout :: Writable ()

main :: Effect Unit
main = void
  $ iterate (\i -> Tuple (i + 1) (show i <> "\n")) 0 >>= (_ `pipe` stdout)
