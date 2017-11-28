module Count where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Data.Tuple (Tuple(..))
import Node.Stream (Writable)
import Node.Stream.Readable (iterate, pipe)

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

main :: forall eff. Eff (ref :: REF, console :: CONSOLE | eff) Unit
main = void
  $ iterate (\i -> Tuple (i + 1) (show i <> "\n")) 0 >>= (_ `pipe` stdout)
