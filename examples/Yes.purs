module Yes where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Stream (Writable)
import Node.Stream.Readable (repeat, pipe)

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = void $ repeat "yes\n" >>= (_ `pipe` stdout)
