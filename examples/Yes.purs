module Yes where

import Prelude

import Effect (Effect)
import Node.Stream (Writable)
import Node.Stream.Readable (repeat, pipe)

foreign import stdout :: Writable ()

main :: Effect Unit
main = void $ repeat "yes\n" >>= (_ `pipe` stdout)
