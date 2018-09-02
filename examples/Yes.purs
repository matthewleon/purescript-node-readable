module Yes where

import Prelude

import Effect (Effect)
import Node.Process (stdout)
import Node.Stream.Readable (repeat, pipe)

main :: Effect Unit
main = void $ repeat "yes\n" >>= (_ `pipe` stdout)
