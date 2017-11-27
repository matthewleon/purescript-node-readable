module Yes where

import Prelude

import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Stream (Writable, pipe)
import Node.Stream.Readable (ReadCb, newReadable, pushString)

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

repeatCb :: forall r p eff. String -> ReadCb r p eff
repeatCb str stream _ = untilE $ not <$> pushString stream str

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  readable <- newReadable $ repeatCb "yes\n"
  void $ readable `pipe` stdout
