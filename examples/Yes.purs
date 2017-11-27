module Yes where

import Prelude

import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Stream (Writable)
import Node.Stream.Readable (ReadCb, newStringReadable, push, pipe)

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

repeatCb :: forall r p eff. String -> ReadCb String r p eff
repeatCb str stream _ = untilE $ not <$> push stream str

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  readable <- newStringReadable $ repeatCb "yes\n"
  void $ readable `pipe` stdout
