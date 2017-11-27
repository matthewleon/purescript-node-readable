module Count where

import Prelude

import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, runST)
import Node.Stream (Writable)
import Node.Stream.Readable (ReadCb, newStringReadable, push, pipe)

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

incrementCb :: forall r p h eff. STRef h Int -> ReadCb String r p (st :: ST h | eff)
incrementCb countRef stream _ =
  untilE do
    count <- modifySTRef countRef (_ + 1)
    not <$> push stream (show count <> "\n")

startCounting :: forall eff. Int -> Eff (console :: CONSOLE | eff) Unit
startCounting startCount = runST do
  countRef <- newSTRef startCount
  readable <- newStringReadable $ incrementCb countRef
  void $ readable `pipe` stdout

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = startCounting 0
