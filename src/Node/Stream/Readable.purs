module Node.Stream.Readable (
  kind Region
, ReadCb
, Size
, Push
, newReadable
, pushBuffer
, pushUint8Array
, pushString
, pushStringWithEncoding
, pushEnd
) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.ArrayBuffer.Types (Uint8Array)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream (Readable)

foreign import kind Region

-- | We use a phantom parameter to contain use of push methods.
foreign import data Push :: Region -> Effect

type Size = Int

-- | The read callback for readable streams
type ReadCb r p eff
   = Readable r eff
  -> Size
  -> Eff (push :: Push p | eff) Unit

foreign import newReadable
  :: forall r p eff
   . ReadCb r p eff
  -> Eff eff (Readable r eff)

foreign import pushBuffer
  :: forall r p eff
   . Readable r eff
  -> Buffer
  -> Eff (push :: Push p | eff) Boolean

foreign import pushUint8Array
  :: forall r p eff
   . Readable r eff
  -> Uint8Array
  -> Eff (push :: Push p | eff) Boolean

foreign import pushString
  :: forall r p eff
   . Readable r eff
  -> String
  -> Eff (push :: Push p | eff) Boolean

pushStringWithEncoding
  :: forall r p eff
   . Readable r eff
  -> String
  -> Encoding
  -> Eff (push :: Push p | eff) Boolean
pushStringWithEncoding rs s = pushStringWithEncodingImpl rs s <<< show

foreign import pushStringWithEncodingImpl
  :: forall r p eff
   . Readable r eff
  -> String
  -> String
  -> Eff (push :: Push p | eff) Boolean

foreign import pushEnd :: forall p eff. Eff (push :: Push p | eff) Unit
