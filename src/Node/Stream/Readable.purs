module Node.Stream.Readable (
  Readable
, kind Region
, class Chunkable
, ReadCb
, Size
, Push
, newReadable
, newReadable'
, repeat
, repeat'
, iterate
, iterate'
, unfoldr
, unfoldr'
, push
, pushStringWithEncoding
, pushEnd
, pipe
) where

import Prelude

import Control.Monad.Eff (Eff, untilE, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Function.Uncurried (Fn2)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..), swap)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream (Readable, Writable, pipe) as S

-- | A readable stream parameterized by the type of its chunks
newtype Readable chunktype r eff = Readable (S.Readable r eff)
derive instance newtypeReadable :: Newtype (Readable chunktype r eff) _

foreign import kind Region

-- | We use a phantom parameter to contain use of `push`
foreign import data Push :: Region -> Effect

type Size = Int

-- | The read callback for readable streams
type ReadCb chunktype r p eff
   = Readable chunktype r eff
  -> Size
  -> Eff (push :: Push p | eff) Unit

type StreamOptions eff = (
  highWaterMark :: Int
, destroy :: Fn2 Error (Eff eff Unit) (Eff eff Unit)
)

class Chunkable t
instance chunkableString :: Chunkable String
instance chunkableBuffer :: Chunkable Buffer
instance chunkableUint8Array :: Chunkable (ArrayView Uint8)

newReadable
  :: forall chunktype r p eff
   . Chunkable chunktype
  => ReadCb chunktype r p eff
  -> Eff eff (Readable chunktype r eff)
newReadable = map wrap <<< newReadableImpl {}

newReadable'
  :: forall optionsrow rest chunktype r p eff
   . Union optionsrow rest (StreamOptions eff)
  => Chunkable chunktype
  => { | optionsrow}
  -> ReadCb chunktype r p eff
  -> Eff eff (Readable chunktype r eff)
newReadable' r = map wrap <<< newReadableImpl r

foreign import newReadableImpl
  :: forall optionsrow chunktype r p eff
   . { | optionsrow}
  -> ReadCb chunktype r p eff
  -> Eff eff (S.Readable r eff)

repeat
  :: forall chunktype r eff
   . Chunkable chunktype
  => chunktype
  -> Eff eff (Readable chunktype r eff)
repeat = repeat' {}

repeat'
  :: forall optionsrow rest chunktype r eff
   . Union optionsrow rest (StreamOptions eff)
  => Chunkable chunktype
  => { | optionsrow}
  -> chunktype
  -> Eff eff (Readable chunktype r eff)
repeat' opts chunk = newReadable' opts
  $ \strm _ -> untilE $ not <$> push strm chunk

iterate
  :: forall chunktype iterstate r eff
   . Chunkable chunktype
  => (iterstate -> Tuple iterstate chunktype)
  -> iterstate
  -> Eff (ref :: REF | eff) (Readable chunktype r (ref :: REF | eff))
iterate = iterate' {}

iterate'
  :: forall optionsrow rest chunktype iterstate r eff
   . Union optionsrow rest (StreamOptions (ref :: REF | eff))
  => Chunkable chunktype
  => { | optionsrow}
  -> (iterstate -> Tuple iterstate chunktype)
  -> iterstate
  -> Eff (ref :: REF | eff) (Readable chunktype r (ref :: REF | eff))
iterate' opts iter = unfoldr' opts $ Just <<< swap <<< iter

unfoldr
  :: forall chunktype iterstate r eff
   . Chunkable chunktype
  => (iterstate -> Maybe (Tuple chunktype iterstate))
  -> iterstate
  -> Eff (ref :: REF | eff) (Readable chunktype r (ref :: REF | eff))
unfoldr = unfoldr' {}

unfoldr'
  :: forall optionsrow rest chunktype iterstate r eff
   . Union optionsrow rest (StreamOptions (ref :: REF | eff))
  => Chunkable chunktype
  => { | optionsrow}
  -> (iterstate -> Maybe (Tuple chunktype iterstate))
  -> iterstate
  -> Eff (ref :: REF | eff) (Readable chunktype r (ref :: REF | eff))
unfoldr' opts iter startState = do
  iterStateRef <- newRef startState
  newReadable' opts
    $ \strm _ -> untilE do
      iterState <- readRef iterStateRef
      case iter iterState of
        Nothing -> voidRight true $ pushEnd strm
        Just (Tuple chunk newState) -> do
          writeRef iterStateRef newState
          not <$> push strm chunk

push
  :: forall chunktype r p eff
   . Readable chunktype r eff
  -> chunktype
  -> Eff (push :: Push p | eff) Boolean
push r = pushImpl (unwrap r)

foreign import pushImpl
  :: forall chunktype r p eff
   . S.Readable r eff
  -> chunktype
  -> Eff (push :: Push p | eff) Boolean

pushStringWithEncoding
  :: forall r p eff
   . Readable String r eff
  -> String
  -> Encoding
  -> Eff (push :: Push p | eff) Boolean
pushStringWithEncoding rs s = pushStringWithEncodingImpl (unwrap rs) s <<< show

foreign import pushStringWithEncodingImpl
  :: forall r p eff
   . S.Readable r eff
  -> String
  -> String
  -> Eff (push :: Push p | eff) Boolean

pushEnd
  :: forall chunktype r p eff
   . Readable chunktype r eff
  -> Eff (push :: Push p | eff) Unit
pushEnd = pushEndImpl <<< unwrap

foreign import pushEndImpl
  :: forall r p eff
   . S.Readable r eff
  -> Eff (push :: Push p | eff) Unit

pipe
  :: forall chunktype w r eff
   . Readable chunktype w eff
  -> S.Writable r eff
  -> Eff eff (S.Writable r eff)
pipe r = S.pipe $ unwrap r
