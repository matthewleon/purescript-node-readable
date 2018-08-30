module Node.Stream.Readable (
  Readable
, class Chunkable
, ReadCb
, Size
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

import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Function.Uncurried (Fn2)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..), swap)
import Effect (Effect, untilE)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream (Readable, Writable, pipe) as S
import Prim.Row (class Union)

-- | A readable stream parameterized by the type of its chunks
newtype Readable chunktype r = Readable (S.Readable r)
derive instance newtypeReadable :: Newtype (Readable chunktype r) _

type Size = Int

-- | The read callback for readable streams
type ReadCb chunktype r
   = Readable chunktype r
  -> Size
  -> Effect Unit

type StreamOptions = (
  highWaterMark :: Int
, destroy :: Fn2 Error (Effect Unit) (Effect Unit)
)

class Chunkable t
instance chunkableString :: Chunkable String
instance chunkableBuffer :: Chunkable Buffer
instance chunkableUint8Array :: Chunkable (ArrayView Uint8)

newReadable
  :: forall chunktype r
   . Chunkable chunktype
  => ReadCb chunktype r -> Effect (Readable chunktype r)
newReadable = map wrap <<< newReadableImpl {}

newReadable'
  :: forall optionsrow rest chunktype r
   . Union optionsrow rest StreamOptions
  => Chunkable chunktype
  => { | optionsrow} -> ReadCb chunktype r -> Effect (Readable chunktype r)
newReadable' r = map wrap <<< newReadableImpl r

foreign import newReadableImpl
  :: forall optionsrow chunktype r
   . { | optionsrow} -> ReadCb chunktype r -> Effect (S.Readable r)

repeat
  :: forall chunktype r
   . Chunkable chunktype
  => chunktype -> Effect (Readable chunktype r)
repeat = repeat' {}

repeat'
  :: forall optionsrow rest chunktype r
   . Union optionsrow rest StreamOptions
  => Chunkable chunktype
  => { | optionsrow} -> chunktype -> Effect (Readable chunktype r)
repeat' opts chunk = newReadable' opts
  $ \strm _ -> untilE $ not <$> push strm chunk

iterate
  :: forall chunktype iterstate r
   . Chunkable chunktype
  => (iterstate -> Tuple iterstate chunktype)
  -> iterstate
  -> Effect (Readable chunktype r)
iterate = iterate' {}

iterate'
  :: forall optionsrow rest chunktype iterstate r
   . Union optionsrow rest StreamOptions
  => Chunkable chunktype
  => { | optionsrow}
  -> (iterstate -> Tuple iterstate chunktype)
  -> iterstate
  -> Effect (Readable chunktype r)
iterate' opts iter = unfoldr' opts $ Just <<< swap <<< iter

unfoldr
  :: forall chunktype iterstate r
   . Chunkable chunktype
  => (iterstate -> Maybe (Tuple chunktype iterstate))
  -> iterstate
  -> Effect (Readable chunktype r)
unfoldr = unfoldr' {}

unfoldr'
  :: forall optionsrow rest chunktype iterstate r
   . Union optionsrow rest StreamOptions
  => Chunkable chunktype
  => { | optionsrow}
  -> (iterstate -> Maybe (Tuple chunktype iterstate))
  -> iterstate
  -> Effect (Readable chunktype r)
unfoldr' opts iter startState = do
  iterStateRef <- Ref.new startState
  newReadable' opts
    $ \strm _ -> untilE do
      iterState <- Ref.read iterStateRef
      case iter iterState of
        Nothing -> voidRight true $ pushEnd strm
        Just (Tuple chunk newState) -> do
          Ref.write newState iterStateRef
          not <$> push strm chunk

push
  :: forall chunktype r
   . Readable chunktype r -> chunktype -> Effect Boolean
push r = pushImpl (unwrap r)

foreign import pushImpl
  :: forall chunktype r
   . S.Readable r -> chunktype -> Effect Boolean

pushStringWithEncoding
  :: forall r
   . Readable String r -> String -> Encoding -> Effect Boolean
pushStringWithEncoding rs s = pushStringWithEncodingImpl (unwrap rs) s <<< show

foreign import pushStringWithEncodingImpl
  :: forall r
   . S.Readable r -> String -> String -> Effect Boolean

pushEnd
  :: forall chunktype r
   . Readable chunktype r -> Effect Unit
pushEnd = pushEndImpl <<< unwrap

foreign import pushEndImpl
  :: forall r
   . S.Readable r -> Effect Unit

pipe
  :: forall chunktype w r
   . Readable chunktype w -> S.Writable r -> Effect (S.Writable r)
pipe r = S.pipe $ unwrap r
