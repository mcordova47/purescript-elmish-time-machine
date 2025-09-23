module TimeTravel.History
  ( History
  , Message
  , formatMessage
  , formatState
  , future
  , hasFuture
  , hasPast
  , init
  , jump
  , past
  , present
  , presentMessage
  , presentState
  , redo
  , track
  , undo
  )
  where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, Fn1, runFn1, runFn2)
import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

newtype History msg s = History
  { past :: List (Message msg /\ s)
  , present :: Message msg /\ s
  , future :: List (Message msg /\ s)
  }

data Message msg
  = Message msg
  | Init

type IndexedEvent msg s =
  { index :: Int
  , message :: Message msg
  , state :: s
  }

-- Constructors

init :: forall msg s. s -> History msg s
init s = History
  { past: Nil
  , present: Init /\ s
  , future: Nil
  }

-- Accessors

present :: forall msg s. History msg s -> Message msg /\ s
present (History h) = h.present

presentState :: forall msg s. History msg s -> s
presentState = snd <<< present

presentMessage :: forall msg s. History msg s -> Message msg
presentMessage = fst <<< present

past :: forall msg s. History msg s -> Array (IndexedEvent msg s)
past (History h) = toArray ((+) 1 >>> negate) h.past # Array.reverse

future :: forall msg s. History msg s -> Array (IndexedEvent msg s)
future (History h) = toArray ((+) 1) h.future

toArray :: forall msg s. (Int -> Int) -> List (Message msg /\ s) -> Array (IndexedEvent msg s)
toArray indexBy =
  Array.fromFoldable >>>
  Array.mapWithIndex \index (message /\ state) ->
    { index: indexBy index, message, state }

hasPast :: forall msg s. History msg s -> Boolean
hasPast (History h) = not List.null h.past

hasFuture :: forall msg s. History msg s -> Boolean
hasFuture (History h) = not List.null h.future

-- Controls

undo :: forall msg s. History msg s -> History msg s
undo (History h) = case h.past of
  present' : past' ->
    History
      { past: past'
      , present: present'
      , future: h.present : h.future
      }
  Nil ->
    History h

redo :: forall msg s. History msg s -> History msg s
redo (History h) = case h.future of
  present' : future' ->
    History
      { past: h.present : h.past
      , present: present'
      , future: future'
      }
  Nil ->
    History h

track :: forall msg s. History msg s -> msg -> s -> History msg s
track (History h) msg next = History
  { past: h.present : h.past
  , present: Message msg /\ next
  , future: Nil
  }

jump :: forall msg s. Int -> History msg s -> History msg s
jump index history
  | index > 0
  , hasFuture history =
    jump (index - 1) $ redo history
  | index < 0
  , hasPast history =
    jump (index + 1) $ undo history
  | otherwise =
    history

-- Display

formatMessage :: forall msg. Boolean -> Message msg -> String
formatMessage full = case _ of
  Init -> "Initial State"
  Message msg -> runFn2 formatMessage_ full msg

foreign import formatMessage_ :: forall a. Fn2 Boolean a String

formatState :: forall a. a -> String
formatState = runFn1 formatState_

foreign import formatState_ :: forall a. Fn1 a String
