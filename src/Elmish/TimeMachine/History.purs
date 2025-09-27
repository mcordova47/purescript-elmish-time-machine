module Elmish.TimeMachine.History
  ( Event
  , History
  , IndexedEvent
  , Message
  , formatMessage
  , formatState
  , future
  , hasFuture
  , hasPast
  , init
  , jump
  , latestMessage
  , past
  , play
  , present
  , presentState
  , redo
  , stash
  , stop
  , track
  , undo
  )
  where

import Prelude

import Control.Monad.Rec.Class (Step(..), loop2, tailRec2)
import Data.Array as Array
import Data.Function.Uncurried (Fn2, Fn1, runFn1, runFn2)
import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

-- | A zipper type that allows easy traversal through pairs of states and
-- | messages in an Elmish UI
newtype History msg s = History
  { past :: List (Event msg s)
  , present :: (Event msg s)
  , future :: List (Event msg s)
  }

-- | Initial states have no associated previous message, so it helps to have a
-- | type that wraps either an "initial state" message or the latest message
-- | which led to the current state
data Message msg
  = Message msg
  | Init

-- | Represents a pairing of a given state with the message that led to it
type Event msg s = Message msg /\ s

-- | A record representing a given state, its corresponding message, along with
-- | its distance from the present (in "steps" where past events are negative
-- | integers and future events are positive)
type IndexedEvent msg s =
  { index :: Int
  , message :: Message msg
  , state :: s
  }

-- Constructors

-- | Initializes the History type with a current state
init :: forall msg s. s -> History msg s
init s = History
  { past: Nil
  , present: Init /\ s
  , future: Nil
  }

-- Accessors

-- | Gets the current state paired with the latest message
present :: forall msg s. History msg s -> Event msg s
present (History h) = h.present

-- | Gets the current state
presentState :: forall msg s. History msg s -> s
presentState = snd <<< present

-- | Gets the latest message
latestMessage :: forall msg s. History msg s -> Message msg
latestMessage = fst <<< present

-- | Gets an array of the past state/message pairs, along with its index (or
-- | distance from the present), ordered chronologically
past :: forall msg s. History msg s -> Array (IndexedEvent msg s)
past (History h) = indexedEvents ((+) 1 >>> negate) h.past # Array.reverse

-- | Gets an array of the future state/message pairs, along with its index (or
-- | distance from the present), ordered chronologically
future :: forall msg s. History msg s -> Array (IndexedEvent msg s)
future (History h) = indexedEvents ((+) 1) h.future

indexedEvents :: forall msg s. (Int -> Int) -> List (Event msg s) -> Array (IndexedEvent msg s)
indexedEvents indexBy =
  Array.fromFoldable >>>
  Array.mapWithIndex \index (message /\ state) ->
    { index: indexBy index, message, state }

-- | Returns true iff there is a nonempty past
hasPast :: forall msg s. History msg s -> Boolean
hasPast (History h) = not List.null h.past

-- | Returns true iff there is a nonempty future
hasFuture :: forall msg s. History msg s -> Boolean
hasFuture (History h) = not List.null h.future

-- Controls

-- | Steps back one event into the past
-- | No-op if there is no past
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

-- | Steps forward one event into the future
-- | No-op if there is no future
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

-- | Adds a new event to the history, emptying out the future events
track :: forall msg s. History msg s -> msg -> s -> History msg s
track (History h) msg next = History
  { past: h.present : h.past
  , present: Message msg /\ next
  , future: Nil
  }

-- | For the case where the history is paused, stash changes in the future
stash :: forall msg s. History msg s -> msg -> s -> History msg s
stash (History h) msg next = History h
  { future = List.snoc h.future (Message msg /\ next) }

-- | Jumps the given distance, i.e. by a certain number of steps forward or
-- | backwards
jump :: forall msg s. Int -> History msg s -> History msg s
jump = tailRec2 go
  where
    go distance history
      | distance > 0
      , hasFuture history =
        loop2 (distance - 1) $ redo history
      | distance < 0
      , hasPast history =
        loop2 (distance + 1) $ undo history
      | otherwise =
        Done history

-- | Go to the end state of the history
play :: forall msg s. History msg s -> History msg s
play history
  | not hasFuture history = history
  | otherwise = play $ redo history

-- | Clears the future state, for when the stop button is pressed
stop :: forall msg s. History msg s -> History msg s
stop (History h) = History h
  { future = Nil }

-- Display

-- | Formats a message (using its JS representation)
-- |
-- | E.g. `SetFoo 1` might look like `SetFoo2 {"value0": 1}`
formatMessage :: forall msg. Boolean -> Message msg -> String
formatMessage full = case _ of
  Init -> "Initial State"
  Message msg -> runFn2 formatMessage_ full msg

foreign import formatMessage_ :: forall a. Fn2 Boolean a String

-- | Formats a given state by `JSON.stringify`ing it
formatState :: forall a. a -> String
formatState = runFn1 formatState_

foreign import formatState_ :: forall a. Fn1 a String
