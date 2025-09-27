module Elmish.TimeMachine
  ( Expanded
  , Keybindings
  , Message
  , withTimeMachine
  , withTimeMachine'
  )
  where

import Prelude

import Data.Foldable (fold, for_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Debug as Debug
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Elmish (ComponentDef', ReactElement, forks, lmap, subscribe, (<|))
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Elmish.HTML.Styled as H
import Elmish.Subscription (Subscription(..))
import Elmish.TimeMachine.History (History, formatMessage, formatState)
import Elmish.TimeMachine.History as History
import Web.DOM (Element)
import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.NonElementParentNode (getElementById) as W
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as W
import Web.HTML.HTMLDocument (body, toDocument, toNonElementParentNode) as W
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as W
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

data Message msg
  = Message msg
  -- Controls
  | Undo
  | Redo
  | Jump Int
  | TogglePaused
  -- UI
  | ToggleExpanded
  | ToggleSection Section
  -- Keydown
  | Keydown KeyboardEvent

type State msg s =
  { history :: History msg s
  , paused :: Boolean
  , visible :: Boolean
  , expanded :: Expanded
  , keybindings :: Keybindings
  }

data Expanded
  = Expanded (Set Section)
  | Collapsed

data Section
  = Past
  | Present
  | Future
derive instance Eq Section
derive instance Ord Section

-- | A record of keybindings, each represented by a predicate function which
-- | returns true iff a `KeyboardEvent` should trigger the given command
type Keybindings =
  { toggle :: KeyboardEvent -> Boolean
  }

-- | Wraps a `ComponentDef` to add a "time machine" debug tool to the Elmish UI
withTimeMachine ::
  forall m msg state
  . Debug.DebugWarning
  => MonadEffect m
  => Functor m
  => ComponentDef' m msg state
  -> ComponentDef' m (Message msg) (State msg state)
withTimeMachine =
  withTimeMachine'
    { toggle: \e ->
        (KeyboardEvent.ctrlKey e || KeyboardEvent.metaKey e) &&
        KeyboardEvent.altKey e &&
        KeyboardEvent.code e == "KeyZ"
    }

-- | A version of `withTimeMachine` that allows configuring the keybinding for
-- | showing/hiding the time machine
withTimeMachine' ::
  forall m msg state
  . Debug.DebugWarning
  => MonadEffect m
  => Functor m
  => Keybindings
  -> ComponentDef' m msg state
  -> ComponentDef' m (Message msg) (State msg state)
withTimeMachine' keybindings def = { init, update, view }
  where
    init = do
      subscribe Keydown keydownSub
      state <- def.init # lmap Message
      pure
        { history: History.init state
        , paused: false
        , visible: true
        , expanded: Collapsed
        , keybindings
        }

    update state = case _ of
      Message msg -> do
        next <- def.update (History.presentState state.history) msg # lmap Message
        let track = if state.paused then History.stash else History.track
        pure state { history = track state.history msg next }
      Undo ->
        pure state { history = History.undo state.history }
      Redo ->
        pure state { history = History.redo state.history }
      Jump index ->
        pure state { history = History.jump index state.history }
      TogglePaused ->
        pure state
          { history = History.live state.history
          , paused = not state.paused
          }
      ToggleExpanded ->
        pure state { expanded = toggle state.expanded }
      ToggleSection section | Expanded sections <- state.expanded ->
        pure state { expanded = Expanded $ Set.toggle section sections }
      ToggleSection _ ->
        pure state
      Keydown e | state.keybindings.toggle e ->
        pure state { visible = not state.visible }
      Keydown _ ->
        pure state
      where
        toggle = case _ of
          Expanded _ -> Collapsed
          Collapsed -> Expanded $ Set.singleton Present

    view { history, paused, visible, expanded } dispatch =
      H.fragment
      [ def.view (History.presentState history) $ dispatch <<< Message
      , guard visible $
          portal
            { id: "tardis-time-machine"
            , content:
                H.div_ "etm-container" { tabIndex: -1 }
                [ header
                , body
                ]
            }
      , stylesheet
      ]
      where
        header =
          H.div "etm-header"
          [ whenCollapsed undoButton
          , H.code "etm-code" $
              formatMessage false $
                History.latestMessage history
          , whenCollapsed redoButton
          , H.button_ "etm-btn etm-ml-auto"
              { onClick: dispatch <| ToggleExpanded
              , disabled: false
              }
              case expanded of
                Expanded _ -> "▼"
                Collapsed -> "▶"
          ]

        body = whenExpanded \sections ->
          H.div "etm-body"
          [ controls
          , section
              { section: Past, expanded: sections, bodyClass: "" } $
              historyEvent <$> History.past history
          , section
              { section: Present, expanded: sections, bodyClass: "etm-px-sm" }
              [ H.h6 "" "Last Message"
              , H.pre "etm-code-block" $
                  formatMessage true $ History.latestMessage history
              , H.h6 "" "Current State"
              , H.pre "etm-code-block" $
                  formatState $ History.presentState history
              ]
          , section
              { section: Future, expanded: sections, bodyClass: "" } $
              historyEvent <$> History.future history
          ]

        historyEvent { index, message } =
          H.pre_ "etm-history-event"
            { onClick: dispatch <| Jump index } $
            formatMessage true message

        section props content =
          H.div "etm-section"
          [ H.h6_ "etm-section-header"
            { onClick: dispatch <| ToggleSection props.section }
            [ H.div "" case props.section of
                Past -> "Past"
                Present -> "Present"
                Future -> "Future"
            , H.div ""
                if Set.member props.section props.expanded then "▼" else "▶"
            ]
          , guard (Set.member props.section props.expanded) $
              H.div ("etm-section-body " <> props.bodyClass) content
          ]

        controls =
          H.div "etm-section etm-d-flex"
          [ undoButton
          , redoButton
            -- Rewind
          , H.button_ "etm-btn etm-ml-auto"
              { onClick: dispatch <| TogglePaused }
              if paused then "▶️" else "⏸️"
            -- Fast forward
          ]

        undoButton =
          H.button_ "etm-btn"
            { onClick: dispatch <| Undo
            , disabled: not History.hasPast history
            }
            "↩️"

        redoButton =
          H.button_ "etm-btn"
            { onClick: dispatch <| Redo
            , disabled: not History.hasFuture history
            }
            "↪️"

        whenExpanded f =
          case expanded of
            Expanded sections -> f sections
            Collapsed -> H.empty

        whenCollapsed content =
          case expanded of
            Collapsed -> content
            Expanded _ -> H.empty

    keydownSub = Subscription \dispatch -> liftEffect do
      listener <- eventListener \e -> case KeyboardEvent.fromEvent e of
        Just ke -> dispatch ke
        _ -> pure unit

      W.window <#> toEventTarget >>= addEventListener keydown listener false

      pure $
         liftEffect $ W.window <#> toEventTarget >>= removeEventListener keydown listener false

portal :: { id :: String, content :: ReactElement } -> ReactElement
portal = wrapWithLocalState (ComponentName "Portal") \{ id, content } ->
  { init: do
      forks \{ dispatch } -> liftEffect do
        mContainer <- elementById id
        case mContainer of
          Just container ->
            dispatch container
          Nothing -> do
            doc <- W.document =<< W.window
            mBody <- W.body doc
            for_ mBody \b -> do
              container <- DOM.createElement "div" $ W.toDocument doc
              Element.setId id container
              DOM.appendChild (Element.toNode container) (HTMLElement.toNode b)
              dispatch container
      pure Nothing
  , update: \_ container -> pure $ Just container
  , view: \container _ -> fold $
      createPortal content <$> container
  }
  where
    elementById :: String -> Effect (Maybe Element)
    elementById id =
      W.window
      >>= W.document
      <#> W.toNonElementParentNode
      >>= W.getElementById id

createPortal :: ReactElement -> Element -> ReactElement
createPortal = runFn2 createPortal_

foreign import createPortal_ :: Fn2 ReactElement Element ReactElement

stylesheet :: ReactElement
stylesheet = H.style ""
  """
    div.etm-container {
      position: fixed !important;
      bottom: 1rem !important;
      right: 1rem !important;
      border: 1px solid lightgray !important;
      border-radius: 0.5rem !important;
      background-color: white !important;
      width: 300px !important;
    }

    div.etm-header {
      display: flex !important;
      align-items: center !important;
      padding: 0.75rem !important;
    }

    div.etm-body {
      padding: 0.75rem 0 !important;
      border-top: 1px solid lightgray !important;
      max-height: 500px !important;
      overflow: auto !important;
    }

    button.etm-btn {
      display: inline-block !important;
      text-align: center !important;
      text-decoration: none !important;
      vertical-align: middle !important;
      cursor: pointer !important;
      background-color: transparent !important;
      border: none !important;
      padding: 0.375rem 0.75rem !important;
      font-size: 1rem !important;
      border-radius: 0.25rem !important;
    }

    button.etm-btn[disabled] {
      cursor: default !important;
    }

    .etm-ml-auto {
      margin-left: auto !important;
    }

    pre.etm-history-event {
      cursor: pointer !important;
      overflow: hidden !important;
      word-wrap: nowrap !important;
      text-overflow: ellipsis !important;
      margin: 0 !important;
      padding: 0.5rem 0.75rem !important;
    }

    pre.etm-history-event:hover {
      background-color: #e6f5ff;
    }

    div.etm-section {
      border-bottom: 1px solid lightgray !important;
    }

    div.etm-section:last-child {
      border-bottom: none !important;
    }

    h6.etm-section-header {
      display: flex !important;
      align-items: center !important;
      justify-content: space-between !important;
      margin-bottom: 0 !important;
      cursor: pointer !important;
      padding: 0.75rem !important;
    }

    .etm-px-sm {
      padding: 0 0.75rem !important;
    }

    pre.etm-code-block {
      background-color: #efefef !important;
      padding: 0.75rem 0.5rem !important;
      border: 1px solid lightgray !important;
      border-radius: 0.25rem !important;
    }

    code.etm-code {
      color: #656565 !important;
    }

    .etm-d-flex {
      display: flex !important;
    }
  """
