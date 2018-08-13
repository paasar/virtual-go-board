module Game where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Halogen.Query.EventSource as ES
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP

import Board as Board
import CaptureZones as CaptureZones
import Cell as Cell
import ModeSelector as ModeSelector

data Query a = ModeChanged ModeSelector.Message a
             | CellClicked Board.Message a
             | Init a
             | HandleKey KeyboardEvent a

type ChildQuery = Coproduct3 ModeSelector.Query CaptureZones.Query Board.Query

type State = { action :: Cell.CellState
             , mode :: ModeSelector.Mode }

type Slot = Either3 Unit Unit Unit


nextAction :: State -> Cell.CellState -> Cell.CellState
nextAction {action, mode} cellState =
  case mode of
    ModeSelector.Static -> action
    _ -> if action == Cell.Black
           then Cell.White
           else Cell.Black

game :: Int -> H.Component HH.HTML Query Unit Void Aff
game size =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
--    , initializer: Just (H.action Init)
--    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { action: Cell.Black
                   , mode: ModeSelector.Alternate }

    render :: State -> H.ParentHTML Query ChildQuery Slot Aff
    render state =
      HH.div_ [ HH.div [ HP.class_ (H.ClassName "modes-and-captures")]
                [ HH.slot' CP.cp1
                         unit
                         ModeSelector.modeSelector
                         state.action
                         (HE.input ModeChanged)
                , HH.slot' CP.cp2 unit CaptureZones.captureZones unit absurd
                ]
              , HH.slot' CP.cp3 unit (Board.board size) state (HE.input CellClicked)
              ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void Aff
    eval = case _ of
      ModeChanged (ModeSelector.Changed modeState) next -> do
        -- TODO when
        H.put $ { action: modeState.nextPiece, mode: modeState.mode}
        pure next
      CellClicked (Cell.Toggled cellState) next -> do
        state <- H.get
        -- TODO when
        H.modify_ (_ {action = nextAction state cellState})
        pure next
      Init next -> do
        document <- H.liftEffect $ Web.document =<< Web.window
{- TODO ?
        H.subscribe
          ES.eventSource
            KET.keyup
            (HTMLDocument.toEventTarget document)
            (map (H.action <<< HandleKey) <<< KE.fromEvent)
-}
        pure next
      HandleKey ev next
        | KE.key ev == "b" -> do
            H.liftEffect $ E.preventDefault (KE.toEvent ev)
            H.put $ { action: Cell.Black, mode: ModeSelector.Static }
            pure next
        | otherwise ->
            pure next
