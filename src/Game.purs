module Game where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath as CP

import Board as Board
import Cell as Cell
import ModeSelector as ModeSelector

data Query a = ModeChanged ModeSelector.Message a
             | CellClicked Board.Message a

type ChildQuery = Coproduct2 ModeSelector.Query Board.Query

type State = { action :: Cell.CellState
             , mode :: ModeSelector.Mode }

type Slot = Either2 Unit Unit


nextAction :: State -> Cell.CellState -> Cell.CellState
nextAction {action, mode} cellState =
  case mode of
    ModeSelector.Static -> action
    _ -> if action == Cell.Black
           then Cell.White
           else Cell.Black

game :: forall m. H.Component HH.HTML Query Unit Void m
game =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { action: Cell.Black
                   , mode: ModeSelector.Alternate }

    render :: State -> H.ParentHTML Query ChildQuery Slot m
    render state =
      HH.div_ [ HH.slot' CP.cp1
                         unit
                         ModeSelector.modeSelector
                         state.action
                         (HE.input ModeChanged)
              , HH.slot' CP.cp2 unit Board.board state (HE.input CellClicked)
              ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void m
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
