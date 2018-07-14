module Cell where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP

data CellState = Empty | Black | White

derive instance genericCellState :: Generic CellState _

instance showCellState :: Show CellState where
  show = genericShow

nextState :: CellState -> CellState
nextState Empty = Black
nextState Black = White
nextState White = Empty

type State = CellState

data Query a
  = Toggle a
  | Content (CellState -> a)

type Input = Unit

data Message = Toggled CellState

cell :: forall m. H.Component HH.HTML Query Input Message m
cell =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Empty

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = show state
    in
      HH.div
        [ HE.onClick (HE.input_ Toggle) ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nxtState = nextState state
      H.put nxtState
      H.raise $ Toggled nxtState
      pure next
    Content reply -> do
      state <- H.get
      pure (reply state)
