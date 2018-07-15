module Cell where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data CellState = Empty | Black | White

derive instance genericCellState :: Generic CellState _
instance showCellState :: Show CellState where
  show = genericShow
instance eqCellState :: Eq CellState where
  eq = genericEq

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
      children = if state == Empty
                   then []
                   else [ HH.div
                          [ HP.classes [ (H.ClassName "piece"),
                                         (H.ClassName $ show state)] ]
                          [ HH.text ""]]
    in
      HH.div
        [ HP.class_ (H.ClassName "cell")
        , HE.onClick (HE.input_ Toggle) ]
        children

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
