module Cell where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

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

nextState :: CellState -> CellState -> CellState
nextState _ Empty = Empty
nextState s s' = if s == s' then Empty else s'

type State = { current :: CellState
             , desired :: CellState }

data Query a
  = Toggle a
  | HandleInput CellState a

type Input = CellState
data Message = Toggled CellState

cell :: forall m. String -> H.Component HH.HTML Query Input Message m
cell orientationClass =
  H.component
    { initialState: const initialState
    , render: (render orientationClass)
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: State
  initialState = { current: Empty
                 , desired: Black }

  render :: String -> State -> H.ComponentHTML Query
  render orientationClassName state =
    let
      piece = if state.current == Empty
                then [ ]
                else [ HH.div
                       [ HP.classes [ (H.ClassName "piece"),
                                      (H.ClassName $ show state.current)] ]
                       [ ]]
    in
      HH.div
        [ HP.classes [ (H.ClassName "cell"),
                       (H.ClassName orientationClassName) ]
        , HE.onClick (HE.input_ Toggle)
        ]
        piece

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    HandleInput desiredState next -> do
      state <- H.get
      when (state.desired /= desiredState) $ H.modify_ (_ { desired = desiredState })
      pure next

    Toggle next -> do
      state <- H.get
      let nxtState = nextState state.current state.desired
      when (nxtState /= state.current) $ H.modify_ (_ { current = nxtState })
      newState <- H.get
      H.raise $ Toggled newState.current
      pure next

