module ModeSelector where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Cell as Cell

data Mode = Static | Alternate
derive instance genericMode :: Generic Mode _
instance showMode :: Show Mode where
  show = genericShow
instance eqMode :: Eq Mode where
  eq = genericEq

type State = { nextPiece :: Cell.CellState
             , mode :: Mode
             }

data Query a = ModeChanged a
             | HandleInput Cell.CellState a

type Input = Cell.CellState

data Message = Changed State

modeSelector :: forall m. H.Component HH.HTML Query Input Message m
modeSelector =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

    initialState :: State
    initialState = { nextPiece: Cell.Black
                   , mode: Static }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div [ HP.classes [ (H.ClassName "mode-selector")
                          , (H.ClassName $ show state.nextPiece)
                          , (H.ClassName $ show state.mode)]]
             [ HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.Black) ]]
                      [ HH.text "Black" ]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName "Alternate") ]]
                      [ HH.text "Alternate" ]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.White) ]]
                      [ HH.text "White" ]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.Empty) ]]
                      [ HH.text "Remove" ]]
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      HandleInput piece next -> do
        H.modify_ (\st -> st { nextPiece = piece })
        pure next
      ModeChanged next -> do
        -- TODO
        pure next
