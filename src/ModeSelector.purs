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

data Query a = SetMode Mode Cell.CellState a
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
                   , mode: Alternate }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div [ HP.classes [ (H.ClassName "mode-selector")
                          , (H.ClassName $ show state.nextPiece)
                          , (H.ClassName $ show state.mode)]
             ]
             [ HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.Black) ]
                      , HE.onClick (HE.input_ (SetMode Static Cell.Black)) ]
                      [ HH.div [ HP.classes [ (H.ClassName "piece")
                                            , (H.ClassName $ show Cell.Black)]]
                               []]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName "Alternate") ]
                      , HE.onClick (HE.input_ (SetMode Alternate state.nextPiece)) ]
                      [ HH.div [ HP.classes [ (H.ClassName "piece")
                                            , (H.ClassName $ show Cell.Black) ]]
                               []
                      , HH.div [ HP.classes [ (H.ClassName "piece")
                                            , (H.ClassName $ show Cell.White) ]]
                               []]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.White) ]
                      , HE.onClick (HE.input_ (SetMode Static Cell.White)) ]
                      [ HH.div [ HP.classes [ (H.ClassName "piece")
                                            , (H.ClassName $ show Cell.White)]]
                               []]
             , HH.div [ HP.classes [ (H.ClassName "mode")
                                   , (H.ClassName $ show Cell.Empty) ]
                      , HE.onClick (HE.input_ (SetMode Static Cell.Empty)) ]
                      [ HH.div [ HP.class_ (H.ClassName "remove") ]
                               [ HH.text "X" ]]
             ]
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      HandleInput piece next -> do
        H.modify_ (\st -> st { nextPiece = piece })
        pure next
      SetMode newMode newNextPiece next -> do
        state <- H.get
        when (newMode /= state.mode) $ H.modify_ (_ {mode = newMode})
        when (newNextPiece /= state.nextPiece) $ H.modify_ (_ {nextPiece = newNextPiece})
        when (newMode == Alternate && state.mode == Alternate) $
          H.modify_ (_ {nextPiece = if newNextPiece == Cell.Black
                                      then Cell.White
                                      else Cell.Black})
        when (newMode == Alternate && newNextPiece == Cell.Empty) $
          H.modify_ (_ {nextPiece = Cell.Black})
        newState <- H.get
        H.raise $ Changed newState
        pure next
