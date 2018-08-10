module CaptureZones where

import Prelude

import Cell as Cell
import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { blackCaptures :: Int
             , whiteCaptures :: Int }

data Query a = Increase Cell.CellState a
             | Decrease Cell.CellState a

captureZones :: forall m. H.Component HH.HTML Query Unit Void m
captureZones =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { blackCaptures: 0
                   , whiteCaptures: 0 }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div [ HP.class_ (H.ClassName "capture-zones") ]
        [ createCaptureZone Cell.Black state.blackCaptures
        , createCaptureZone Cell.White state.whiteCaptures
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Increase color next -> do
        state <- H.get
        when (color == Cell.Black) $ H.modify_ (_ { blackCaptures = state.blackCaptures + 1})
        when (color == Cell.White) $ H.modify_ (_ { whiteCaptures = state.whiteCaptures + 1})
        pure next
      Decrease color next -> do
        state <- H.get
        let valueCandidate = if (color == Cell.Black)
                               then state.blackCaptures - 1
                               else state.whiteCaptures - 1
        let newValue = if valueCandidate >= 0
                         then valueCandidate
                         else 0
        when (color == Cell.Black) $ H.modify_ (_ { blackCaptures = newValue })
        when (color == Cell.White) $ H.modify_ (_ { whiteCaptures = newValue })
        pure next

createCaptureZone :: Cell.CellState -> Int -> H.ComponentHTML Query
createCaptureZone color captures =
  HH.div
    [ HP.classes [ (H.ClassName "capture-zone") ]]
    [ HH.div [ HP.classes [ (H.ClassName "capture-cell")
                          , (H.ClassName $ show color)]
             , HE.onClick (HE.input_ (Increase color))
             ]
      [ HH.div [ HP.classes [ (H.ClassName "piece")
                            , (H.ClassName $ show color)]
                            ]
       [ HH.text $ show captures ]]
    , HH.div [ HP.class_ (H.ClassName "free-cell")
             , HE.onClick (HE.input_ (Decrease color))
             ]
       [ HH.text "x" ]
    ]
