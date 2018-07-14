module Board where

import Prelude

import Data.Array (replicate)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Cell as Cell

data Query a
  = Clicked Cell.Message a

type State = Int

data Slot = CellSlot
derive instance eqCellSlot :: Eq Slot
derive instance ordCellSlot :: Ord Slot

board :: forall m. H.Component HH.HTML Query Unit Void m
board =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = 0

    render :: State -> H.ParentHTML Query Cell.Query Slot m
    render state =
      HH.div
        [ HP.class_ (H.ClassName "board") ]
        (replicate (9 * 9) $ HH.slot CellSlot Cell.cell unit (HE.input Clicked))

    eval :: Query ~> H.ParentDSL State Query Cell.Query Slot Void m
    eval = case _ of
      Clicked (Cell.Toggled _) next -> do
        pure next

