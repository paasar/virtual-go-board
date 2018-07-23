module Board where

import Prelude

import Data.Array (concat, length, range, replicate, zipWith)
import Data.Maybe (Maybe(Nothing))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Cell as Cell

data Query a
  = Clicked Cell.Message a

type State = Int

data Slot = CellSlot Int
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
        [ HP.classes [ (H.ClassName "board")
                     , (H.ClassName $ "x" <> (show size)) ] ]
        (createCells size)
        where
          -- TODO dropdown of 9, 13 and 19
          size = 9

    eval :: Query ~> H.ParentDSL State Query Cell.Query Slot Void m
    eval = case _ of
      Clicked (Cell.Toggled _) next -> do
        pure next

createCells :: forall m. Int -> Array (H.ParentHTML Query Cell.Query Slot m)
createCells cellsPerRow = createSlots $ topRow <> middleRows <> bottomRow
  where
    topRow = [(Cell.cell "nw")]
          <> (replicate middleCellsAmount (Cell.cell "n"))
          <> [(Cell.cell "ne")]
    middleRows = concat $ replicate middleCellsAmount middleRow
    middleRow = [(Cell.cell "w")]
             <> (replicate middleCellsAmount (Cell.cell "m"))
             <> [(Cell.cell "e")]
    bottomRow = [(Cell.cell "sw")]
             <> (replicate middleCellsAmount (Cell.cell "s"))
             <> [(Cell.cell "se")]
    middleCellsAmount = (cellsPerRow - 2)
    createSlots cells = zipWith createSlot cells (range 1 (length cells))

createSlot :: forall m. (H.Component HH.HTML Cell.Query Cell.Input Cell.Message m)
                     -> Int
                     -> H.ParentHTML Query Cell.Query Slot m
createSlot cell slotNumber = HH.slot (CellSlot slotNumber) cell unit (HE.input Clicked)
