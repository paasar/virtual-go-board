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
  = CellClicked Cell.Message a
  | Switch a

data Mode = Static | Alternate

type State = { action :: Cell.CellState
             , mode :: Mode }

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
    initialState = { action: Cell.Black
                   , mode: Static }

    render :: State -> H.ParentHTML Query Cell.Query Slot m
    render state =
      HH.div_
      [
        HH.button [ HE.onClick (HE.input_ Switch)]
                  [ HH.text $ show state.action ]
      , HH.div
          [ HP.classes [ (H.ClassName "board")
                       , (H.ClassName $ "x" <> (show size)) ] ]
          (createCells state size)
      ]
      where
        -- TODO dropdown of 9, 13 and 19
        size = 9

    eval :: Query ~> H.ParentDSL State Query Cell.Query Slot Void m
    eval = case _ of
      CellClicked (Cell.Toggled _) next -> do
        state <- H.get
        H.put state { action = if state.action == Cell.Black then Cell.White else Cell.Black }
        pure next
      Switch next -> do
        state <- H.get
        H.put state { action = if state.action == Cell.Black then Cell.White else Cell.Black }
        pure next

createCells :: forall m. State -> Int -> Array (H.ParentHTML Query Cell.Query Slot m)
createCells state cellsPerRow = createSlots $ topRow <> middleRows <> bottomRow
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
    createSlots cells = zipWith (createSlot state) cells (range 1 (length cells))

createSlot :: forall m. State
                     -> (H.Component HH.HTML Cell.Query Cell.Input Cell.Message m)
                     -> Int
                     -> H.ParentHTML Query Cell.Query Slot m
createSlot state cell slotNumber = HH.slot
                               (CellSlot slotNumber)
                               cell
                               state.action
                               (HE.input CellClicked)
