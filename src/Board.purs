module Board where

import Prelude

import Data.Array (concat, length, range, replicate, zipWith)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Cell as Cell
import ModeSelector as ModeSelector

type Input = { action :: Cell.CellState
             , mode :: ModeSelector.Mode}

data Query a
  = CellClicked Cell.Message a
  | HandleInput Input a

type State = { action :: Cell.CellState
             , mode :: ModeSelector.Mode }

data Slot = CellSlot Int
derive instance eqCellSlot :: Eq Slot
derive instance ordCellSlot :: Ord Slot

type Message = Cell.Message

board :: forall m. Int -> H.Component HH.HTML Query Input Message m
board size =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
    initialState :: State
    initialState = { action: Cell.Black
                   , mode: ModeSelector.Static }

    render :: State -> H.ParentHTML Query Cell.Query Slot m
    render state =
      HH.div
        [ HP.classes [ (H.ClassName "board")
                     , (H.ClassName $ "x" <> (show size)) ] ]
        (createCells state size)

    eval :: Query ~> H.ParentDSL State Query Cell.Query Slot Message m
    eval = case _ of
      CellClicked message next -> do
        H.raise $ message
        pure next
      HandleInput gameState next -> do
        state <- H.get
        when (gameState /= state) $ H.put $ gameState
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
