module Board where

import Prelude

import Data.Array (concat, replicate)
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
        [ HP.classes [ (H.ClassName "board"),
                       (H.ClassName $ "x" <> (show size)) ] ]
        (createCells size)
        where
          size = 9 -- TODO dropdown of 9, 13 and 19

    eval :: Query ~> H.ParentDSL State Query Cell.Query Slot Void m
    eval = case _ of
      Clicked (Cell.Toggled _) next -> do
        pure next

createCells :: forall m. Int -> Array (H.ParentHTML Query Cell.Query Slot m)
createCells cellsPerRow = topRow <> middleRows <> bottomRow
  where
    topRow = [(createCell "nw")]
          <> (replicate middleCellsAmount (createCell "n"))
          <> [(createCell "ne")]
    middleRows = concat $ replicate middleCellsAmount middleRow
    middleRow = [(createCell "w")]
             <> (replicate middleCellsAmount (createCell "m"))
             <> [(createCell "e")]
    bottomRow = [(createCell "sw")]
             <> (replicate middleCellsAmount (createCell "s"))
             <> [(createCell "se")]
    middleCellsAmount = (cellsPerRow - 2)

createCell :: forall m. String -> H.ParentHTML Query Cell.Query Slot m
createCell orientationClass =
  HH.slot CellSlot (Cell.cell orientationClass) unit (HE.input Clicked)
