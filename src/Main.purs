module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Cell (cell)

main :: Effect Unit
main = do
  log "Hello Go player!"
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI cell unit body
