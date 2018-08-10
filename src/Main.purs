module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix)

import Effect (Effect)
import Effect.Console (log)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window as Window

import Game (game)

main :: Effect Unit
main = do
  log "Hello Go player!"

  params <- getParams
  let size = case params of
               "19" -> 19
               "13" -> 13
               _    -> 9

  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (game size) unit body

-- A bit modified version of Routing.Hash/getHash
getParams :: Effect String
getParams = window
            >>= Window.location
            >>= L.search
            >>> map (stripPrefix (Pattern "?")
            >>> fromMaybe "")
