module Main where

import App.Events (Event(..), foldp)
import App.Routes (match)
import App.State (State, init)
import App.View.Layout (view)
import Web.HTML (window)
import Pux (App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Effect (Effect)
import Control.Bind ((=<<), bind, discard)
import Control.Applicative (pure)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

main :: String -> State -> Effect WebApp
main url state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [routeSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
