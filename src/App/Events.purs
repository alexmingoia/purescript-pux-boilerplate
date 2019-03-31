module App.Events where

import Prelude
import App.Routes (Route)
import App.State (State(..))
import Pux (EffModel, noEffects)

data Event = PageView Route

foldp :: Event -> State -> EffModel State Event
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
