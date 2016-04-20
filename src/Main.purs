module Main where

import App (Action, State, view, update)
import App.Effects (AppEffects)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Prelude (bind, return)
import Pux (App, Config, CoreEffects, renderToDOM)

-- | App configuration
config :: State -> Eff (CoreEffects AppEffects) (Config State Action AppEffects)
config state = do
  return
    { initialState: state
    , update: update
    , view: view
    , inputs: [] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- Pux.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app
