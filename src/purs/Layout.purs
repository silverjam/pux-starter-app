module App.Layout where

import Control.Monad.Eff.Console (CONSOLE)
import App.Counter as Counter
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux (EffModel, noEffects)
import Pux.Html (Html, (#), bind, forwardTo, div, h1, p, text)
import Signal.Channel (CHANNEL)

data Action
  = Child (Counter.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Counter.State }

init :: State
init =
  { route: NotFound
  , count: Counter.init }

update :: forall eff. Action -> State -> EffModel State Action (console :: CONSOLE | eff)
update (PageView route) state = noEffects $ state { route = route }
update (Child action) state =
  { state: state { count = counter.state }
  , effects: map (map Child) counter.effects
  }
  where counter = Counter.update action state.count

view :: State -> Html Action
view state =
  div # do
    h1 # text "Pux Starter App"
    p # text "Change me in src/purs/Layout.purs and watch me 'vanilla' hot-reload."
    case state.route of
      Home -> forwardTo Child $ Counter.view state.count
      NotFound -> App.NotFound.view state
