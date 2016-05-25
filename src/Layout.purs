module App.Layout where

import App.Counter as Counter
import App.Routes (Route(Home, Bacon, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

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

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Counter.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App Bacon 2" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Counter.view state.count
        Bacon -> p [] [text "The bacon page"]
        NotFound -> App.NotFound.view state
    ]
