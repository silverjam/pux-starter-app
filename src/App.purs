module App where

import App.Effects (AppEffects)
import App.TodoList as TodoList
import Prelude (map)
import Pux (EffModel, mapEffects)
import Pux.Html (Html, a, div, footer, p, section, text, span)
import Pux.Html.Attributes (className, href)

data Action = ListAction TodoList.Action

type State = TodoList.State

init :: State
init = TodoList.init

update :: Action -> State -> EffModel State Action AppEffects
update (ListAction action) state =
  mapEffects ListAction (TodoList.update action state)

view :: State -> Html Action
view state =
  div
    []
    [ section
      [ className "todoapp" ]
      [ map ListAction (TodoList.view state) ]
    , footer
        [ className "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ span [] [ text "Template by " ]
            , a [ href "http://sindresorhus.com" ] [ text "Sindre Sorhus" ]
            ]
        , p [] [ text "Created by Alex Mingoia" ]
        , p []
            [ span [] [ text "Part of " ]
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
    ]
