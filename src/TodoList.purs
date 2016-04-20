module App.TodoList where

import App.Effects (AppEffects)
import App.NewTodo as NewTodo
import App.TodoItem as TodoItem
import Data.Array as Array
import Data.StrMap as StrMap
import Data.Foldable (fold)
import Data.Maybe (Maybe(Just))
import Prelude (map, show)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.Html (div, Html, h1, header, input, label, ul, section, text)
import Pux.Html.Attributes (className, htmlFor, type_)

data Action = ItemAction String TodoItem.Action | NewTodoAction NewTodo.Action

type State =
  { todos :: StrMap.StrMap TodoItem.State
  , newTodo :: NewTodo.State
  }

init :: State
init =
  { todos: StrMap.empty
  , newTodo: NewTodo.init
  }

update :: Action -> State -> EffModel State Action AppEffects
update (ItemAction id TodoItem.Remove) state =
  noEffects (state { todos = StrMap.delete id state.todos })
update (ItemAction id action) state =
  noEffects (state { todos = StrMap.update (\todo -> Just (TodoItem.update action todo)) id state.todos })
update (NewTodoAction (NewTodo.Add item)) state =
  noEffects (state { todos = StrMap.insert (show item.todo.id) item state.todos })
update (NewTodoAction action) state =
  mapEffects NewTodoAction
    (mapState (\s -> state { newTodo = s }) (NewTodo.update action state.newTodo))

view :: State -> Html Action
view state =
  div
    []
    [ header
        [ className "header" ]
        [ h1 [] [ text "todos" ]
        , map NewTodoAction (NewTodo.view state.newTodo)
        ]
    , section
        [ className "main" ]
        [ input [ className "toggle-all", type_ "checkbox" ] []
        , label [ htmlFor "toggle-all" ] [ text "Mark all as complete" ]
        , ul
            [ className "todo-list" ]
            (StrMap.fold (\arr key item -> Array.snoc arr (map (ItemAction key) (TodoItem.view item))) [] state.todos)
        ]
    ]
