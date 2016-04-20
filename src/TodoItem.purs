module App.TodoItem where

import Data.UUID (UUID)
import Prelude (($), (++), (==), const, not, show)
import Pux.Html (button, div, Html, input, label, li, text)
import Pux.Html.Attributes (defaultChecked, checked, className, key, type_, value)
import Pux.Html.Events (onChange, onClick, onDoubleClick, onKeyUp)

data Action
  = ToggleCompleted
  | ToggleEdit
  | ChangeText String
  | Save String
  | Cancel
  | Remove

type Todo =
  { id :: UUID
  , text :: String
  }

type State =
  { editing :: Boolean
  , completed :: Boolean
  , newText :: String
  , todo :: Todo
  }

init :: UUID -> String -> State
init id text =
  { editing: false
  , completed: false
  , newText: text
  , todo:
      { id: id
      , text: text
      }
  }

update :: Action -> State -> State
update ToggleCompleted state =
  state { completed = not state.completed }
update ToggleEdit state =
  state { editing = not state.editing }
update (ChangeText text) state =
  state { newText = text }
update (Save newText) state =
  state { todo = state.todo { text = newText }, editing = false }
update Cancel state =
  state { newText = state.todo.text, editing = false }
update Remove state = state

itemClass editing completed =
  if editing then "editing" else ""
  ++ " " ++
  if completed then "completed" else ""

view :: State -> Html Action
view state =
  li
    [ className (itemClass state.editing state.completed) ]
    [ div
      [ className "view" ]
      [ input
        [ className "toggle"
        , type_ "checkbox"
        , checked state.completed
        , value "completed"
        , onClick (const ToggleCompleted)
        ]
        []
      , label [ onDoubleClick (const ToggleEdit) ] [ text state.todo.text ]
      , button
          [ className "destroy"
          , onClick (const Remove)
          ]
          []
      ]
    , input
        [ className "edit"
        , value state.newText
        , onChange \ev -> ChangeText ev.target.value
        , onKeyUp \ev ->
            if ev.key == "Enter" then (Save state.newText) else
              if ev.key == "Escape" then Cancel else (ChangeText state.newText)
        ]
        []
    ]
