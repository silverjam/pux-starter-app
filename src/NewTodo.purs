module App.NewTodo where

import App.Effects (AppEffects)
import App.TodoItem as TodoItem
import Control.Monad.Eff.Class (liftEff)
import Data.UUID (genUUID)
import Prelude (const, bind, return)
import Pux (EffModel, noEffects)
import Pux.Html (Html, input)
import Pux.Html.Attributes (className, value)
import Pux.Html.Events (onChange, onKey)

data Action = ChangeText String | Save | Cancel | Add TodoItem.State

type State = String

init :: State
init = ""

update :: Action -> State -> EffModel State Action AppEffects
update (ChangeText text) state = noEffects text
update Save state =
  { state: ""
  , effects: [ do
      id <- liftEff genUUID
      return (Add (TodoItem.init id state))
      ]
  }
update _ state = noEffects state

view :: State -> Html Action
view state =
  input
    [ className "new-todo"
    , onChange \ev -> ChangeText ev.target.value
    , onKey "Enter" (const Save)
    , value state
    ]
    []
