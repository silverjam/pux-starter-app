module App.Counter where

import Prelude (($), (+), (-), const, show, return)
import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Pux (noEffects, EffModel)
import Pux.Html (Html, (!), (#), bind, div, span, button, text)
import Pux.Html.Events (onClick)

data Action = Increment | Decrement | Nop

type State = Int

init :: State
init = 0

update :: forall eff. Action -> State -> EffModel State Action (console :: CONSOLE | eff)
update Increment count =
  { state: count + 1
  , effects: [liftEff $ log "increment" *> return Nop]
  }
update Decrement count =
  { state: count + 1
  , effects: [liftEff $ log "decrement" *> return Nop]
  }
update Nop count = noEffects $ count

view :: State -> Html Action
view count =
  div # do
    button ! onClick (const Increment) # text "Increment"
    span # text (show count)
    button ! onClick (const Decrement) # text "decrement"
