module App.Routes where

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Prelude (($))
import Pux.Router (end, router, lit)

data Route = Home | Bacon | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Bacon <$ (lit "bacon") <* end
