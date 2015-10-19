module Halogen.Menu.Submenu.Query where

import Prelude

data SubmenuQuery a next = ChooseSubmenuItem a next
