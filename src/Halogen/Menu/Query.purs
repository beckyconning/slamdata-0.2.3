module Halogen.Menu.Query where

data MenuQuery a next
  = SetMenu (Array a) next
  | ChooseSubmenu Int next
  | HideSubmenu next
