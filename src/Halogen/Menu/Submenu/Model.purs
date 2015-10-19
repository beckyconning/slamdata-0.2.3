module Halogen.Menu.Submenu.Model where

import Prelude

import Data.Maybe (Maybe())

type KeyboardShortcut = String

type SubmenuItem a =
  { label :: String
  , action :: a
  , keyboardShortcut :: Maybe KeyboardShortcut
  }

type Submenu a = Array (SubmenuItem a)
