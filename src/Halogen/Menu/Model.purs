module Halogen.Menu.Model where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Menu.Submenu.Model (Submenu())

type Menu a =
  { chosen :: Maybe Int
  , submenus :: Array (MenuSubmenu a)
  }

type MenuSubmenu a = { label :: String, submenu :: Submenu a }
