module UI.Notebook.Menu.Action where

import Prelude
import UI.Notebook.Menu.Submenu.Model (SubmenuLabel())

data MenuActionP a
  = ChooseSubmenu SubmenuLabel a
  | HideSubmenu a
