module UI.Notebook.Menu.Query where

import UI.Notebook.Menu.Submenu.Model (SubmenuLabel())

data MenuQuery a
  = ChooseSubmenu SubmenuLabel a
  | HideSubmenu a
