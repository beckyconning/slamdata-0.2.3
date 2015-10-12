module UI.Notebook.Menu.Submenu.Action where

import Prelude
import UI.Notebook.Action (NotebookActionP())

data SubmenuAction a = ChooseSubmenuItem (NotebookActionP Unit) a
