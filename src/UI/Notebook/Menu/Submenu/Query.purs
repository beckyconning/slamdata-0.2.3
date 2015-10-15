module UI.Notebook.Menu.Submenu.Query where

import Prelude

import UI.Notebook.Query (NotebookQuery())

data SubmenuQuery a
  = ChooseSubmenuItem (NotebookQuery Unit) a
