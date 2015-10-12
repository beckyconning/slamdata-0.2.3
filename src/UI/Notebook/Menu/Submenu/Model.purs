module UI.Notebook.Menu.Submenu.Model where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe())
import UI.Notebook.Action (NotebookActionP())

type KeyboardShortcut = String

type SubmenuItem = { label :: String, action :: NotebookActionP Unit, keyboardShortcut :: Maybe KeyboardShortcut }

type Submenu = { label :: SubmenuLabel, items :: Array SubmenuItem }

nullSubmenu :: Submenu
nullSubmenu = { label: SubmenuLabel "", items: [] }

newtype SubmenuLabel = SubmenuLabel String
derive instance genericSubmenuLabel :: Generic SubmenuLabel
instance eqSubmenuLabel :: Eq SubmenuLabel where eq = gEq
instance ordSubmenuLabel :: Ord SubmenuLabel where compare = gCompare
instance showSubmenuLabel :: Show SubmenuLabel where show (SubmenuLabel s) = s
