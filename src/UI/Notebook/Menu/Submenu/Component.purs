module UI.Notebook.Menu.Submenu.Component where

import Prelude
import Halogen

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Data.Maybe (maybe)
import UI.Notebook.Action (NotebookActionP(..))
import UI.Notebook.Cell.Type (CellType(..))
import UI.Notebook.Menu.Submenu.Model (Submenu(), SubmenuItem(), SubmenuLabel())
import UI.Notebook.Menu.Submenu.Action (SubmenuAction(..))

submenuComponent :: forall g p. Component Submenu SubmenuAction g p
submenuComponent = component render eval
  where

  render :: Render Submenu SubmenuAction p
  render = H.ul_ <<< map renderItem <<< _.items

  renderItem :: SubmenuItem -> HTML p SubmenuAction
  renderItem item =
      H.li_ $ [ H.button [ E.onClick $ E.input_ (ChooseSubmenuItem item.action) ]
                         [ H.text item.label ]
             , H.span_ [ H.text $ maybe "" show item.keyboardShortcut ]
             ]

  eval :: Eval SubmenuAction Submenu SubmenuAction g
  eval (ChooseSubmenuItem _ next) = pure next
