module UI.Notebook.Menu.Submenu.Component where

import Prelude

import Data.Maybe (maybe)
import Data.Void (Void())

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import UI.Notebook.Cell.Type (CellType(..))
import UI.Notebook.Menu.Submenu.Model (Submenu(), SubmenuItem(), SubmenuLabel())
import UI.Notebook.Menu.Submenu.Query (SubmenuQuery(..))
import UI.Notebook.Query (NotebookQuery(..))

submenuComponent :: forall g. Component Submenu SubmenuQuery g
submenuComponent = component render eval
  where

  render :: Render Submenu SubmenuQuery
  render = H.ul_ <<< map renderItem <<< _.items

  renderItem :: SubmenuItem -> HTML Void SubmenuQuery
  renderItem item =
      H.li_ $ [ H.button [ E.onClick $ E.input_ (ChooseSubmenuItem item.action) ]
                         [ H.text item.label ]
             , H.span_ [ H.text $ maybe "" show item.keyboardShortcut ]
             ]

  eval :: Eval SubmenuQuery Submenu SubmenuQuery g
  eval (ChooseSubmenuItem _ next) = pure next
