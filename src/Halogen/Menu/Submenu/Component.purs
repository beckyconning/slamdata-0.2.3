module Halogen.Menu.Submenu.Component where

import Prelude

import Data.Maybe (maybe)
import Data.Void (Void())

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import Halogen.Menu.Submenu.Model (Submenu(), SubmenuItem())
import Halogen.Menu.Submenu.Query (SubmenuQuery(..))

submenuComponent :: forall g a. Component (Submenu a) (SubmenuQuery a) g
submenuComponent = component render eval
  where

  render :: Render (Submenu a) (SubmenuQuery a)
  render = H.ul_ <<< map renderItem

  renderItem :: (SubmenuItem a) -> HTML Void (SubmenuQuery a)
  renderItem item =
    H.li_
      [ H.button
          [ E.onClick $ E.input_ (ChooseSubmenuItem item.action) ]
          [ H.text item.label ]
      , H.span_ [ H.text $ maybe "" show item.keyboardShortcut ]
      ]

  eval :: Eval (SubmenuQuery a) (Submenu a) (SubmenuQuery a) g
  eval (ChooseSubmenuItem _ next) = pure next
