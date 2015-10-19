module UI.Notebook.Menu.Component where

import Prelude

import Control.Plus (Plus)

import Data.Foldable (find)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import UI.Notebook.Menu.Model (Menu(..))
import UI.Notebook.Menu.Query (MenuQuery(..))
import UI.Notebook.Menu.Submenu.Component (submenuComponent)
import UI.Notebook.Menu.Submenu.Model (Submenu(), SubmenuLabel(), nullSubmenu)
import UI.Notebook.Menu.Submenu.Query (SubmenuQuery())

type MenuP g = InstalledState Menu Submenu MenuQuery SubmenuQuery g SubmenuSlot
type MenuQueryP = Coproduct MenuQuery (ChildF SubmenuSlot SubmenuQuery)

newtype SubmenuSlot = SubmenuSlot SubmenuLabel

derive instance genericSubmenuSlot :: Generic SubmenuSlot
instance eqSubmenuSlot :: Eq SubmenuSlot where eq = gEq
instance ordSubmenuSlot :: Ord SubmenuSlot where compare = gCompare

menuComponent :: forall g. (Plus g) => Component (MenuP g) MenuQueryP g
menuComponent = parentComponent render eval
  where

  render :: RenderParent Menu Submenu MenuQuery SubmenuQuery g SubmenuSlot
  render menu = H.ul_ $ map renderSubmenu menu.submenus

    where

    renderSubmenu :: Submenu -> HTML _ _
    renderSubmenu submenu
      | menu.chosen == Just submenu.label = renderChosenSubmenu submenu.label
      | otherwise = renderHiddenSubmenu submenu.label

    renderChosenSubmenu :: SubmenuLabel -> HTML _ _
    renderChosenSubmenu label =
      H.li_ [ H.div_ [ renderSubmenuButton label HideSubmenu
                     , H.slot (SubmenuSlot label) \_ -> { component: submenuComponent
                                                        , initialState: findSubmenu label
                                                        }
                     ]
            ]

    findSubmenu :: SubmenuLabel -> Submenu
    findSubmenu label = fromMaybe nullSubmenu $ ((== label) <<< _.label) `find` menu.submenus

    renderHiddenSubmenu :: SubmenuLabel -> HTML _ _
    renderHiddenSubmenu label =
      H.li_ [ H.div_ [ renderSubmenuButton label (ChooseSubmenu label) ] ]

    renderSubmenuButton :: SubmenuLabel -> Action MenuQuery -> HTML _ _
    renderSubmenuButton label action = H.button [ E.onClick $ E.input_ action ]
                                              [ H.text $ show label ]

  eval :: EvalParent MenuQuery Menu Submenu MenuQuery SubmenuQuery g SubmenuSlot
  eval (ChooseSubmenu label next) = modify (_ { chosen = Just label }) $> next
  eval (HideSubmenu next) = modify (_ { chosen = Nothing }) $> next
