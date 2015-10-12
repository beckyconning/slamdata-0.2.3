module UI.Notebook.Menu.Component where

import Prelude
import Halogen

import Control.Plus (Plus)
import Data.Generic (Generic, gEq, gCompare)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Foldable (find)
import Data.Maybe (Maybe(..))

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import UI.Notebook.Menu.Submenu.Model (Submenu(), SubmenuLabel(), nullSubmenu)
import UI.Notebook.Menu.Submenu.Action (SubmenuAction())
import UI.Notebook.Menu.Submenu.Component (submenuComponent)
import UI.Notebook.Menu.Model (MenuP(..))
import UI.Notebook.Menu.Action (MenuActionP(..))

newtype SubmenuSlot = SubmenuSlot SubmenuLabel

derive instance genericSubmenuSlot :: Generic SubmenuSlot
instance eqSubmenuSlot :: Eq SubmenuSlot where eq = gEq
instance ordSubmenuSlot :: Ord SubmenuSlot where compare = gCompare

type Menu g p = InstalledState MenuP Submenu MenuActionP SubmenuAction g SubmenuSlot p
type MenuAction = Coproduct MenuActionP (ChildF SubmenuSlot SubmenuAction)

menuParent :: forall g p. (Plus g) => ParentComponent MenuP Submenu MenuActionP SubmenuAction g SubmenuSlot p
menuParent = component render eval
  where

  render :: Render MenuP MenuActionP SubmenuSlot
  render menu = H.ul_ $ map (renderSubmenu menu.chosen) menu.submenus

  renderSubmenu :: Maybe SubmenuLabel -> Submenu -> HTML SubmenuSlot MenuActionP
  renderSubmenu chosen submenu
    | chosen == Just submenu.label = renderChosenSubmenu submenu.label
    | otherwise = renderHiddenSubmenu submenu.label

  renderSubmenuButton :: SubmenuLabel -> Action MenuActionP -> HTML SubmenuSlot MenuActionP
  renderSubmenuButton label action = H.button [ E.onClick $ E.input_ action ]
                                              [ H.text $ show label ]

  renderChosenSubmenu :: SubmenuLabel -> HTML SubmenuSlot MenuActionP
  renderChosenSubmenu label =
    H.li_ [ H.div_ [ renderSubmenuButton label HideSubmenu
                   , H.slot (SubmenuSlot label)
                   ]
          ]

  renderHiddenSubmenu :: SubmenuLabel -> HTML SubmenuSlot MenuActionP
  renderHiddenSubmenu label =
    H.li_ [ H.div_ [ renderSubmenuButton label (ChooseSubmenu label) ] ]

  eval :: EvalP MenuActionP MenuP Submenu MenuActionP SubmenuAction g SubmenuSlot p
  eval (ChooseSubmenu label next) = modify (_ { chosen = Just label }) $> next
  eval (HideSubmenu next) = modify (_ { chosen = Nothing }) $> next

menuComponent :: forall g p. (Plus g) => Component (Menu g p) MenuAction g p
menuComponent = installWithState menuParent createSubmenu
  where
  createSubmenu :: MenuP -> SubmenuSlot -> ChildState Submenu SubmenuAction g p
  createSubmenu m (SubmenuSlot label) = case ((== label) <<< _.label) `find` m.submenus of
                                           Nothing -> createChild submenuComponent nullSubmenu
                                           Just sm -> createChild submenuComponent sm
