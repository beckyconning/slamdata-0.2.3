module Halogen.Menu.Component where

import Prelude

import Control.Plus (Plus)

import Data.Array (zip, (..), length)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import Halogen.Menu.Model (Menu(), MenuSubmenu())
import Halogen.Menu.Query (MenuQuery(..))
import Halogen.Menu.Submenu.Component (submenuComponent)
import Halogen.Menu.Submenu.Model (Submenu())
import Halogen.Menu.Submenu.Query (SubmenuQuery())

type MenuP a g = InstalledState (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlot
type MenuQueryP a = Coproduct (MenuQuery a) (ChildF SubmenuSlot (SubmenuQuery a))

newtype SubmenuSlot = SubmenuSlot Int

derive instance genericSubmenuSlot :: Generic SubmenuSlot
instance eqSubmenuSlot :: Eq SubmenuSlot where eq = gEq
instance ordSubmenuSlot :: Ord SubmenuSlot where compare = gCompare

menuComponent :: forall a g. (Plus g) => Component (MenuP a g) (MenuQueryP a) g
menuComponent = parentComponent render eval
  where

  render :: RenderParent (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlot
  render menu = H.ul_ $ map renderSubmenu (zip (0 .. (length menu.submenus - 1)) menu.submenus)
    where

    renderSubmenu :: Tuple Int (MenuSubmenu a) -> HTML _ _
    renderSubmenu (Tuple index menuSubmenu)
      | menu.chosen == Just index = renderChosenSubmenu index menuSubmenu
      | otherwise = renderHiddenSubmenu index menuSubmenu

    renderChosenSubmenu :: Int -> MenuSubmenu a -> HTML _ _
    renderChosenSubmenu index menuSubmenu =
      H.li_
        [ H.div_
            [ renderSubmenuButton index menuSubmenu
            , H.slot (SubmenuSlot index) \_ ->
                { component: submenuComponent
                , initialState: menuSubmenu.submenu
                }
            ]
        ]

    renderHiddenSubmenu :: Int -> MenuSubmenu a -> HTML _ _
    renderHiddenSubmenu index menuSubmenu =
        H.li_ [ H.div_ [ renderSubmenuButton index menuSubmenu ] ]

    renderSubmenuButton :: Int -> MenuSubmenu a -> HTML _ _
    renderSubmenuButton index menuSubmenu =
      H.button
        [ E.onClick $ E.input_ $ ChooseSubmenu index ]
        [ H.text $ show menuSubmenu.label ]

  eval :: EvalParent (MenuQuery a) (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlot
  eval (ChooseSubmenu index next) = modify (_ { chosen = Just index }) $> next
  eval (HideSubmenu next) = modify (_ { chosen = Nothing }) $> next
