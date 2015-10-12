module UI.Notebook.Component where

import Prelude
import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpI)

import Control.Plus (Plus)
import Control.Monad.Free (Free())
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)
import Data.Void (Void(), absurd)

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import UI.Notebook.Menu.Component (Menu(), MenuAction(), SubmenuSlot(), menuComponent)
import UI.Notebook.Action (NotebookActionP(..))
import UI.Notebook.Model (NotebookP())
import UI.Notebook.Menu.Model (menu)
import UI.Notebook.Menu.Submenu.Action (SubmenuAction(..))

type NotebookChildState g p = Menu g p
type NotebookChildAction = MenuAction
type NotebookChildSlot = MenuSlot

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type Notebook g p = InstalledState NotebookP (Menu g p) (Const Void) MenuAction g MenuSlot p
type NotebookAction = Coproduct (Const Void) (ChildF MenuSlot MenuAction)

cpMenu :: forall g p. ChildPath (Menu g p) (NotebookChildState g p) MenuAction NotebookChildAction MenuSlot NotebookChildSlot
cpMenu = cpI

notebookParent :: forall g p. ParentComponentP NotebookP (NotebookChildState g p) (Const Void) NotebookChildAction g NotebookChildSlot p
notebookParent = component' render eval peek
  where
  render :: Render NotebookP (Const Void) NotebookChildSlot
  render state =
      H.div_ [ H.slot MenuSlot
             , H.h1_ [ H.text state.word ]
             ]

  eval :: EvalP (Const Void) NotebookP (NotebookChildState g p) (Const Void) NotebookChildAction g NotebookChildSlot p
  eval (Const v) = absurd v

  peek :: Peek (ChildF NotebookChildSlot NotebookChildAction) NotebookP (NotebookChildState g p) (Const Void) NotebookChildAction g NotebookChildSlot p
  peek (ChildF _ q) = coproduct (const (pure unit)) peekSubmenu q

  peekSubmenu :: Peek (ChildF SubmenuSlot SubmenuAction) NotebookP (NotebookChildState g p) (Const Void) NotebookChildAction g NotebookChildSlot p
  peekSubmenu (ChildF _ (ChooseSubmenuItem (SetWord word _) _)) = modify (_ { word = word })
  peekSubmenu  _ = pure unit

notebookComponent :: forall g p. (Plus g) => Component (Notebook g p) NotebookAction g p
notebookComponent = install' notebookParent createMenu
  where
  createMenu :: MenuSlot -> ChildState (Menu g p) MenuAction g p
  createMenu MenuSlot = createChild menuComponent (installedState menu)
