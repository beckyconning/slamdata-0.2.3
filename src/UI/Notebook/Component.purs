module UI.Notebook.Component where

import Prelude

import Control.Monad.Free (Free())
import Control.Plus (Plus)

import Data.Const (Const(..))
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)
import Data.Void (Void(), absurd)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpI)
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import UI.Notebook.Menu.Component (MenuP(), MenuQueryP(), SubmenuSlot(), menuComponent)
import UI.Notebook.Menu.Model (menu)
import UI.Notebook.Menu.Submenu.Query (SubmenuQuery(..))
import UI.Notebook.Model (Notebook())
import UI.Notebook.Query (NotebookQuery(..))

type NotebookP g = InstalledState Notebook (MenuP g) NotebookQuery MenuQueryP g MenuSlot
type NotebookQueryP = Coproduct NotebookQuery (ChildF MenuSlot MenuQueryP)

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type NotebookChildState g = MenuP g
type NotebookChildQuery = MenuQueryP
type NotebookChildSlot = MenuSlot

cpMenu :: forall g. ChildPath (MenuP g) (NotebookChildState g) MenuQueryP NotebookChildQuery MenuSlot NotebookChildSlot
cpMenu = cpI

notebookComponent :: forall g. (Plus g) => Component (NotebookP g) NotebookQueryP g
notebookComponent = parentComponent' render eval peek
  where

  render :: RenderParent Notebook (NotebookChildState g) NotebookQuery MenuQueryP g MenuSlot
  render state =
      H.div_ [ H.slot' cpMenu MenuSlot \_ -> { component: menuComponent, initialState: installedState menu }
             , H.h1_ [ H.text state.word ]
             ]

  eval :: EvalParent NotebookQuery Notebook (NotebookChildState g) NotebookQuery MenuQueryP g MenuSlot
  eval (SetWord word next) = modify (_ { word = word }) $> next

  peek :: Peek (ChildF NotebookChildSlot NotebookChildQuery) Notebook (NotebookChildState g) NotebookQuery MenuQueryP g MenuSlot
  peek (ChildF _ q) = coproduct (const (pure unit)) peekSubmenu q

  peekSubmenu :: Peek (ChildF SubmenuSlot SubmenuQuery) Notebook (NotebookChildState g) NotebookQuery MenuQueryP g MenuSlot
  peekSubmenu (ChildF _ (ChooseSubmenuItem q _)) = eval q
  peekSubmenu  _ = pure unit

