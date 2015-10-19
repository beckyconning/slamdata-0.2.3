module Example.Component where

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

import Halogen.Menu.Component (MenuQueryP(), SubmenuSlot(), menuComponent)
import Halogen.Menu.Submenu.Query (SubmenuQuery(..))
import Example.Model (Example(), ExampleMenu(), exampleMenu)
import Example.Query (ExampleQuery(..))

type ExampleMenuQuery = MenuQueryP (ExampleQuery Unit)

type ExampleP g = InstalledState Example (ExampleMenu g) ExampleQuery ExampleMenuQuery g MenuSlot
type ExampleQueryP = Coproduct ExampleQuery (ChildF MenuSlot ExampleMenuQuery)

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ExampleChildState g = ExampleMenu g
type ExampleChildQuery = ExampleMenuQuery
type ExampleChildSlot = MenuSlot

cpMenu :: forall g. ChildPath (ExampleMenu g) (ExampleChildState g) ExampleMenuQuery ExampleChildQuery MenuSlot ExampleChildSlot
cpMenu = cpI

notebookComponent :: forall g. (Plus g) => Component (ExampleP g) ExampleQueryP g
notebookComponent = parentComponent' render eval peek
  where

  render :: RenderParent Example (ExampleChildState g) ExampleQuery ExampleMenuQuery g MenuSlot
  render state =
      H.div_ [-- H.slot' cpMenu MenuSlot \_ -> { component: menuComponent, initialState: installedState exampleMenu }
             -- ,
             H.h1_ [ H.text state.word ]
             ]

  eval :: EvalParent ExampleQuery Example (ExampleChildState g) ExampleQuery ExampleMenuQuery g MenuSlot
  eval (SetWord word next) = modify (_ { word = word }) $> next

  peek :: Peek (ChildF ExampleChildSlot ExampleChildQuery) Example (ExampleChildState g) ExampleQuery ExampleMenuQuery g MenuSlot
  peek (ChildF _ q) = coproduct (const (pure unit)) peekSubmenu q

  peekSubmenu :: Peek (ChildF SubmenuSlot (SubmenuQuery) Example (ExampleChildState g) ExampleQuery ExampleMenuQuery g MenuSlot
  peekSubmenu (ChildF _ (ChooseSubmenuItem q _)) = eval q
  peekSubmenu  _ = pure unit
