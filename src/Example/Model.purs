module Example.Model where

import Prelude

import Data.Maybe (Maybe(..))

import Example.Query (ExampleQuery(..))
import Halogen (installedState)
import Halogen.Menu.Component (MenuP())

type Example = { word :: String }

type ExampleMenu g = MenuP Example g

notebook :: Example
notebook = { word: "" }

exampleMenu :: forall g. ExampleMenu g
exampleMenu = installedState
  { chosen: Nothing
  , submenus: [
  --    { label: "Colors"
  --    , submenu:
  --        [ { label: "Red"
  --          , action: SetWord "Red" unit
  --          , keyboardShortcut: Nothing
  --          }
  --        , { label: "Blue"
  --          , action: SetWord "Blue" unit
  --          , keyboardShortcut: Nothing
  --          }
  --        , { label: "Green"
  --          , action: SetWord "Green" unit
  --          , keyboardShortcut: Nothing
  --          }
  --        ]
  --    }
  --    , { label: "Animals"
  --      , submenu:
  --          [ { label: "Dog"
  --            , action: SetWord "Dog" unit
  --            , keyboardShortcut: Nothing
  --            }
  --          , { label: "Cat"
  --            , action: SetWord "Cat" unit
  --            , keyboardShortcut: Nothing
  --            }
  --          , { label: "Rabbit"
  --            , action: SetWord "Rabbit" unit
  --            , keyboardShortcut: Nothing
  --            }
  --          , { label: "Fox"
  --            , action: SetWord "Fox" unit
  --            , keyboardShortcut: Nothing
  --            }
  --          ]
  --      }
  --    ]
  }
