module UI.Notebook.Menu.Model where

import Prelude

import Data.Maybe (Maybe(..))

import UI.Notebook.Menu.Submenu.Model (SubmenuLabel(..), Submenu())
import UI.Notebook.Query (NotebookQuery(..))

type Menu =
  { chosen :: Maybe SubmenuLabel
  , submenus :: Array Submenu
  }

menu :: Menu
menu =
  { chosen: Nothing
  , submenus: [ { label: SubmenuLabel "Colors"
                , items: [ { label: "Red"
                           , action: SetWord "Red" unit
                           , keyboardShortcut: Nothing
                           }
                         , { label: "Blue"
                           , action: SetWord "Blue" unit
                           , keyboardShortcut: Nothing
                           }
                         , { label: "Green"
                           , action: SetWord "Green" unit
                           , keyboardShortcut: Nothing
                           }
                         ]
                }
              , { label: SubmenuLabel "Animals"
                , items: [ { label: "Dog"
                           , action: SetWord "Dog" unit
                           , keyboardShortcut: Nothing
                           }
                         , { label: "Cat"
                           , action: SetWord "Cat" unit
                           , keyboardShortcut: Nothing
                           }
                         , { label: "Rabbit"
                           , action: SetWord "Rabbit" unit
                           , keyboardShortcut: Nothing
                           }
                         , { label: "Fox"
                           , action: SetWord "Fox" unit
                           , keyboardShortcut: Nothing
                           }
                         ]
                }
              ]
  }
