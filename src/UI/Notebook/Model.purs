module UI.Notebook.Model where

import Prelude

-- | The state of the component
type NotebookP = { word :: String }

notebook :: NotebookP
notebook = { word: "" }
