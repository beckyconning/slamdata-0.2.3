module UI.Notebook.Action where

import UI.Notebook.Cell.Type (CellType())

data NotebookActionP a
  = Rename a
  | Delete a
  | Publish a
  | InsertCell CellType a
  | EvaluateCurrentCell a
  | DeleteCurrentCell a
  | SetWord String a
  | OpenURLInNewTab String a
