module UI.Notebook.Query where

import UI.Notebook.Cell.Type (CellType())

data NotebookQuery a
  = Rename a
  | Delete a
  | Publish a
  | InsertCell CellType a
  | EvaluateCurrentCell a
  | DeleteCurrentCell a
  | SetWord String a
  | OpenURLInNewTab String a
