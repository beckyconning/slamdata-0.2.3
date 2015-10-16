module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)

import UI.Notebook.Component (notebookComponent)
import UI.Notebook.Model (notebook)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI notebookComponent (installedState notebook)
  appendToBody app.node
