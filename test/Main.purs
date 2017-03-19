module Test.Main where

import Prelude
import CSS.BEM as BEM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)


assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: EXCEPTION) Unit
main = do
  let
    block = BEM.b "block"
    blockElement = BEM.e block "element"
    redBlock = BEM.mVal "color" "red" block
    emptyRedBlock = BEM.mWhen "empty" true redBlock
    emptyRedBlockContent = BEM.e emptyRedBlock "content"
    emptyRedBlockContentNonempty = BEM.mWhen "empty" false emptyRedBlockContent
    emptyRedBlockContentNonemptyImportant = BEM.m "important" emptyRedBlockContentNonempty
  show block `assertEqual` "block"
  show blockElement `assertEqual` "block__element"
  show redBlock `assertEqual` "block block_color_red"
  show emptyRedBlock `assertEqual` "block block_color_red block_empty"
  show emptyRedBlockContent `assertEqual` "block__content"
  show emptyRedBlockContentNonempty `assertEqual` "block__content"
  show emptyRedBlockContentNonemptyImportant `assertEqual` "block__content block__content_important"
