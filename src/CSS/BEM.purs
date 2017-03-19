module CSS.BEM (
  class WithModifiers,
  Block(),
  Element(),
  b,
  e,
  m,
  mWhen,
  mVal
) where

import Prelude
import Data.List (List(..), intercalate, reverse, (:))


data Modifier
  = Modifier String
  | ModifierVal String String

type BlockName = String

data Block = Block BlockName (List Modifier)

data Element = Element String BlockName (List Modifier)

b :: String -> Block
b name = Block name Nil

e :: Block -> String -> Element
e (Block block _) name = Element name block Nil

class WithModifiers bem where
  m :: String -> bem -> bem
  mWhen :: String -> Boolean -> bem -> bem
  mVal :: String -> String -> bem -> bem

instance modifierShow :: Show Modifier where
  show (Modifier m) = m
  show (ModifierVal m v) = m <> "_" <> v

instance blockWithModifiers :: WithModifiers Block where
  m mod (Block name blockMods) = Block name $ Modifier mod : blockMods
  mWhen mod true (Block name blockMods) = Block name $ Modifier mod : blockMods
  mWhen _ _ block = block
  mVal mod val (Block name blockMods) = Block name $ ModifierVal mod val : blockMods

instance blockShow :: Show Block where
  show (Block name mods) = intercalate " " classNames
    where
    classNames = name : modClassNames
    modClassNames = reverse $ append (name <> "_") <<< show <$> mods

instance elementWithModifiers :: WithModifiers Element where
  m mod (Element name blockName elementMods) =
    Element name blockName $ Modifier mod : elementMods
  mWhen mod true (Element name blockName elementMods) =
    Element name blockName $ Modifier mod : elementMods
  mWhen _ _ element = element
  mVal mod val (Element name blockName elementMods) =
    Element name blockName $ ModifierVal mod val : elementMods

instance elementShow :: Show Element where
  show (Element name block mods) = intercalate " " classNames
    where
    classNames = elementClass : modClassNames
    modClassNames = append (elementClass <> "_") <<< show <$> mods
    elementClass = block <> "__" <> name
