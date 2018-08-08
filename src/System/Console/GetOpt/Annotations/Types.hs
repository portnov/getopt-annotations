{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.Console.GetOpt.Annotations.Types where

import qualified Data.Map as M
import Data.Default
import Data.Data
import Data.Typeable
import Data.Char (isDigit)
import Data.String
import Language.Haskell.TH

data Option = Option {
    short :: [Char],
    long :: [String],
    placeholder :: String,
    help :: String
  }
  deriving (Eq, Show, Data, Typeable)

instance Default Option where
  def = Option [] [] "VALUE" "no help provided"

instance IsString Option where
  fromString s =
    def {long = [s], short = [head s]}

data FieldAnnotation = FieldAnnotation Name Type Option
  deriving (Eq, Show)

class OptionValue a where
  parseValue :: String -> Either String a

instance OptionValue String where
  parseValue x = Right x

parseInt :: (Integral a, Read a) => String -> Either String a
parseInt s = 
    if all isDigit s
      then Right $ read s
      else Left $ "Not a number: " ++ s

instance OptionValue Int where
  parseValue s = parseInt s

instance OptionValue Integer where
  parseValue s = parseInt s

