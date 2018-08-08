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

data Option =
    Option {
      short :: [Char],
      long :: [String],
      placeholder :: String,
      help :: String
    }
  | Auto String
  deriving (Eq, Show, Data, Typeable)

getShort :: Option -> [Char]
getShort (Option {short=s}) = s
getShort (Auto str) = [head str]

getLong :: Option -> [String]
getLong (Option {long=l}) = l
getLong (Auto str) = [str]

getHelp :: Option -> String
getHelp (Option {help=h}) = h
getHelp (Auto _) = "no help provided"

getPlaceholder :: Option -> String
getPlaceholder (Option {placeholder=ph}) = ph
getPlaceholder (Auto _) = "VALUE"

instance Default Option where
  def = Option [] [] "VALUE" "no help provided"

instance Semigroup Option where
  (Auto _) <> o2@(Auto _) = o2

  (Auto _) <> o2@(Option {}) = o2

  o1@(Option {}) <> (Auto _) = o1

  o1@(Option {}) <> o2@(Option {}) =
    Option {
        short = short o1 ++ short o2,
        long  = long o1 ++ long o2,
        placeholder = if placeholder o1 == placeholder o2
                        then placeholder o2
                        else placeholder o1 ++ "|" ++ placeholder o2,
        help = if help o1 == help o2
                 then help o2
                 else help o1 ++ " " ++ help o2
      }

instance Monoid Option where
  mempty = def
  mappend = (<>)

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

