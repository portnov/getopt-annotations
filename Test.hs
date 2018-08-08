{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import System.Console.GetOpt as G
import System.Environment

import System.Console.GetOpt.Annotations

data CmdLine = CmdLine {
    cDebug :: Bool,
    cInputFile :: String,
    cVerboseLevel :: Maybe Int
  }
  deriving (Eq, Show)

{-# ANN cDebug "debug" {help = "enable debug"} #-}
{-# ANN cInputFile "input" {help = "input file"} #-}
{-# ANN cVerboseLevel "verbose" {help = "verbosity level"} #-}

instance Default CmdLine where
  def = CmdLine False "" Nothing

$(return [])

options :: [G.OptDescr (CmdLine -> CmdLine)]
options = $(makeOptions ''CmdLine)

main :: IO ()
main = do
  argv <- getArgs
  let header = "usage: Test..."
  opts <- case getOpt Permute options argv of
            (o,n, []) -> return $ foldl (flip id) def o
            (_,_,errs) -> fail $ concat errs ++ usageInfo header options
  print opts


