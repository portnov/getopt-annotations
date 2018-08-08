{-# LANGUAGE TemplateHaskell #-}

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

{-# ANN cDebug def {short = ['d'], verbose = ["debug"], help = "enable debug"} #-}
{-# ANN cInputFile def {short = ['i'], verbose = ["input"], help = "input file"} #-}
{-# ANN cVerboseLevel def {short = ['v'], verbose = ["verbose"], help = "verbosity level"} #-}

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


