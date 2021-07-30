module CmdOptions (parseArguments
                  ) where

import System.Console.GetOpt
    ( OptDescr(..), ArgDescr(NoArg), getOpt, ArgOrder(RequireOrder) )

data Options = Options { optInteractive :: Bool
                       , optVersion :: Bool
                       , optHelp :: Bool
                       } deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optInteractive = False
                         , optVersion = False
                         , optHelp = False}

optionDescription :: [OptDescr (Options -> Options)]
optionDescription =
    [ Option ['i']  ["interactive"] (NoArg (\opts -> opts { optInteractive = True })) "Launch into a interactive session after source file execution (or, immediately if no source is given"
    , Option ['v']  ["version"] (NoArg (\opts -> opts { optVersion = True })) "Print the interpreter version number"
    , Option ['h']  ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print this help dialogue" ]

-- The "flip id" actually translates to the (\ curr new -> new curr) function
optionsListToRecord :: [Options -> Options] -> Options
optionsListToRecord = foldl (flip id) defaultOptions

parseArguments :: [String] -> (Options, [String])
parseArguments args = 
    case getOpt RequireOrder optionDescription args of
        (opts, nonopts, []) -> (optionsListToRecord opts, nonopts)
        (_, nonopts, errs) -> (defaultOptions { optHelp = True }, nonopts)