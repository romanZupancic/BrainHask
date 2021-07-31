module CmdOptions (parseArguments
                  , Options(..)
                  , optionDescriptions
                  ) where

import System.Console.GetOpt
    ( OptDescr(..), ArgDescr(NoArg, ReqArg), getOpt, ArgOrder(RequireOrder) )

data Options = Options { optHelp :: Bool
                       , optErrors :: [String]
                       , optVersion :: Bool
                       , optInteractive :: Bool
                       , optMemoryOnExit :: Bool
                       , optCmd :: Maybe String 
                       } deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optHelp = False
                         , optErrors = []
                         , optVersion = False
                         , optInteractive = False
                         , optMemoryOnExit = False
                         , optCmd = Nothing}

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions =
    [ Option ['h']  ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print this help dialogue"
    , Option ['i']  ["interactive"] (NoArg (\opts -> opts { optInteractive = True })) "Launch into a interactive session after source file execution (or, immediately if no source is given)"
    , Option ['v']  ["version"] (NoArg (\opts -> opts { optVersion = True })) "Print the interpreter version number"
    , Option ['m']  ["memexit"] (NoArg (\opts -> opts { optMemoryOnExit = True })) "Print the Brainfuck State Memory on program exit"
    , Option ['c']  ["cmd"] (ReqArg (\str opts -> opts { optCmd = Just str }) "STRING") "Interpreter runs the given command, passed in as a string"
    ]

-- | Flattens a list of option changes into a single options record
-- The "flip id" actually translates to the (\ curr new -> new curr) function
optionsListToRecord :: [Options -> Options] -> Options
optionsListToRecord = foldl (flip id) defaultOptions

-- | Return an options record and a list of non-option command line arguments described by args
parseArguments :: [String] -> (Options, [String])
parseArguments args = 
    case getOpt RequireOrder optionDescriptions args of
        (opts, nonopts, []) -> (optionsListToRecord opts, nonopts)
        (_, nonopts, errs) -> (defaultOptions { optHelp = True, optErrors = errs}, nonopts)