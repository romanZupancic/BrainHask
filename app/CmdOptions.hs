module CmdOptions (parseArguments
                  , Options(..)
                  , optionDescriptions
                  ) where

import System.Console.GetOpt
    ( OptDescr(..), ArgDescr(NoArg, ReqArg), getOpt, ArgOrder(RequireOrder) )

import Data.Maybe ( isNothing )

data Options = Options { optHelp :: Bool
                       , optErrors :: Maybe [String]
                       , optVersion :: Bool
                       , optInteractive :: Bool
                       , optMemoryOnExit :: Bool
                       , optCmd :: Maybe String
                       , optFile :: Maybe String
                       } deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optHelp = False
                         , optErrors = Nothing
                         , optVersion = False
                         , optInteractive = False
                         , optMemoryOnExit = False
                         , optCmd = Nothing
                         , optFile = Nothing
                         }

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
parseArguments :: [String] -> Options
parseArguments args =
    case getOpt RequireOrder optionDescriptions args of
        (opts, nonOpts, []) -> parseNonOpts (optionsListToRecord opts) nonOpts
        (_, nonOpts, errs)  -> (defaultOptions { optErrors = Just errs })
    where
        parseNonOpts optsRecord@Options{ optCmd = c} nonOpts
            -- If neither a cmd nor a file is given, trigger interactive mode
            | null nonOpts = if isNothing c then optsRecord { optInteractive = True } else optsRecord
            | otherwise = optsRecord { optFile = Just $ head nonOpts}
