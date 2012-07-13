#!/usr/bin/env runhaskell
module Main where

import Data.Maybe
import Development.Shake
import System.FilePath

main = shake shakeOptions {shakeVerbosity = Loud} $ do
    want ["a.out"]
    "a.out" *> \out -> do
        cs <- getDirectoryFiles "." "*.c"
        let objs = map (++".o") cs
        need objs
        system' "gcc" $ ["-o",out] ++ objs
    "*.c.o" *> \out -> do
        compile (defaultOptions {includeDirs=["."]}) (dropExtension out) out

cIncludes :: FilePath -> [FilePath] -> Action [FilePath]
cIncludes x ipaths = do
    let incs = map ("-I"++) ipaths
    (stdout,_) <- systemOutput "gcc" $ ["-MM"] ++ incs ++ [x]
    return $ drop 2 $ words stdout

data ToolChain = GCC
               | MSVC
               | Intel
  deriving (Show, Read, Eq, Ord)

data StageEnum = Compile
               | Assembly
               | Preprocessor
               | Link
  deriving (Show, Read, Eq, Ord)

data Argument = Includes [FilePath]
              | Sources [FilePath]
              | Stage StageEnum
              | Output FilePath
  deriving (Show, Read, Eq, Ord)
type Pattern = [Argument]

data Options = Options { toolChain :: ToolChain
                       , command :: FilePath
                       , includeDirs :: [FilePath]
                       , pattern :: Pattern
                       } deriving (Show, Read, Eq, Ord)

-- | GCC specific!
argument :: Argument -> [String]
argument (Includes is) = map ("-I"++) is
argument (Sources srcs) = srcs
argument (Stage Compile) = ["-c"]
argument (Stage Assembly) = ["-S"]
argument (Stage Preprocessor) = ["-E"]
argument (Stage Link) = []
argument (Output o) = ["-o",o]

emptyPattern :: Pattern
emptyPattern = [Includes [], Stage Compile, Output "", Sources []]

defaultOptions :: Options
defaultOptions = Options { toolChain=GCC
                         , command="gcc"
                         , includeDirs=[]
                         , pattern=emptyPattern
                         }

fillArgument :: Options -> [FilePath] -> FilePath -> Argument -> Argument
fillArgument opts@(Options {includeDirs=incs}) _ _ (Includes _) = Includes incs
fillArgument _ srcs _ (Sources _) = Sources srcs
fillArgument _ _ _ s@(Stage _) = s
fillArgument _ _ out (Output _) = Output out

-- | Compile the 'pin' into 'pout' using 'opts'.
compile :: Options -> FilePath -> FilePath -> Action ()
compile opts@(Options {toolChain=GCC}) pin pout = do
  let comm = command opts
      gcc = if null comm then "gcc" else comm
      incs = includeDirs opts
      pat = pattern opts
  pinincs <- cIncludes pin incs
  need $ pin : pinincs
  let pat' = map (fillArgument opts [pin] pout) pat
  system' gcc $ concat $ map argument pat'

