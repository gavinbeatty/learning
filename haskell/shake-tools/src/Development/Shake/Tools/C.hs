module Development.Shake.Tools.C(
    ToolChain(..)
  , StageEnum(..)
  , Element(..)
  , Pattern
  , Options(..)
  , cIncludes
  , options
  , compile
  )where

import Development.Shake

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

data Element = Command FilePath
             | IncludePath FilePath
             | LibraryPath FilePath
             | Library FilePath
             | Source FilePath
             | Stage StageEnum
             | Output FilePath
             | Options [String]
  deriving (Show, Read, Eq, Ord)
type Pattern = [Element]

data Options = SimpleOptions { toolChain :: ToolChain
                             , compiler :: FilePath
                             , linker :: FilePath
                             , includeDirs :: [FilePath]
                             , libraryDirs :: [FilePath]
                             , libraries :: [FilePath]
                             , simplePattern :: Pattern
                             }
             | PatternOptions { pattern :: Pattern }
 deriving (Show, Read, Eq, Ord)

-- | GCC specific!
element :: Element -> [String]
element (Command c) = [c]
element (IncludePath idir) = ["-I"++idir]
element (LibraryPath ldir) = ["-L"++ldir]
element (Library l) = ["-l"++l]
element (Source src) = src
element (Stage Compile) = ["-c"]
element (Stage Assembly) = ["-S"]
element (Stage Preprocessor) = ["-E"]
element (Stage Link) = []
element (Output o) = ["-o",o]
element (Options opts) = opts

options :: Options
options = SimpleOptions { toolChain=GCC
                        , compiler="gcc"
                        , linker="gcc"
                        , includeDirs=[]
                        , libraryDirs=[]
                        , libraries=[]
                        , pattern=emptyPattern
                        }

fillElement :: Options -> [FilePath] -> FilePath -> Element -> Element
fillElement (Options {includeDirs=incs}) _ _ (Includes _) = Includes incs
fillElement _ srcs _ (Sources _) = Sources srcs
fillElement _ _ _ s@(Stage _) = s
fillElement _ _ out (Output _) = Output out

-- | Compile the 'pin' into 'pout' using 'opts'.
-- As a convenience, 'pout' is returned via the 'Action'.
compile :: Options -> FilePath -> FilePath -> Action FilePath
compile opts@(SimpleOptions _) = compileSimple opts

compileSimple :: Options -> FilePath -> FilePath -> Action FilePath
compileSimple opts@(SimpleOptions {toolChain=GCC}) pin pout = do
  let comp = compiler opts
      gcc = if null comp then "gcc" else comp
      incs = includeDirs opts
      pat = pattern opts
  pinincs <- cIncludes pin incs
  need $ pin : pinincs
  let pat' = map (fillElement opts [pin] pout) pat
  system' gcc $ concat $ map element pat'
compileSimple _ _ _ = undefined

-- | Link the 'pins' into 'pout' using 'opts'.
-- As a convenience, 'pout' is returned via the 'Action'.
link :: Options -> [FilePath] -> FilePath -> Action FilePath
link opts@(Options {toolChain=GCC}) pins pout = do
  let ld' = linker opts
      ld = if null ld' then "gcc" else ld'
      pat = pattern opts
  need pins
link _ _ _ = undefined
