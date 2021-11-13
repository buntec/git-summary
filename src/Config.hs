module Config (Config (..), configParser) where

import Data.Version (showVersion)
import Options.Applicative (Parser, auto, fullDesc, header, help, helper, info, long, metavar, option, optional, short, strOption, switch, value, (<**>))
import Options.Applicative.Types (ParserInfo)
import qualified Paths_git_summary as BuildInfo

appVersion :: String
appVersion = showVersion BuildInfo.version

data Config = Config
  { rootPath :: Maybe String,
    showAllRepos :: Bool,
    filterDots :: Bool,
    showLegend :: Bool,
    localOnly :: Bool,
    showAbsPath :: Bool,
    maxDepth :: Int
  }

defaultSearchDepth :: Int
defaultSearchDepth = 7

configParser0 :: Parser Config
configParser0 =
  Config
    <$> optional
      ( strOption
          ( long "root"
              <> short 'r'
              <> help "The root directory under which to search recursively for git repos; defaults to the current working directory."
              <> metavar "DIR"
          )
      )
    <*> switch
      ( long "show-all"
          <> short 'a'
          <> help "Show all repos. When absent, only repos that are not up-to-date or have local modifications are shown."
      )
    <*> switch
      ( long "skip-dots"
          <> short 's'
          <> help "Do not traverse into directories that start with a dot."
      )
    <*> ( not
            <$> switch
              ( long "no-legend"
                  <> help "Do not show the legend."
              )
        )
    <*> switch
      ( long "local-only"
          <> short 'l'
          <> help "Skip git fetch."
      )
    <*> switch
      ( long "abs-paths"
          <> help "Show absolute paths to repos instead of relative to the root."
      )
    <*> option
      auto
      ( long "max-depth"
          <> short 'd'
          <> value defaultSearchDepth
          <> help ("The maximum depth of recursion for finding git repos; defaults to " ++ show defaultSearchDepth ++ ".")
          <> metavar "INT"
      )

configParser :: ParserInfo Config
configParser =
  info
    (configParser0 <**> helper)
    (fullDesc <> header ("git-summary " ++ appVersion ++ " - displays a status summary of all git repos under a given root"))
