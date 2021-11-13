module Config (Config (..), configParser) where

import Data.Version (showVersion)
import Options.Applicative (Parser, auto, fullDesc, header, help, helper, info, long, metavar, option, short, strOption, switch, value, (<**>))
import Options.Applicative.Types (ParserInfo)
import qualified Paths_git_summary as BuildInfo

appVersion :: String
appVersion = showVersion BuildInfo.version

data Config = Config
  { rootPath :: String,
    showAllRepos :: Bool,
    showLegend :: Bool,
    verbose :: Bool,
    localOnly :: Bool,
    showFullPath :: Bool,
    maxDepth :: Int
  }

configParser0 :: Parser Config
configParser0 =
  Config
    <$> strOption
      ( long "root"
          <> short 'r'
          <> value ""
          <> help "The root directory under which to search recursively for git repos; defaults to the current working directory."
          <> metavar "DIR"
      )
    <*> switch
      ( long "show-all"
          <> short 'a'
          <> help "Show all repos. If absent, only repos that are not up-to-date or have local modifications are shown."
      )
    <*> ( not
            <$> switch
              ( long "no-legend"
                  <> help "Do not show the legend."
              )
        )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Show warning messages etc."
      )
    <*> switch
      ( long "local-only"
          <> short 'l'
          <> help "Skip git fetch."
      )
    <*> ( not
            <$> switch
              ( long "short"
                  <> short 's'
                  <> help "Show only the name of the folder containing the repo instead of the full path."
              )
        )
    <*> option
      auto
      ( long "max-depth"
          <> short 'd'
          <> value 10
          <> help "The maximum depth of recursion for finding git repos; defaults to 10; negative values mean infinity."
          <> metavar "INT"
      )

configParser :: ParserInfo Config
configParser =
  info
    (configParser0 <**> helper)
    (fullDesc <> header ("git-summary " ++ appVersion ++ " - displays a status summary of all git repos under a given root"))
