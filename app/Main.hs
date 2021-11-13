module Main where

import qualified Config as Cfg
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import qualified GitUtils as Git
import Options.Applicative (execParser)
import StreamUtils (gitDirs)
import qualified Streamly.Data.Fold as Fold
import Streamly.Internal.FileSystem.Dir (toDirs, toFiles)
import qualified Streamly.Prelude as Stream
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cfg <- execParser Cfg.configParser
  hasGit <- Git.isGitAvailable
  if hasGit
    then do
      cwd <- getCurrentDirectory
      gitDirs (Cfg.maxDepth cfg) cwd & Stream.drain
      putStrLn "done!"
    else putStrLn "Error: Unable to find git executable on your PATH."

main0 :: IO ()
main0 = do
  cfg <- execParser Cfg.configParser
  hasGit <- Git.isGitAvailable
  if hasGit
    then do
      cwd <- getCurrentDirectory
      let root = if not (null (Cfg.rootPath cfg)) then Cfg.rootPath cfg else cwd
      gitRepos <- Git.findGitRepos cfg root
      statuses <-
        if Cfg.showAllRepos cfg
          then catMaybes <$> mapConcurrently (Git.getGitStatus cfg) gitRepos
          else filter Git.isModified . catMaybes <$> mapConcurrently (Git.getGitStatus cfg) gitRepos
      let formatter = \status -> Git.formatRepoStatus status (Cfg.showFullPath cfg)
      let sorter = if Cfg.showFullPath cfg then Git.repoPath else Git.getRepoName
      let sortedStatuses = sortOn sorter statuses
      forM_ sortedStatuses $ putStrLn . formatter
      putStrLn $ "\n" ++ show (length gitRepos) ++ " repos found."
      when (Cfg.showLegend cfg) (putStrLn $ "Legend: " ++ Git.statusLegend)
    else putStrLn "Error: Unable to find git executable on your PATH."
