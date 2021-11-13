module Main where

import qualified Config as Cfg
import Control.Monad (when)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified GitUtils as Git
import Options.Applicative (execParser)
import StreamUtils (gitDirs)
import qualified Streamly.Prelude as Stream
import System.Directory (getCurrentDirectory)
import System.Directory.Internal.Prelude (newIORef)

main :: IO ()
main = do
  cfg <- execParser Cfg.configParser
  hasGit <- Git.isGitAvailable
  if hasGit
    then do
      root <- maybe getCurrentDirectory return (Cfg.rootPath cfg)
      countRef <- newIORef 0 :: IO (IORef Int)
      let formatter = \status -> Git.formatRepoStatus status (Cfg.showAbsPath cfg) root
      gitDirs (Cfg.maxDepth cfg) root
        & Stream.mapMaybeM (Git.getGitStatus cfg)
        & Stream.trace (\_ -> modifyIORef' countRef (+ 1))
        & Stream.filter (if Cfg.showAllRepos cfg then const True else Git.isModified)
        & Stream.map formatter
        & Stream.mapM putStrLn
        & Stream.fromAsync
        & Stream.drain
      nRepos <- readIORef countRef
      putStrLn $ "\n" ++ show nRepos ++ " repos checked."
      when (Cfg.showLegend cfg) (putStrLn $ "Legend: " ++ Git.statusLegend)
    else putStrLn "Error: Unable to find git executable on PATH."
