{-# LANGUAGE NamedFieldPuns #-}

module GitUtils
  ( statusLegend,
    findGitRepos,
    isGitAvailable,
    doGitFetch,
    getRepoName,
    isModified,
    getGitStatus,
    formatRepoStatus,
    RepoStatus (..),
  )
where

import Config (Config (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import FileUtils (doesDirectoryExistSafe, isSymbolicLinkSafe, listDirectorySafe)
import System.Exit
import System.FilePath (makeRelative, splitPath, (</>))
import System.Process

findGitRepos :: Config -> FilePath -> IO [FilePath]
findGitRepos cfg root = go root (maxDepth cfg)
  where
    go _ 0 = return []
    go p depth = do
      ig <- isGitRepo cfg p
      if ig
        then return [p]
        else do
          fps0 <- listDirectorySafe cfg p
          let fps = map (p </>) fps0
          dirs0 <- filterM (doesDirectoryExistSafe cfg) fps
          dirs <- filterM (fmap not . isSymbolicLinkSafe) dirs0
          if null dirs
            then return []
            else concat <$> mapConcurrently (\dir -> go dir (depth - 1)) dirs

data StatusLine = StatusLine
  { x :: Char,
    y :: Char,
    path :: String,
    orig_path :: Maybe String
  }
  deriving (Show)

data PathStatus = Untracked | Modified | Added | Deleted deriving (Eq)

isGitRepo :: Config -> FilePath -> IO Bool
isGitRepo cfg dir = do
  fs <- listDirectorySafe cfg dir
  return (".git" `elem` fs)

pathStatusFromStatusLine :: StatusLine -> PathStatus
pathStatusFromStatusLine StatusLine {x, y} = case (x, y) of
  ('?', '?') -> Untracked
  ('A', ' ') -> Added
  (' ', 'M') -> Modified
  ('M', '_') -> Modified
  (' ', 'D') -> Deleted
  ('D', '_') -> Deleted
  (_, _) -> Modified

data RepoStatus = RepoStatus
  { repoPath :: String,
    statusLines :: [StatusLine],
    unpushed :: Int,
    unpulled :: Int
  }

isModified :: RepoStatus -> Bool
isModified rs = not (null (statusLines rs)) || unpushed rs > 0 || unpulled rs > 0

hasModified :: RepoStatus -> Bool
hasModified rs = Modified `elem` map pathStatusFromStatusLine (statusLines rs)

hasUntracked :: RepoStatus -> Bool
hasUntracked rs = Untracked `elem` map pathStatusFromStatusLine (statusLines rs)

hasAdded :: RepoStatus -> Bool
hasAdded rs = Added `elem` map pathStatusFromStatusLine (statusLines rs)

hasDeleted :: RepoStatus -> Bool
hasDeleted rs = Deleted `elem` map pathStatusFromStatusLine (statusLines rs)

hasUnpushed :: RepoStatus -> Bool
hasUnpushed rs = unpushed rs > 0

hasUnpulled :: RepoStatus -> Bool
hasUnpulled rs = unpulled rs > 0

formatTags :: RepoStatus -> String
formatTags rs =
  let m = if hasModified rs then "M" else "-"
      u = if hasUntracked rs then "?" else "-"
      a = if hasAdded rs then "A" else "-"
      d = if hasDeleted rs then "D" else "-"
      up = if hasUnpushed rs then "^" else "-"
      down = if hasUnpulled rs then "v" else "-"
   in m ++ a ++ d ++ u ++ up ++ down

statusLegend :: String
statusLegend = "modified (M), untracked (?), added (A), deleted (D), unpushed (^), unpulled (v)"

formatRepoStatus :: RepoStatus -> Bool -> String -> String
formatRepoStatus rs fullPath root =
  let absRepoPath = repoPath rs
      label = if fullPath then absRepoPath else makeRelative root absRepoPath
      tags = formatTags rs
   in tags ++ " " ++ label

getRepoName :: RepoStatus -> FilePath
getRepoName rs = last $ splitPath (repoPath rs)

parseGitStatusLine :: String -> Maybe StatusLine
parseGitStatusLine s =
  case s of
    x0 : y0 : ' ' : path0 -> Just StatusLine {x = x0, y = y0, path = path0, orig_path = Nothing}
    _ -> Nothing

getGitStatus :: Config -> FilePath -> IO (Maybe RepoStatus)
getGitStatus _ cwd0 = do
  let p = (proc "git" ["status", "--porcelain=v1"]) {cwd = Just cwd0}
  (exitCode, stdout, _) <-
    readCreateProcessWithExitCode p ""
  let ret =
        ( case exitCode of
            ExitSuccess ->
              let statusLines = fromMaybe [] $ sequence $ parseGitStatusLine <$> lines stdout
               in do
                    unpulled <- fromMaybe 0 <$> getUnpulled cwd0
                    unpushed <- fromMaybe 0 <$> getUnpushed cwd0
                    return (Just $ RepoStatus cwd0 statusLines unpushed unpulled)
            _ -> return Nothing
        )
  ret

-- TODO: log errors if verbose
doGitFetch :: Config -> FilePath -> IO ()
doGitFetch _ dir = do
  let p = (proc "git" ["fetch"]) {cwd = Just dir}
  void $ readCreateProcessWithExitCode p ""

isGitAvailable :: IO Bool
isGitAvailable = do
  let cp = proc "git" ["--version"]
  (exitCode, _, _) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> return True
    _ -> return False

getUnpulled :: FilePath -> IO (Maybe Int)
getUnpulled cwd0 = do
  let p = (proc "git" ["log", "--pretty=format:'%h'", "..@{u}"]) {cwd = Just cwd0}
  (exitCode, stdout, _) <- readCreateProcessWithExitCode p ""
  case exitCode of
    ExitSuccess -> return (Just $ length $ lines stdout)
    _ -> return Nothing

getUnpushed :: FilePath -> IO (Maybe Int)
getUnpushed cwd0 = do
  let p = (proc "git" ["log", "--pretty=format:'%h'", "@{u}.."]) {cwd = Just cwd0}
  (exitCode, stdout, _) <- readCreateProcessWithExitCode p ""
  case exitCode of
    ExitSuccess -> return (Just $ length $ lines stdout)
    _ -> return Nothing
