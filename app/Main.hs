{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad (filterM, forM_, when)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Options.Applicative (Parser, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, short, strOption, switch, value, (<**>))
import Options.Applicative.Types (ParserInfo)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory, pathIsSymbolicLink)
import System.Exit
import System.FilePath (splitPath, (</>))
import System.Process

appVersion :: String
appVersion = "0.1.1"

data Config = Config
  { rootPath :: String,
    showAllRepos :: Bool,
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

data StatusLine = StatusLine
  { x :: Char,
    y :: Char,
    path :: String,
    orig_path :: Maybe String
  }
  deriving (Show)

data PathStatus = Untracked | Modified | Added | Deleted deriving (Eq)

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
  let m = if hasModified rs then "M" else "."
      u = if hasUntracked rs then "?" else "."
      a = if hasAdded rs then "A" else "."
      d = if hasDeleted rs then "D" else "."
      up = if hasUnpushed rs then "^" else "."
      down = if hasUnpulled rs then "v" else "."
   in m ++ a ++ d ++ u ++ up ++ down

formatRepoStatus :: RepoStatus -> Bool -> String
formatRepoStatus rs fullPath =
  let repoName = last $ splitPath (repoPath rs)
      label = if fullPath then repoPath rs else repoName
      tags = formatTags rs
   in tags ++ " " ++ label

getRepoName :: RepoStatus -> FilePath
getRepoName rs = last $ splitPath (repoPath rs)

parseGitStatusLine :: String -> Maybe StatusLine
parseGitStatusLine s =
  case s of
    x0 : y0 : ' ' : path0 -> Just StatusLine {x = x0, y = y0, path = path0, orig_path = Nothing}
    _ -> Nothing

listDirectorySafe :: Config -> FilePath -> IO [FilePath]
listDirectorySafe cfg dir = do
  efs <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case efs of
    Left e -> do
      when (verbose cfg) $ putStrLn ("Failed to access directory " ++ dir ++ show e)
      return []
    Right fs -> return fs

isGitRepo :: Config -> FilePath -> IO Bool
isGitRepo cfg dir = do
  fs <- listDirectorySafe cfg dir
  return (".git" `elem` fs)

doesDirectoryExistSafe :: Config -> FilePath -> IO Bool
doesDirectoryExistSafe _ p = do
  eb <- try (doesDirectoryExist p) :: IO (Either SomeException Bool)
  case eb of
    Left _ -> return False
    Right b -> return b

isSymbolicLinkSafe :: FilePath -> IO Bool
isSymbolicLinkSafe p = do
  eb <- try (pathIsSymbolicLink p) :: IO (Either SomeException Bool)
  case eb of
    Left _ -> return False
    Right b -> return b

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

getGitStatus :: Config -> FilePath -> IO (Maybe RepoStatus)
getGitStatus cfg cwd0 = do
  let p = (proc "git" ["status", "--porcelain=v1"]) {cwd = Just cwd0}
  (exitCode, stdout, _) <-
    readCreateProcessWithExitCode p ""
  let ret =
        ( case exitCode of
            ExitSuccess ->
              let statusLines = fromMaybe [] $ sequence $ parseGitStatusLine <$> lines stdout
               in do
                    unpulled <- if localOnly cfg then return 0 else fromMaybe 0 <$> getUnpulled cwd0
                    unpushed <- if localOnly cfg then return 0 else fromMaybe 0 <$> getUnpushed cwd0
                    return (Just $ RepoStatus cwd0 statusLines unpushed unpulled)
            _ -> return Nothing
        )
  ret

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

main :: IO ()
main = do
  cfg <- execParser configParser
  hasGit <- isGitAvailable
  if hasGit
    then do
      cwd <- getCurrentDirectory
      let root = if not (null (rootPath cfg)) then rootPath cfg else cwd
      gitRepos <- findGitRepos cfg root
      statuses <-
        if showAllRepos cfg
          then catMaybes <$> mapConcurrently (getGitStatus cfg) gitRepos
          else filter isModified . catMaybes <$> mapConcurrently (getGitStatus cfg) gitRepos
      let formatter = \status -> formatRepoStatus status (showFullPath cfg)
      let sorter = if showFullPath cfg then repoPath else getRepoName
      let sortedStatuses = sortOn sorter statuses
      forM_ sortedStatuses $ putStrLn . formatter
      putStrLn $ "\n" ++ show (length gitRepos) ++ " repos found."
    else putStrLn "Error: Unable to find git executable."
