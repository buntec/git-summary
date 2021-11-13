module FileUtils (listDirectorySafe, doesDirectoryExistSafe, isSymbolicLinkSafe) where

import Config (Config (..))
import Control.Exception (SomeException, try)
import Control.Monad (when)
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)

listDirectorySafe :: Config -> FilePath -> IO [FilePath]
listDirectorySafe cfg dir = do
  efs <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case efs of
    Left e -> do
      when (verbose cfg) $ putStrLn ("Failed to access directory " ++ dir ++ show e)
      return []
    Right fs -> return fs

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
