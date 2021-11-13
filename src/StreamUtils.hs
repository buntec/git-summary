module StreamUtils (gitDirs) where

import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Data.Function ((&))
import Streamly.Internal.FileSystem.Dir (toDirs)
import qualified Streamly.Prelude as Stream
import System.Directory (pathIsSymbolicLink)
import System.FilePath ((</>))

toDirsSafe :: Stream.IsStream t => String -> t IO String
toDirsSafe root = toDirs root & Stream.handle handler
  where
    handler :: (Stream.IsStream t) => SomeException -> t IO String
    handler _ = Stream.nil

isSymbolicLinkSafe :: FilePath -> IO Bool
isSymbolicLinkSafe p = do
  eb <- try (pathIsSymbolicLink p) :: IO (Either SomeException Bool)
  case eb of
    Left _ -> return False
    Right b -> return b

isGitDir :: String -> IO Bool
isGitDir dir = toDirsSafe dir & Stream.elem ".git"

gitDirs :: Stream.IsStream t => Int -> String -> t IO String
gitDirs maxDepth root = Stream.fromAsync $ do
  if maxDepth > 0
    then do
      p <- toDirsSafe root
      let ap = root </> p
      isGit <- Stream.fromEffect $ isGitDir ap
      isSymLink <- Stream.fromEffect $ isSymbolicLinkSafe ap
      case (isGit, isSymLink) of
        (_, True) -> Stream.nil
        (True, _) -> return ap
        (False, False) -> gitDirs (maxDepth - 1) ap
    else Stream.nil
