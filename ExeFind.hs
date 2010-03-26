-- Author: Helge Skogly Holm
--
-- Tool for finding all files and directories matching a given glob pattern
-- in current directory and all subdirectories.
-- Glob patterns must start with a ! character.  Wildcards (*, ?) and
-- character groups ([ABC123]) are supported.
--
-- Example - find all entries ending in .hs below this directory:
--  runhaskell ExeFind.hs !*.hs
--
-- Example - find that L4D2 config file I always forget the exact location of:
--  runhaskell ExeFind.hs "c:\Program Files (x86)\Steam" !config.cfg
--
-- Example - list all entries under current directory:
--  runhaskell ExeFind.hs
--
-- Example - list all entries under "c:\Users\Public\Documents":
--  runhaskell ExeFind.hs "c:\Users\Public\Documents"

import Control.Exception (handle)
import Control.Monad (liftM, mapM_, forM)
import System (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (splitFileName, (</>))

import Globmatch (match)

main = do
  (path, pat) <- liftM parseArgs getArgs
  mapM_ putStrLn =<< findAll path pat

parseArgs :: [String] -> (String, String)
parseArgs arg = case arg of
  (path:('!':pat):[]) -> (path, pat)
  (('!':pat):[])      -> (".", pat)
  (path:[])           -> (path, "*")
  []                  -> (".", "*")
  otherwise           -> error "Parameters: [path] [!pattern]"

findAll :: String -> String -> IO [String]
findAll path pat = do
   contents <- getUsefulDirectoryContents path
   liftM concat $ forM contents $ \x -> do
     let subPath = path </> x
     let matchL = if match pat x
                  then [subPath]
                  else []
     isDir <- doesDirectoryExist subPath
     rec <- if isDir then findAll subPath pat
                     else return []
     return $ matchL ++ rec

type IOErrH a = IOError -> IO a

getUsefulDirectoryContents :: String -> IO [String]
getUsefulDirectoryContents path =
  handle ((\_ -> return [])::IOErrH [String]) getUDC
  where getUDC = do
          entries <- getDirectoryContents path
          return $ filter (not . (`elem` [".", ".."])) entries
