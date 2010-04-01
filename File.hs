module File where

import Control.Exception (handle, IOException)
import Control.Monad (liftM)
import Data.ByteString.Lazy as B (foldl', readFile, writeFile)
import Data.Word
import Directory

safeMove :: String -> String -> IO ()
safeMove src dst = do
  hSrc <- checkSum src
  copy src dst
  hDst <- checkSum dst
  if hSrc == hDst
    then removeFile src
    else do
      handle ignoreIOException $ removeFile dst
      fail $ "Checksum fail, " ++ (show hSrc) ++ " /= " ++ (show hDst)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

checkSum :: String -> IO Word32
checkSum name = do
  dataz <- B.readFile name
  return $ B.foldl' (\a b -> a + (fromIntegral b)) 0 dataz

copy :: String -> String -> IO ()
copy src dst = do
  dataz <- B.readFile src
  B.writeFile dst dataz

getUsefulDirConts :: String -> IO [String]
getUsefulDirConts d = do
  conts <- getDirectoryContents d
  return $ filter (`notElem` [".", ".."]) conts
