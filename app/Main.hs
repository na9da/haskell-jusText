{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B (readFile, putStr)
import Data.Text (Text)
import qualified Data.Text as T (lines, unlines)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Environment (getArgs)
import System.Exit (die)


readUtf8File :: FilePath -> IO Text
readUtf8File filePath = decodeUtf8 <$> B.readFile filePath

main :: IO ()
main = do
  (htmlFile, stopWordsFile) <- getArgs'
  html <- readUtf8File htmlFile
  stopWords <- T.lines <$> readUtf8File stopWordsFile

  let paras = justext html defaultParams stopWords
      goodParas = filter ((== Good) . classType) paras
      mainText = T.unlines (printPara <$> goodParas)
  B.putStr $ encodeUtf8 mainText
  
  where
    getArgs' :: IO (FilePath, FilePath)
    getArgs' = do
      args <- getArgs
      if length args /= 2
         then die "Usage: justext <htmlFile> <stopwordsFile>"
         else return (args !! 0, args !! 1)
    
    printPara :: Paragraph -> Text
    printPara para =
      let tagType = fromMaybe "p" (getHeadingTag (path para))
      in "<" <> tagType <> ">"
           <> text para
           <> "</" <> tagType <> ">"

    
