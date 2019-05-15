{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Text.XML.HXT.Core
import           Text.XML.HXT.DOM.FormatXmlTree
import           Text.XML.HXT.HTTP

import           Control.Concurrent.Async

paths :: [String]
paths = [  "http://export.arxiv.org/api/query?search_query=all:the&start="
        ++ show (i * 2000)
        ++ "&max_results=2000" | i <- [0..14]]

selectAllEntries :: ArrowXml a => a XmlTree XmlTree
selectAllEntries = deep (isElem >>> hasName "entry")

selectAllTitle :: ArrowXml a => a XmlTree XmlTree
selectAllTitle = deep (isElem >>> hasName "title")

getDaTitles path = runX (readDocument [withHTTP []] path
    >>> selectAllEntries
    >>> selectAllTitle
    >>> deep isText
    >>> getText)


main :: IO ()
main = do
  xs <- mapConcurrently getDaTitles paths
  let zs = map (filter (/= '\n')) $ concat xs
  putStrLn $ unlines zs
