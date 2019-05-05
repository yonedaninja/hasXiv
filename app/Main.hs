{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Text.XML.HXT.Core
import           Text.XML.HXT.DOM.FormatXmlTree
import           Text.XML.HXT.HTTP

path :: String
path = "http://export.arxiv.org/api/query?search_query=all:coend&start=0&max_results=10"

selectAllEntries :: ArrowXml a => a XmlTree XmlTree
selectAllEntries = deep (isElem >>> hasName "entry")

selectAllTitle :: ArrowXml a => a XmlTree XmlTree
selectAllTitle = deep (isElem >>> hasName "title")

main :: IO ()
main = do
  x <- runX (readDocument [withHTTP []] path
    >>> selectAllEntries
    >>> selectAllTitle
    >>> deep isText
    >>> getText)
  putStrLn $ unlines x
