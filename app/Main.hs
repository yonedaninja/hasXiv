{-# LANGUAGE OverloadedStrings #-}

module Main where

import Library
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
import Text.XML.HXT.HTTP

import Control.Concurrent.Async

paths :: String -> [String]
paths qry =
  [ "http://export.arxiv.org/api/query?search_query=all:" ++
  qry ++ "&start=" ++ show (i * 5) ++ "&max_results=5"
  | i <- [0 .. 14]
  ]

selectAllEntries :: ArrowXml a => a XmlTree XmlTree
selectAllEntries = deep (isElem >>> hasName "entry")

selectAllTitle :: ArrowXml a => a XmlTree XmlTree
selectAllTitle = deep (isElem >>> hasName "title")

getDaTitles :: String -> IO [String]
getDaTitles path =
  runX
    (readDocument [withHTTP []] path >>>
     selectAllEntries >>> selectAllTitle >>> deep isText >>> getText)

main :: IO ()
main = do
  query <- getLine
  xs <- mapConcurrently getDaTitles (paths query)
  let zs = map (filter (/= '\n')) $ concat xs
  putStrLn $ unlines zs
