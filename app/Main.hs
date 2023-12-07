module Main where

import Text.HTML.Scalpel
import Lib

main :: IO ()
main = print $ scrapeStringLike exampleHtml comments

main' :: IO ()
main' = print $ scrapeStringLikeT exampleHtml comments'
