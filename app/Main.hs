module Main where

import Data.Text (Text)
import Text.HTML.Scalpel

main :: IO ()
main = print $ scrapeStringLike exampleHtml comments

-- adopted from <https://github.com/fimad/scalpel/blob/master/examples/example-from-documentation/Main.hs>
exampleHtml :: Text
exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>Sally</span>\
\                <div class='comment text'>Woo hoo!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

type Author = Text

data Comment = TextComment Author Text deriving (Show, Eq)

textComment :: Scraper Text Comment
textComment = do
  author <- text $ "span" @: [hasClass "author"]
  commentText <- text $ "div" @: [hasClass "text"]
  return $ TextComment author commentText

comments :: Scraper Text [Comment]
comments = chroots ("div" @: [hasClass "container"]) textComment
