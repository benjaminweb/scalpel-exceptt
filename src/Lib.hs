module Lib where

import Data.Text (Text)
import Text.HTML.Scalpel
import Data.Functor.Identity
import Control.Monad.Error.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Applicative ((<|>), empty)
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup
import Control.Monad.Except
import Data.String

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

-- step 1: first approach

textComment :: Scraper Text Comment
textComment = do
  author <- text $ "span" @: [hasClass "author"]
  commentText <- text $ "div" @: [hasClass "text"]
  return $ TextComment author commentText

-- | Retrieve comments directly.
--
-- >>> scrapeStringLike exampleHtml comments
-- Just [TextComment "Sally" "Woo hoo!",TextComment "Susan" "WTF!?!"]
comments :: Scraper Text [Comment]
comments = chroots ("div" @: [hasClass "container"]) textComment

-- learnings:
-- - textComment does extract Sally and Susan but not Bill
-- - we don't learn that until we look at the source html
-- - since that is cumbersome, it'd be great to let textComment make us aware of that directly

-- step 2: let's turn on the Monad transformer variant with a null wrapper Identity

type ScrapeWithIdentity str a = ScraperT str Identity a

textComment' :: ScrapeWithIdentity Text Comment
textComment' = do
  author <- text $ "span" @: [hasClass "author"]
  commentText <- text $ "div" @: [hasClass "text"]
  return $ TextComment author commentText

-- | Retrieve Comments with null wrapper.
--
-- >>> scrapeStringLikeT exampleHtml comments'
-- Identity (Just [TextComment "Sally" "Woo hoo!",TextComment "Susan" "WTF!?!"])
--
-- >>> runIdentity $ scrapeStringLikeT exampleHtml comments'
-- Just [TextComment "Sally" "Woo hoo!",TextComment "Susan" "WTF!?!"]
--
-- >>> runIdentity $ scrapeStringLikeT exampleHtml comments' == scrapeStringLike exampleHtml comments
-- True
comments' :: ScrapeWithIdentity Text [Comment]
comments' = chroots ("div" @: [hasClass "container"]) textComment'

-- learnings:
-- - we can wrap it with a Monad and unwrap it with its runner after

-- step 3: let's use the ExceptT transformer to track exceptions.

type ScrapeWithError str e a = ScraperT str (Either e) a

textComment'' :: ScrapeWithError Text Text Comment
textComment'' = do
  author <- text $ "span" @: [hasClass "author"] <|> throwError "author field not present"
  commentText <- text $ "div" @: [hasClass "text"] <|> throwError "comment field not present"
  return $ TextComment author commentText

-- |
-- >>> scrapeStringLikeOrError exampleHtml comments''
-- ???
comments'' :: ScrapeWithError Text Text [Comment]
comments'' = chroots ("div" @: [hasClass "container"]) textComment'' <|> throwError "div class 'container' not present"


-- | Unpack `ScraperWithError Either Text (Maybe a)` to `Either Text a`.
scrapeStringLikeOrError :: Text -> ScraperWithError Text Text a -> Either Text a
scrapeStringLikeOrError html scraper
    | Left error <- result = Left error
    | Right Nothing <- result = Left "Unknown error"
    | Right (Just a) <- result = Right a
  where
  result = scrapeStringLikeT html scraper

-- learnings:
-- - we got Either working
-- - wherever an error is thrown, it cancels the execution and returns a Left
-- - this Left is propagated through the top scraper
-- - since we may have multiple errors, we would like to have an overview, so collect all errors

-- step 4: let's use WriterT monad to collect all errors
type ScraperWithErrors str e a = ScraperT str (Writer [e]) a

scrapeStringOrErrors :: Text -> ScraperWithErrors Text Text a
scrapeStringOrErrors html scraper = runWriter . scrapeStringLikeT

textComment''' :: ScraperWithErrors Text Text Comment
textComment''' = do
  author <- text $ "span" @: [hasClass "author"] <|> logError "author field not present"
  commentText <- text $ "div" @: [hasClass "text"] <|> logError "comment field not present"
  return $ TextComment author commentText

-- | Append error to WriterT.
logError :: Text -> ScraperWithErrors Text Text a
logError message = do
    currentHtml <- html anySelector
    tell ["Unknown comment type: " html]
    empty

comments''' :: ScraperWithErrors Text Text [Comment]
comments''' = chroots ("div" @: [hasClass "container"]) textComment'' <|> logError "div class 'container' not present"

-- learnings:
-- - we collect all errors now and get an overview of all that way
