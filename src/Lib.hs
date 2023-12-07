module Lib where

import Data.Text (Text)
import Text.HTML.Scalpel
import Data.Functor.Identity
import Control.Monad.Error.Class
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

type ScrapeWithExcept str e a = ScraperT str (Except e) a

textComment'' :: ScrapeWithExcept Text Text Comment
textComment'' = do
  author <- text $ "span" @: [hasClass "author"]
  commentText <- text $ "div" @: [hasClass "text"]
  return $ TextComment author commentText

-- |
--
-- >>> scrapeStringLikeT exampleHtml comments''
-- ExceptT (Identity (Right (Just [TextComment "Sally" "Woo hoo!",TextComment "Susan" "WTF!?!"])))
--
-- >>> runExcept $ scrapeStringLikeT exampleHtml comments''
-- Right (Just [TextComment "Sally" "Woo hoo!",TextComment "Susan" "WTF!?!"])
comments'' :: ScrapeWithExcept Text Text [Comment]
comments'' = chroots ("div" @: [hasClass "container"]) textComment''

-- learnings:
-- - we got ExceptT working (its simple constructor Except = ExceptT Identity)
-- - we can unwrap it with runExcept
-- - we yet don't know how to let it throw errors

-- step 4: let `comments` throw error
--
-- >>> runExcept $ scrapeStringLikeT exampleHtml comments'''
-- Left "Constant error"
comments''' :: ScrapeWithExcept Text Text [Comment]
comments''' = throwError "Constant error"

-- learnings:
-- - throwing that error in comments''' works
-- - how do we throw it only if there is no div with class "container"?

-- step 5: let `comments` only throw error if there is no div with class "container"
comments'''' :: ScrapeWithExcept Text Text [Comment]
comments'''' = do
                containers <- chroots ("div" @: [hasClass "container"])
                case containers of
                    --[] -> throwError "No div of class `container`"
                    --x -> return x

-- sel :: (TagSoup.StringLike str, Monad m, Data.String.IsString a, Control.Monad.Error.Class.MonadError a) => Either a (ScraperT str m str)
-- sel = (Right <$> text $ "div" @: [hasClass "text"]) <|> Left "could not find div class `text`"

--(<$?>) :: Text -> Scraper str a -> Scraper str b
--e <$?> scraper = Right <$> scraper >>= maybe (Left t) return
