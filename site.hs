--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import           Debug.Trace
import           Hakyll.Core.Compiler
import           Data.List
import           qualified Data.Map as M
import           Text.JSON
import           Data.Maybe
import           Data.Char
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ version "raw" $ do 
        route idRoute
        compile  getResourceBody

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match "search.html" $ do 
        route idRoute
        compile  copyFileCompiler

--- Pass the posts and their route rules
    createSearch (loadAll("posts/*" .&&. hasVersion "raw")) (setExtension "html")

------------------------------------------------------------------------------------------

type PostUrl       = Int
type PostContent   = String
type PostData      = (Maybe PostUrl, PostContent)
type Word          = String
type WordList      = [Word]
type PostWords     = (Maybe PostUrl, WordList)
type SearchData    = [(Word, [PostUrl])]
type UrlList       = [PostUrl]
type MapSearchData = M.Map Word UrlList

-- get post title, URL and content
extractPostData :: [Identifier] -> Item String -> PostData
extractPostData posts post = 
                       let url = elemIndex (itemIdentifier post) posts
                           content = itemBody post
                       in (url, content)

addUrlToSearch :: Maybe PostUrl -> MapSearchData -> Word -> MapSearchData
addUrlToSearch (Just url) search word = case M.lookup word search of
                                    Just urlList -> M.insert word (url : urlList) search
                                    Nothing -> M.insert word [url] search
addUrlToSearch Nothing search _ = search


addPostToSearch :: SearchData -> PostWords -> SearchData
addPostToSearch search post = M.toList (foldl (addUrlToSearch (fst post)) (M.fromList search) (snd post) )

foldWordList :: SearchData -> [PostWords] -> SearchData
foldWordList search wordlist = foldl addPostToSearch search wordlist

--extracts the list of words from a post and creates a mapping word -> list of URLs
postsToWordList :: [Item String] -> SearchData
postsToWordList posts = let postsData = fmap (extractPostData (fmap itemIdentifier posts)) posts
                            word = fmap (\x -> (fst x, extractWordsFromText $ snd x )) $ postsData
                        in foldWordList [] word

extractWordsFromText :: String -> WordList
extractWordsFromText = nub . sort . (map lowercase) . words

lowercase :: String -> String
lowercase = map toLower

getWords :: Compiler String -> Compiler [Item String] -> Routes -> Compiler String
getWords route posts routes = do
    p <- posts
    r <- route
    return $ case r of "urls.json" -> encode . showJSON $ fmap (fromJust . (runRoutes routes) . itemIdentifier) p
                       str         -> encode . toJSObject . postsToWordList $ p

createSearch :: Compiler [Item String] -> Routes -> Rules()
createSearch posts routes =
    create ["search.json", "urls.json"] $ do
        route idRoute
        compile $ do
            let myCtx = field "words" $ \_ -> (getWords routeToString posts routes)
            makeItem "" >>= loadAndApplyTemplate "templates/words.txt" myCtx

routeToString :: Compiler String
routeToString = do 
    identifier <- getUnderlying
    Just s <- getRoute identifier
    return s
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = (field "reverse" $ return . reverse .itemBody ) `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
