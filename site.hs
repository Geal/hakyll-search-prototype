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
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

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

    create ["search.json"] $ do
        route idRoute
        compile $ do
            --let myCtx = field "words" $ \_ -> wordList
            let myCtx = field "words" $ \_ -> (getWords routeToString (loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"))
            --let myCtx = field "words" $ \_ -> (getWords routeToString (loadAll ("posts/*" .&&. hasVersion "raw")))
            makeItem "" >>= loadAndApplyTemplate "templates/words.txt" myCtx

routeToString :: Compiler String
routeToString = do 
    identifier <- getUnderlying
    Just s <- getRoute identifier
    return s

countWords :: [String] -> [(String, Int)]
countWords words = map (\xs -> (head xs, length xs)) . group . sort $ words

kvToString :: (String, Int) -> String
kvToString (k,v) = k ++ ": " ++ (show  v)

displayCount :: [(String, Int)] -> [String]
displayCount = map kvToString

listWords :: [Item String] -> [String]
--listWords posts = concat (fmap words (fmap itemBody posts))
listWords posts = concat . (fmap words) . (fmap itemBody) $ posts

firstWord :: [Item String] -> String
--firstWord = head . concat . (fmap words) . listWords
firstWord = concat . sort . concat . (fmap words) . listWords

type PostUrl       = String
type PostContent   = String
type PostData      = (PostUrl, PostContent)
type Word          = String
type WordList      = [Word]
type PostWords     = (PostUrl, WordList)
type SearchData    = [(Word, [PostUrl])]
type UrlList       = [PostUrl]
type MapSearchData = M.Map Word UrlList

-- get post title, URL and content
extractPostData :: Item String -> PostData
extractPostData post = 
                       let url = fromJust . (runRoutes (setExtension "html")) . itemIdentifier $ post
                           content = itemBody post
                       in (url, content)

addUrlToSearch :: PostUrl -> MapSearchData -> Word -> MapSearchData
addUrlToSearch url search word = case M.lookup word search of
                                    Just urlList -> M.insert word (url : urlList) search
                                    Nothing -> M.insert word [url] search

addPostToSearch :: SearchData -> PostWords -> SearchData
addPostToSearch search post = M.toList (foldl (addUrlToSearch (fst post)) (M.fromList search) (snd post) )

foldWordList :: SearchData -> [PostWords] -> SearchData
foldWordList search wordlist = foldl addPostToSearch search wordlist

--extracts the list of words from a post and creates a mapping word -> list of URLs
postsToWordList :: [Item String] -> SearchData
postsToWordList posts = let postsData = fmap extractPostData posts
                            word = fmap (\x -> (fst x, (nub . sort . words . snd) $ x )) $ postsData
                        in foldWordList [] word

getWords :: Compiler String -> Compiler [Item String] -> Compiler String
getWords route posts = do
    p <- posts
    r <- route
    return $ encode . showJSON . postsToWordList $ p
    --return $ encode . showJSON . (fmap (extractPostData )) $ p
    --return $ fromJust (runRoutes (setExtension "html") ( itemIdentifier (head p) ))
    --return $ r ++ (encode . showJSON $ listWords $ p) -- show the list of words as JSON array
    --return $ r ++ (unlines . displayCount . countWords . listWords $ p) -- show for each word the number of occurrences, and prepend the file's name
    --return $ (unlines (nub (sort ( listWords p ))))
    --return $ (( listWords p ) !! 1)
    --p <- posts
    --return head listWords p
    -- words <- itemBody <$> posts
    -- body  <- words --map itemBody words
    -- return ["aaa"]


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
