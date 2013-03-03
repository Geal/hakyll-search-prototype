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
            let myCtx = field "words" $ \_ -> (getWords routeToString (loadAll ("posts/*" .&&. hasVersion "raw")))
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

getWords :: Compiler String -> Compiler [Item String] -> Compiler String
getWords route posts = do
    p <- posts
    r <- route
    return $ encode . showJSON $ listWords $ p --r ++ (unlines . displayCount . countWords . listWords $ p)
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
