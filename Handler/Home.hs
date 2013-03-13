{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.EventSource
import Data.Conduit
import Control.Concurrent (threadDelay)
import Control.Monad
import Network.Wai.EventSource
import Blaze.ByteString.Builder
import System.Environment
import Data.Text (pack)
import Data.Text.Encoding
import Data.Monoid

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepEventSource
getHomeR = do
    port <- liftIO $ getEnv "PORT"
    repEventSource $ const $ forever $ do
        yield $ ServerEvent Nothing Nothing [fromByteString $ "Test: " <> encodeUtf8 (pack port)]
        liftIO $ threadDelay 10000000

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
