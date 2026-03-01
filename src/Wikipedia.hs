{-# LANGUAGE OverloadedStrings #-}

module Wikipedia
    ( getWikipediaSummary
    , getWikipediaHistory
    , getWikipediaLanguages
    ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Function ((&))
import Response
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as HTTPStatus

summaryBaseURL :: String
summaryBaseURL = "https://en.wikipedia.org/api/rest_v1/page/"

pageBaseURL :: String
pageBaseURL = "https://en.wikipedia.org/w/rest.php/v1/page/"

summaryURL :: String -> String
summaryURL topic = summaryBaseURL ++ "summary/" ++ topic

historyURL :: String -> String
historyURL topic = pageBaseURL ++ topic ++ "/history"

languageURL :: String -> String
languageURL topic = pageBaseURL ++ topic ++ "/links/language"

getWikipediaSummary :: Maybe Int -> String -> IO Response
getWikipediaSummary reqId topic = fetchWikipediaPage reqId (summaryURL topic)

getWikipediaHistory :: Maybe Int -> String -> IO Response
getWikipediaHistory reqId topic = fetchWikipediaPage reqId (historyURL topic)

getWikipediaLanguages :: Maybe Int -> String -> IO Response
getWikipediaLanguages reqId topic = fetchWikipediaPage reqId (languageURL topic)

fetchWikipediaPage :: Maybe Int -> String -> IO Response
fetchWikipediaPage reqId url = do
    baseReq <- HTTP.parseRequest url
    let req = baseReq
            & HTTP.setRequestHeader "User-Agent" ["haskell-mcp-server/0.1"]
            & HTTP.setRequestHeader "Accept-Encoding" ["identity"]
    res <- try (HTTP.httpBS req) :: IO (Either SomeException (HTTP.Response BS.ByteString))
    case res of
        Left _ -> return $ Response reqId $ Left $ RPCError (-32603) "HTTP request failed"
        Right httpRes -> return $ toResponse reqId httpRes

toResponse :: Maybe Int -> HTTP.Response BS.ByteString -> Response
toResponse reqId res =
    case HTTPStatus.statusCode (HTTP.getResponseStatus res) of
        200 -> case eitherDecodeStrict (HTTP.getResponseBody res) of
            Right val -> Response reqId $ Right val
            Left _ -> Response reqId $ Left $ RPCError (-32603) "Parse error"
        404 -> Response reqId $ Left $ RPCError (-32602) "Topic not found"
        429 -> Response reqId $ Left $ RPCError (-32603) "Wikipedia rate limit exceeded"
        _ -> Response reqId $ Left $ RPCError (-32603) "Upstream error"
