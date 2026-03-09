{-# LANGUAGE OverloadedStrings #-}

module Wikipedia
    ( getWikipediaSummary
    , getWikipediaHistory
    , getWikipediaLanguages
    ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Function ((&))
import Data.Text
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Response
import ResultExtractor
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as HTTPStatus

type RequestId = Maybe Int
type Topic = String
type Url = String
type Extractor = (Value -> Maybe Value)

summaryBaseURL :: Url
summaryBaseURL = "https://en.wikipedia.org/api/rest_v1/page/"

pageBaseURL :: Url
pageBaseURL = "https://en.wikipedia.org/w/rest.php/v1/page/"

summaryURL :: Topic -> Url
summaryURL topic = summaryBaseURL ++ "summary/" ++ topic

historyURL :: Topic -> Url
historyURL topic = pageBaseURL ++ topic ++ "/history"

languageURL :: Topic -> Url
languageURL topic = pageBaseURL ++ topic ++ "/links/language"

getWikipediaSummary :: RequestId -> Topic -> IO Response
getWikipediaSummary reqId topic = fetchWikipediaPage reqId summaryExtractor (summaryURL topic)

getWikipediaHistory :: RequestId -> Topic -> IO Response
getWikipediaHistory reqId topic = fetchWikipediaPage reqId historyExtractor (historyURL topic)

getWikipediaLanguages :: RequestId -> Topic -> IO Response
getWikipediaLanguages reqId topic = fetchWikipediaPage reqId languageExtractor (languageURL topic)

fetchWikipediaPage :: RequestId -> Extractor -> Url -> IO Response
fetchWikipediaPage reqId extractor url = do
    baseReq <- HTTP.parseRequest url
    let req = baseReq
            & HTTP.setRequestHeader "User-Agent" ["haskell-mcp-server/0.1"]
            & HTTP.setRequestHeader "Accept-Encoding" ["identity"]
    res <- try (HTTP.httpBS req) :: IO (Either SomeException (HTTP.Response BS.ByteString))
    case res of
        Left _ -> return $ Response reqId $ Left $ RPCError (-32603) "HTTP request failed"
        Right httpRes -> return $ toResponse reqId extractor httpRes

toResponse :: RequestId -> Extractor -> HTTP.Response BS.ByteString -> Response
toResponse reqId extractor res =
    case HTTPStatus.statusCode (HTTP.getResponseStatus res) of
        200 -> case eitherDecodeStrict (HTTP.getResponseBody res) of
            Right rawVal -> processRequestValue rawVal
            Left _ -> Response reqId $ Left $ RPCError (-32603) "Parse error"
        404 -> Response reqId $ Left $ RPCError (-32602) "Topic not found"
        429 -> Response reqId $ Left $ RPCError (-32603) "Wikipedia rate limit exceeded"
        _ -> Response reqId $ Left $ RPCError (-32603) "Upstream error"
  where
    processRequestValue = extractResult reqId extractor

extractResult :: RequestId -> Extractor -> Value -> Response
extractResult reqId extractor rawVal = 
    case extractor rawVal of
        Just val -> Response reqId $ Right (mkContent val) 
        Nothing -> Response reqId $ Left $ RPCError (-32603) "Parse error"

mkContent :: Value -> Value
mkContent val = object 
    [ "content" .= 
        [ object 
            [ "type" .= ("text" :: Text)
            , "text" .= jsonToText val 
            ]
        ]
    ]

jsonToText :: Value -> Text
jsonToText = toStrict . decodeUtf8 . encode
