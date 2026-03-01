{-# LANGUAGE OverloadedStrings #-}

module Response where

import Data.Aeson
import Data.Text (Text)

data Response = Response
    { responseId :: Maybe Int
    , result     :: Either RPCError Value
    } deriving (Show)

instance ToJSON Response where
    toJSON r = object $
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id"      .= responseId r
        ] ++ case result r of
            Left  e -> [ "error"  .= e ]
            Right v -> [ "result" .= v ]

data RPCError = RPCError
    { errorCode    :: Int
    , errorMessage :: Text
    } deriving (Show)

instance ToJSON RPCError where
    toJSON e = object
        [ "code"    .= errorCode e
        , "message" .= errorMessage e
        ]
