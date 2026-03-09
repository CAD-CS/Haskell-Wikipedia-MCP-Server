{-# LANGUAGE OverloadedStrings #-}

module Request where

import Data.Aeson
import Data.Text (Text)
import Control.Monad (when)

data Request = Request
    { requestId :: Maybe Int
    , method    :: Text
    , params    :: Maybe Parameters
    } deriving (Show)

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> do 
        v <- o .: "jsonrpc"
        when (v /= ("2.0" :: Text)) $ fail "expected jsonrpc 2.0"
        Request
            <$> o .:?  "id"
            <*> o .:  "method"
            <*> o .:? "params"

data Parameters = Parameters
    { toolName  :: Maybe Text
    , arguments :: Maybe Object
    } deriving (Show)

instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \o -> Parameters
        <$> o .:? "name"
        <*> o .:? "arguments"
