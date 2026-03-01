{-# LANGUAGE OverloadedStrings #-}

module Request where

import Data.Aeson
import Data.Text (Text)

data Request = Request
    { requestId :: Int
    , method    :: Text
    , params    :: Maybe Parameters
    } deriving (Show)

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> Request
        <$> o .:  "id"
        <*> o .:  "method"
        <*> o .:? "params"

data Parameters = Parameters
    { toolName  :: Text
    , arguments :: Object
    } deriving (Show)

instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \o -> Parameters
        <$> o .: "name"
        <*> o .: "arguments"
