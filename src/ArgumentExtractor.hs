module ArgumentExtractor where

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.Key    (fromString)
import Data.Text         (unpack)
import Prelude hiding    (lookup)
import Request

extractTopic :: Parameters -> Maybe String
extractTopic p =
    case arguments p of 
        Nothing -> Nothing
        Just args -> 
            case lookup (fromString "topic") args of
                Just (String t) -> Just (unpack t)
                _ -> Nothing
