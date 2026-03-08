{-# LANGUAGE OverloadedStrings #-}

module ResultExtractor where 

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

summaryExtractor :: Value -> Maybe Value
summaryExtractor res = parseMaybe (withObject "obj" (.: "extract")) res

historyExtractor :: Value -> Maybe Value
historyExtractor res = parseMaybe (withObject "obj" (.: "revisions")) res

languageExtractor :: Value -> Maybe Value
languageExtractor res = return res
