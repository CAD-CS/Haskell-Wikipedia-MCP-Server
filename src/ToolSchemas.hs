{-# LANGUAGE OverloadedStrings #-}

module ToolSchemas
    ( wikipediaSummaryTool
    , wikipediaLanguagesTool
    , wikipediaHistoryTool
    ) where

import Data.Aeson
import Data.Aeson.Key (fromString)

type ToolName = String 
type ToolDescription = String 
type ToolArgument = String
type ArgumentDescription = String
type ToolSchema = Value

mkTool :: ToolName -> ToolDescription -> ToolArgument -> ArgumentDescription -> ToolSchema
mkTool toolName toolDesc argName argDesc = object
    [ "name"        .= toolName
    , "description" .= toolDesc
    , "inputSchema" .= object
        [ "type"       .= ("object" :: String)
        , "required"   .= ([argName] :: [String])
        , "properties" .= object
            [ fromString argName .= object
                [ "type"        .= ("string" :: String)
                , "description" .= argDesc
                ]
            ]
        ]
    ]

wikipediaSummaryTool :: ToolSchema
wikipediaSummaryTool = mkTool
    "wikipedia_summary"
    "Fetches a summary of a Wikipedia topic"
    "topic"
    "The Wikipedia topic to summarize"

wikipediaLanguagesTool :: ToolSchema
wikipediaLanguagesTool = mkTool
    "wikipedia_languages"
    "Fetches a list of languages available for a Wikipedia topic"
    "topic"
    "The Wikipedia topic to fetch available languages for"

wikipediaHistoryTool :: ToolSchema
wikipediaHistoryTool = mkTool
    "wikipedia_history"
    "Fetches the edit history of a Wikipedia topic"
    "topic"
    "The Wikipedia topic to fetch edit history for"

