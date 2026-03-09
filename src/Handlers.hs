{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Aeson
import ToolResolver
import Request
import Response
import ToolSchemas

handleInitialize :: Request -> Response
handleInitialize req = Response (requestId req) (Right body)
  where
    body = object
        [ "protocolVersion" .= ("2024-11-05" :: String)
        , "capabilities" .= capabilities
        , "serverInfo" .= serverInfo
        ]
    capabilities = object [ "tools" .= object [] ]
    serverInfo = object
        [ "name" .= ("haskell-mcp-server" :: String)
        , "version" .= ("0.1.0" :: String)
        ]

handleListTools :: Request -> Response
handleListTools req = Response (requestId req) (Right body)
  where
    body = object
        [ "tools" .= [ wikipediaSummaryTool
                     , wikipediaLanguagesTool
                     , wikipediaHistoryTool
                     ]
        ]

handleCallTool :: Request -> IO Response
handleCallTool req =
    case params req of
        Just paramObj -> resolveTool (requestId req) paramObj
        Nothing -> return $ Response (requestId req) $ Left $ RPCError (-32602) "No arguments given"
