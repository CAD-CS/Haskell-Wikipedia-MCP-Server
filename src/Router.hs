{-# LANGUAGE OverloadedStrings #-}

module Router where

import Handlers
import Request
import Response

routeRequest :: Request -> IO (Maybe Response)
routeRequest req =
    case method req of
        "initialize" -> return $ Just $ handleInitialize req
        "tools/list" -> return $ Just $ handleListTools req
        "tools/call" -> Just <$> handleCallTool req
        "notifications/initialized" -> return Nothing
        _ -> return $ Just $ Response (requestId req) $ Left $ RPCError (-32601) "Method not found"
