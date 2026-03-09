{-# LANGUAGE OverloadedStrings #-}

module Dispatch (dispatchTool) where

import Response

type RequestId = Maybe Int
type MaybeTopic = Maybe String 
type Tool = (Maybe Int -> String -> IO Response)

dispatchTool :: RequestId -> MaybeTopic -> Tool -> IO Response
dispatchTool reqId mTopic tool =
    case mTopic of
        Nothing -> return $ Response reqId $ Left $ RPCError (-32602) "Invalid params"
        Just topic -> tool reqId topic
