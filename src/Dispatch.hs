{-# LANGUAGE OverloadedStrings #-}

module Dispatch where

import Response

dispatchTool :: Maybe Int -> Maybe String -> (Maybe Int -> String -> IO Response) -> IO Response
dispatchTool reqId mTopic tool =
    case mTopic of
        Nothing -> return $ Response reqId $ Left $ RPCError (-32602) "Invalid params"
        Just topic -> tool reqId topic
