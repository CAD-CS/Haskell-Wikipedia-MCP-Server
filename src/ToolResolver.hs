{-# LANGUAGE OverloadedStrings #-}

module ToolResolver where

import Dispatch
import ArgumentExtractor
import Request
import Response
import Wikipedia

resolveTool :: Maybe Int -> Parameters -> IO Response
resolveTool reqId p =
    case toolName p of
        Just "wikipedia_summary" -> dispatch getWikipediaSummary
        Just "wikipedia_languages" -> dispatch getWikipediaLanguages
        Just "wikipedia_history" -> dispatch getWikipediaHistory
        Just _ -> return $ Response reqId $ Left $ RPCError (-32601) "Unrecognized tool"
        Nothing -> return $ Response reqId $ Left $ RPCError (-32601) "Unrecognized tool"
  where
    dispatch tool = dispatchTool reqId (extractTopic p) tool
