{-# LANGUAGE OverloadedStrings #-}

module ToolResolver where

import Dispatch
import ArgumentExtractor
import Request
import Response
import Wikipedia

resolveTool :: Int -> Parameters -> IO Response
resolveTool reqId p =
    case toolName p of
        "wikipedia_summary" -> dispatch getWikipediaSummary
        "wikipedia_languages" -> dispatch getWikipediaLanguages
        "wikipedia_history" -> dispatch getWikipediaHistory
        _ -> return $ Response (Just reqId) $ Left $ RPCError (-32601) "Unrecognized tool"
  where
    dispatch tool = dispatchTool (Just reqId) (extractTopic p) tool
