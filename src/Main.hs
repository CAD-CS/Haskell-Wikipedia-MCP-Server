{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, IOException)
import System.IO
import System.IO.Error (isEOFError)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Response
import Request
import Router

main :: IO ()
main = do
    setLineBuffering
    hPutStrLn stderr "MCP server started, waiting for input..."
    run

run :: IO ()
run = loop `catch` handleEOF
  where
    loop = do
        line <- BS.getLine
        res <- parseAndHandle line
        case res of
            Just r -> sendToClient r
            Nothing -> return ()
        loop
    handleEOF :: IOException -> IO ()
    handleEOF e
        | isEOFError e = hPutStrLn stderr "Client disconnected, shutting down."
        | otherwise = ioError e

parseAndHandle :: BS.ByteString -> IO (Maybe Response)
parseAndHandle line =
    case decodeStrict line :: Maybe Request of
        Just req -> routeRequest req
        Nothing -> return $ Just $ Response Nothing $ Left $ RPCError 400 "Bad Request"

sendToClient :: Response -> IO ()
sendToClient res = BSL.hPutStrLn stdout $ encode res

setLineBuffering :: IO ()
setLineBuffering = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
