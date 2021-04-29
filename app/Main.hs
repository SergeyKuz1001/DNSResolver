module Main where

import QueryResponse
import MonadDebug
import MonadBind
import MonadStorage

import Control.Monad (forever)

main :: IO ()
main = do
    withDebug (Just High) $ do
        debugTrace Low "Server started"
        withBind 5302 $ do
            debugTrace Middle "Binding was success"
            withStorage defaultStorage $
                forever $ do
                    debugTrace Middle "Recv"
                    query <- recvB
                    debugTrace Middle "Load"
                    answer <- load query
                    debugTrace Middle "Send"
                    sendB $ R answer [] []
        debugTrace Low "Server finished"
