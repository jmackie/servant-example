{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Middleware.Debugging
-- Description : Scrubs debugging info from requests in non-development environments
--
module Middleware.Debugging
    ( middleware
    )
where

import Prelude

import qualified Data.Config as Config
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


middleware :: Config.Config -> Wai.Middleware
middleware config
    | Config.environment config /= Config.Development = removeDebuggingInfo
    | otherwise = id


removeDebuggingInfo :: Wai.Middleware
removeDebuggingInfo application request respond =
    application request $ \response ->
        case HTTP.statusCode (Wai.responseStatus response) of
            200 -> respond response
            _   -> respond (removeResponseBody response)


removeResponseBody :: Wai.Response -> Wai.Response
removeResponseBody response = Wai.responseLBS
    (Wai.responseStatus response)
    (Wai.responseHeaders response)
    ""
