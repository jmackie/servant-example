{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Auth
-- Description : Logic for API Authentication
--
module Auth
    ( Credentials
        ( UserCredentials
        , TemporaryCredentials
        , YoloCredentials
        )
    , authenticate
    )
where

import Prelude

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Nat as Nat
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

import Data.ByteString (ByteString)
import Text.Read (readMaybe)


data Credentials
    = UserCredentials Nat.Nat
    | TemporaryCredentials
    | YoloCredentials


credentialsFromCookie :: Cookie -> Maybe Credentials
credentialsFromCookie Cookie { cookieValue }
    | cookieValue == "yolo" = Just YoloCredentials
    | cookieValue == "temp" = Just TemporaryCredentials
    | otherwise = do
        -- "iam-2" == UserCredentials 2
        suffix     <- ByteString.stripPrefix "iam-" cookieValue
        (i :: Int) <- readMaybe (Data.ByteString.Char8.unpack suffix)
        nat        <- Nat.fromIntegral i
        pure (UserCredentials nat)


authenticate :: Wai.Request -> IO (Maybe Credentials)
authenticate request = pure (findAuthCookie request >>= credentialsFromCookie)


findAuthCookie :: Wai.Request -> Maybe Cookie
findAuthCookie = List.find ((== authCookieName) . cookieName) . extractCookies


authCookieName :: ByteString
authCookieName = "AUTH"


data Cookie = Cookie
    { cookieName  :: ByteString
    , cookieValue :: ByteString
    }


extractCookies :: Wai.Request -> [Cookie]
extractCookies =
    Maybe.catMaybes . List.concatMap filterCookies . Wai.requestHeaders
  where
    filterCookies :: HTTP.Header -> [Maybe Cookie]
    filterCookies (name, value)
        | name == HTTP.hCookie = fmap mkCookie (splitAndTrim ';' value)
        | otherwise            = []


-- | Create a cookie from a "cookieName=cookieValue" string.
mkCookie :: ByteString -> Maybe Cookie
mkCookie raw = case splitAndTrim '=' raw of
    [cookieName, cookieValue] -> Just (Cookie cookieName cookieValue)
    _                         -> Nothing



-- | Split on a character and trim leading and trailing whitespace from the
-- resulting pieces.
splitAndTrim :: Char -> ByteString -> [ByteString]
splitAndTrim c bs = fmap trim (Data.ByteString.Char8.split c bs)


trim :: ByteString -> ByteString
trim = trimRight . trimLeft


trimLeft :: ByteString -> ByteString
trimLeft = Data.ByteString.Char8.dropWhile (== ' ')


trimRight :: ByteString -> ByteString
trimRight = ByteString.reverse . trimLeft . ByteString.reverse
