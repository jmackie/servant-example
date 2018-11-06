{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Module      : Server
-- Description : Entrypoint for the API
--
module Server
    ( run
    , runWith
    , application
    )
where

import Prelude

import qualified API
import qualified Control.Exception as Exception
import qualified Data.Config as Config
import qualified Data.FileEmbed as FileEmbed
import qualified Data.List as List
import qualified Database
import qualified Middleware.Debugging
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import qualified System.Exit as Exit
import qualified WaiAppStatic.Types as Static (Piece, unsafeToPiece)

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.Config (Config)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)


data Error
    = InitializationError InitializationError


instance Show Error where
    show = \case
        InitializationError err ->
            "initialization error: " <> show err


instance Exception Error


data InitializationError
    -- | Error gathering up the configuration
    = ConfigResolveError Config.Error

    -- | Error connecting to the database.
    | DatabaseConnectError Database.ConnectError

    -- | Error initializing the database.
    | DatabaseInitError Database.Error


instance Show InitializationError where
    show = \case
        ConfigResolveError err ->
            "error resolving configuration: " <> show err

        DatabaseConnectError err ->
            "error connecting to the database: " <> show err

        DatabaseInitError err ->
            "error initializing the database: " <> show err


instance Exception InitializationError


run :: IO ()
run = handleError (run' Nothing)


runWith :: Config -> IO ()
runWith config = handleError (run' (Just config))


handleError :: IO () -> IO ()
handleError ma = do
    result <- Exception.try ma
    case result of
        Left  err -> Exit.die (showError err)
        Right _   -> Exit.exitSuccess


-- | Actually run the server, possibly throwing an 'Error'.
run' :: Maybe Config -> IO ()
run' config' = do
    config <- maybe (Config.resolve ?! configResolveError) pure config'
    let connectionInfo = Config.databaseConnectionInfo config
    Exception.bracket
        (Database.connect connectionInfo ?! databaseConnectError)
        (Database.close)
        (\connection' -> do
            connection <- Database.initialize connection' ?! databaseInitError
            let stuff = API.Stuff config connection
            Warp.run (fromIntegral (Config.port config))
                     (middleware config $ application stuff)
        )
  where
    configResolveError   = InitializationError . ConfigResolveError
    databaseConnectError = InitializationError . DatabaseConnectError
    databaseInitError    = InitializationError . DatabaseInitError


application :: API.Stuff -> Wai.Application
application stuff request respond = case Wai.pathInfo request of
    (dir@"docs" : _) -> swagger dir request respond
    _                -> api stuff request respond


api :: API.Stuff -> Wai.Application
api stuff = Servant.serveWithContext (Proxy :: Proxy API.API)
                                     API.context
                                     (API.server stuff)


-- | The middleware stack.
middleware :: Config.Config -> Wai.Application -> Wai.Application
middleware config = Middleware.Debugging.middleware config


swagger :: Text -> Wai.Application
swagger dir' = Static.staticApp settings
  where
    settings :: Static.StaticSettings
    settings = defaultSettings
        { Static.ssRedirectToIndex  = False
        , Static.ssAddTrailingSlash = True
        , Static.ssLookupFile       =
            \pieces -> Static.ssLookupFile defaultSettings
                                           (List.dropWhile (== dir) pieces)
        }

    defaultSettings :: Static.StaticSettings
    defaultSettings = Static.embeddedSettings docs

    dir :: Static.Piece
    dir = Static.unsafeToPiece dir'


docs :: [(FilePath, ByteString)]
docs = $(FileEmbed.embedDir "swagger/dist")


(?!) :: Exception e' => ExceptT e IO a -> (e -> e') -> IO a
(?!) e f = runExceptT e >>= either (Exception.throwIO . f) pure


-- | For type inference.
showError :: Error -> String
showError = show
