{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Config
-- Description : Application configuration
--
module Data.Config
    ( Config
    , Config'
    , Error
    , Environment
        ( Development
        , Staging
        , Production
        )
    , resolve
    , new

    -- * Accessors
    , port
    , databaseConnectionInfo
    , environment
    )
where

import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Database
import qualified Options.Applicative as Opt
import qualified System.Environment as Environment

import Control.Applicative (optional, (<**>), (<|>))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:?))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Text.Read (readMaybe)


type family Setting (f :: * -> *) a where
    Setting Identity a = a
    Setting Maybe    a = Maybe a


data Config' f = Config
    { _port             :: Setting f Word16
    , _env              :: Setting f Environment
    , _databaseHost     :: Setting f ByteString
    , _databasePort     :: Setting f Word16
    , _databaseUser     :: Setting f ByteString
    , _databasePassword :: Setting f ByteString
    , _databaseName     :: Setting f ByteString
    } deriving Generic


type Config = Config' Identity
deriving instance Show Config


type PartialConfig = Config' Maybe
deriving instance Show PartialConfig


instance Semigroup PartialConfig where
    c1 <> c2 = Config
        { _port             = _port c1             <|> _port c2
        , _env              = _env c1              <|> _env c2
        , _databaseHost     = _databaseHost c1     <|> _databaseHost c2
        , _databasePort     = _databasePort c1     <|> _databasePort c2
        , _databaseUser     = _databaseUser c1     <|> _databaseUser c2
        , _databasePassword = _databasePassword c1 <|> _databasePassword c2
        , _databaseName     = _databaseName c1     <|> _databaseName c2
        }


instance Monoid PartialConfig where
    mempty = Config
        { _port             = Nothing
        , _env              = Nothing
        , _databaseHost     = Nothing
        , _databasePort     = Nothing
        , _databaseUser     = Nothing
        , _databasePassword = Nothing
        , _databaseName     = Nothing
        }


instance Aeson.FromJSON PartialConfig where
    parseJSON :: Aeson.Value -> Aeson.Parser PartialConfig
    parseJSON = Aeson.withObject "Config" $ \object -> do
        _port             <- object .:? "port"
        _env              <- object .:? "environment" >>= parseEnv
        _databaseHost     <- fmap Text.encodeUtf8 <$> parseDatabase object "host"
        _databasePort     <- parseDatabase object "port"
        _databaseUser     <- fmap Text.encodeUtf8 <$> parseDatabase object "user"
        _databasePassword <- fmap Text.encodeUtf8 <$> parseDatabase object "password"
        _databaseName     <- fmap Text.encodeUtf8 <$> parseDatabase object "name"
        pure Config {..}
      where
        parseEnv :: Maybe Text -> Aeson.Parser (Maybe Environment)
        parseEnv Nothing = pure Nothing
        parseEnv (Just rawEnv) =
            case readEnvironment (CI.mk rawEnv) of
                Nothing  -> fail ("invalid environment: " <> Text.unpack rawEnv)
                Just env -> pure (Just env)

        parseDatabase
            :: Aeson.FromJSON a
            => Aeson.Object
            -> Text
            -> Aeson.Parser (Maybe a)
        parseDatabase object key =
            object .:? "database" >>= \case
                Nothing -> pure Nothing
                Just database -> database .:? key


-- | Errors raised by this module.
data Error
    -- | Couldn't decode the config file
    = FileDecodeError FilePath String
    -- | Missing option for which there is no sensible default
    | MissingSetting String


instance Show Error where
    show = \case
        FileDecodeError path why ->
            "couldn't decode " <> path <> ": " <> why

        MissingSetting what ->
            "no valid configuration provided for " <> what


finalize :: forall m . MonadError Error m => PartialConfig -> m Config
finalize partial = do
    _port             <- withDefault 8080 (_port partial)
    _env              <- withDefault Development (_env partial)
    _databaseHost     <- withDefault "localhost" (_databaseHost partial)
    _databasePort <- withDefault Database.defaultPort (_databasePort partial)
    _databaseUser     <- required "database.user" (_databaseUser partial)
    _databasePassword <- withDefault "" (_databasePassword partial)
    _databaseName     <- required "database.name" (_databaseName partial)
    pure Config {..}
  where
    withDefault :: a -> Maybe a -> m a
    withDefault def = liftEither . maybe (Right def) Right

    required :: String -> Maybe a -> m a
    required name Nothing  = liftEither . Left . MissingSetting $ name
    required _    (Just a) = liftEither . Right $ a


resolve :: (MonadIO m, MonadError Error m) => m Config
resolve = do
    configEnv                    <- fromEnv
    (configArgs, configFilePath) <- fromArgs
    configFile                   <- maybe (pure mempty) fromFile configFilePath
    finalize . mconcat $ [configArgs, configFile, configEnv]


fromArgs :: forall m . MonadIO m => m (PartialConfig, Maybe FilePath)
fromArgs = liftIO (Opt.execParser args)
  where
    args :: Opt.ParserInfo (PartialConfig, Maybe FilePath)
    args = Opt.info (parser <**> Opt.helper) $ mconcat

        [ Opt.fullDesc
        , Opt.header "https://github.com/jmackie/haskell-config-example"
        ]

    parser :: Opt.Parser (PartialConfig, Maybe FilePath)
    parser = (,) <$> parseConfig <*> parseConfigFilePath

    parseConfig :: Opt.Parser PartialConfig
    parseConfig =
        (\_port _env -> mempty { _port = _port, _env = _env })
            <$> optional
                    (Opt.option Opt.auto $ mconcat
                        [ Opt.long "port"
                        , Opt.metavar "PORT"
                        , Opt.help "Port number to serve on"
                        ]
                    )
            <*> optional
                    (Opt.option Opt.auto $ mconcat
                        [ Opt.long "environment"
                        , Opt.metavar "ENV"
                        , Opt.help "Environment the aplication is running in"
                        ]
                    )

    parseConfigFilePath :: Opt.Parser (Maybe FilePath)
    parseConfigFilePath = optional
        (Opt.strOption $ mconcat
            [ Opt.long "config"
            , Opt.metavar "FILE"
            , Opt.help "Path to a config file (json)"
            ]
        )


fromEnv :: forall m . MonadIO m => m PartialConfig
fromEnv = do
    _port             <- fmap (>>= readMaybe) (lookupEnv "PORT")
    _env              <- fmap (>>= readEnvironment . CI.mk) (lookupEnv "ENV")
    _databaseHost     <- fmap (>>= readMaybe) (lookupEnv "DB_HOST")
    _databasePort     <- fmap (>>= readMaybe) (lookupEnv "DB_PORT")
    _databaseUser     <- fmap (>>= readMaybe) (lookupEnv "DB_USER")
    _databasePassword <- fmap (>>= readMaybe) (lookupEnv "DB_PASSWORD")
    _databaseName     <- fmap (>>= readMaybe) (lookupEnv "DB_NAME")
    pure Config {..}
  where
    lookupEnv :: String -> m (Maybe String)
    lookupEnv = liftIO . Environment.lookupEnv


fromFile :: (MonadIO m, MonadError Error m) => FilePath -> m PartialConfig
fromFile configFilePath = do
    result <- liftIO (Aeson.eitherDecodeFileStrict configFilePath)
    liftEither (Bifunctor.first (FileDecodeError configFilePath) result)


data Environment = Development | Staging | Production deriving Eq


instance Show Environment where
    show Development = "development"
    show Staging     = "staging"
    show Production  = "production"


instance Read Environment where
    readsPrec _ input =
        case readEnvironment (CI.mk input) of
            Just env -> [(env, "")]
            Nothing  -> []


readEnvironment
    :: (CI.FoldCase s, IsString s, Eq s) => CI.CI s -> Maybe Environment
readEnvironment s | isDevelopment = Just Development
                  | isStaging     = Just Staging
                  | isProduction  = Just Production
                  | otherwise     = Nothing
  where
    isDevelopment = s == CI.mk "development"
    isStaging     = s == CI.mk "staging"
    isProduction  = s == CI.mk "production"


new :: Word16 -> Environment -> Database.ConnectionInfo -> Config
new _port _env connectionInfo = Config
    { _port
    , _env
    , _databaseHost     = Database.connHost connectionInfo
    , _databasePort     = Database.connPort connectionInfo
    , _databaseUser     = Database.connUser connectionInfo
    , _databasePassword = Database.connPassword connectionInfo
    , _databaseName     = Database.connDatabase connectionInfo
    }


port :: Config -> Word16
port = _port


environment :: Config -> Environment
environment = _env


databaseConnectionInfo :: Config -> Database.ConnectionInfo
databaseConnectionInfo Config {..} = Database.ConnectionInfo
    { Database.connHost     = _databaseHost
    , Database.connPort     = _databasePort
    , Database.connUser     = _databaseUser
    , Database.connPassword = _databasePassword
    , Database.connDatabase = _databaseName
    }
