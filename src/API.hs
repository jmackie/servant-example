{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- Module      : API
-- Description : Types for this API
--
module API
    ( API
    , UsersAPI
    , Stuff
        ( Stuff
        , config
        , connection
        )
    , server
    , context
    )
where

import Prelude

import qualified Auth
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader.Class as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import qualified Data.Nat as Nat
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import qualified Data.User as User
import qualified Data.Version as Version
import qualified Database
import qualified Network.Wai as Wai
import qualified Paths_servant_example as CabalFile
import qualified Servant
import qualified Servant.Foreign
import qualified Servant.JS
import qualified Servant.Server.Experimental.Auth as Servant
    ( AuthHandler
    , AuthServerData
    , mkAuthHandler
    )
import qualified Servant.Swagger

import Control.Lens ((%~), (.~), (?~))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Config (Config)
import Data.Function ((&))
import Data.Nat (Nat)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (Swagger)
import Data.Text (Text)
import Servant
    ( (:<|>)(..)
    , (:>)
    , Capture
    , Description
    , Get
    , JSON
    , PlainText
    , Post
    , ReqBody
    , Summary
    )
import Servant.Extras (ErrorResponse)


-- | API type.
type API = UsersAPI :<|> SwaggerAPI :<|> CodeGenAPI


type UsersAPI = PublicUsersAPI :<|> PrivateUsersAPI


type PublicUsersAPI = GetUserCount


type PrivateUsersAPI = Servant.AuthProtect "cookie-auth" :> (
        CreateNewUser :<|> GetAllUsers
    )


type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger


type CodeGenAPI = "code" :> Capture "target" String :> Get '[PlainText] Text


-- | API implementation.
server :: Stuff -> Servant.Server API
server stuff = Servant.hoistServerWithContext
    (Proxy :: Proxy API)
    (Proxy :: Proxy '[CookieAuthHandler])
    (toServantHandler stuff)
    (usersServer :<|> swaggerServer :<|> codeGenServer)


-- AUTHENTICATION AND HANDLERS


-- | Server context.
type Context =
    Servant.Context '[CookieAuthHandler]


-- | Server context implementation(s).
context :: Context
context = cookieAuthHandler Servant.:. Servant.EmptyContext


type CookieAuthHandler =
    Servant.AuthHandler Wai.Request Auth.Credentials


type instance Servant.AuthServerData (Servant.AuthProtect "cookie-auth") =
    Auth.Credentials


-- | Cookie authentication.
cookieAuthHandler :: CookieAuthHandler
cookieAuthHandler = Servant.mkAuthHandler $ \request -> do
    result <- liftIO (Auth.authenticate request)
    case result of
        Nothing -> throwError Servant.err401
            { Servant.errBody = "Authentication failed. No API for you."
            }
        Just credentials -> pure credentials


-- | Our 'Handler' Monad.
type Handler =
    ReaderT Stuff (ExceptT Servant.ServantErr IO)


-- | Stuff that handlers will need access to.
data Stuff = Stuff
    { config     :: Config
    , connection :: Database.Connection 'Database.Initialized
    }


-- | Convert our 'Handler' type to servant's 'Handler' type.
toServantHandler :: Stuff -> Handler a -> Servant.Handler a
toServantHandler stuff handler = Servant.Handler (runReaderT handler stuff)


-- MORE TYPES


-- | Wrapper around a 'Nat' in order to provide nicer docs.
newtype Count = Count Nat
    deriving Aeson.ToJSON


-- | Count elements in a 'List'.
count :: [a] -> Count
count = Count . Nat.length


instance Swagger.ToSchema Count where
    declareNamedSchema _ = do
        natSchema <- Swagger.declareNamedSchema (Proxy :: Proxy Nat)
        pure $ natSchema
            & Swagger.schema %~
                (Swagger.description ?~ "Probably a really big number")


-- | Supported code generation targets.
data CodeGenTarget
    = VanillaJavascript


readCodeGenTarget :: String -> Maybe CodeGenTarget
readCodeGenTarget = \case
    "js"         -> Just VanillaJavascript
    "javascript" -> Just VanillaJavascript
    _            -> Nothing


-- USERS API IMPLEMENTATION


usersServer :: Servant.ServerT UsersAPI Handler
usersServer = publicHandlers :<|> privateHandlers
  where
    publicHandlers :: Servant.ServerT PublicUsersAPI Handler
    publicHandlers = getUserCount

    privateHandlers :: Servant.ServerT PrivateUsersAPI Handler
    privateHandlers auth = createNewUser auth :<|> getAllUsers auth


type GetUserCount =
    Summary "How many users we have" :>
    Description "We're a pretty big deal and have lots of users \
    \Give us your ad budget plz." :>
    ErrorResponse 500 "The database exploded" :>
    ---------------------------------------
    "users" :> "count" :> Get '[JSON] Count


getUserCount :: Handler Count
getUserCount = do
    runQuery Database.selectAllUsers >>= \case
        Left err -> throwError Servant.err500
            { Servant.errBody = serializeDatabaseError err
            }
        Right users -> pure (count users)


type CreateNewUser =
    Summary "Create a new (blank) user with the given user name." :>
    ErrorResponse 401 "Missing or inapproprate authorization" :>
    ErrorResponse 409 "Username is already taken" :>
    ErrorResponse 500 "The database exploded" :>
    ------------------------------------------------------------------
    "users" :> "new" :> ReqBody '[JSON] User.Name :> Post '[JSON] Database.User


createNewUser :: Auth.Credentials -> User.Name -> Handler Database.User
createNewUser auth userName = case auth of
    Auth.YoloCredentials   -> createNewUser' userName

    Auth.UserCredentials _ -> throwError Servant.err401
        { Servant.errBody = "existing users can't create other users, soz"
        }

    Auth.TemporaryCredentials -> createNewUser' userName


createNewUser' :: User.Name -> Handler Database.User
createNewUser' userName = do
    runQuery (Database.checkUserNameAvailability userName) >>= \case
        Left err -> throwError Servant.err500
            { Servant.errBody = serializeDatabaseError err
            }

        Right Nothing -> throwError Servant.err409
            { Servant.errBody = "username "
                                <> LBS.pack (show userName)
                                <> " is unavailable"
            }

        Right (Just availableUserName) ->
            runQuery (Database.createNewUser availableUserName) >>= \case
                Left err -> throwError Servant.err500
                    { Servant.errBody = serializeDatabaseError err
                    }
                Right user -> pure user


type GetAllUsers =
    Summary "Query all the users" :>
    ErrorResponse 401 "Missing or inappropriate authorization" :>
    ErrorResponse 500 "The database exploded" :>
    --------------------------------------
    "users" :> "all" :> Get '[JSON] [Database.User]


getAllUsers :: Auth.Credentials -> Handler [Database.User]
getAllUsers = \case
    Auth.YoloCredentials   -> getAllUsers'

    Auth.UserCredentials _ -> throwError Servant.err401
        { Servant.errBody =
            "need YOLO credentials to view all users: you're a user"
        }

    Auth.TemporaryCredentials -> throwError Servant.err401
        { Servant.errBody =
            "need YOLO credentials to view all users: you're a temporary user"
        }


getAllUsers' :: Handler [Database.User]
getAllUsers' = do
    runQuery Database.selectAllUsers >>= \case
        Left err -> throwError Servant.err500
            { Servant.errBody = serializeDatabaseError err
            }
        Right users -> pure users


-- SWAGGER


swaggerServer :: Servant.ServerT SwaggerAPI Handler
swaggerServer = pure swagger


swagger :: Swagger
swagger =
    Servant.Swagger.toSwagger (Proxy :: Proxy UsersAPI)
        & (Swagger.info .~ swaggerInfo)
        & (Swagger.applyTagsFor
              userOperations
              [ "Users API" & Swagger.description ?~ Text.intercalate
                    " "
                    [ "Find out stuff about our users."
                    , "Fully GDPR complicant. We think."
                    ]
              ]
          )
  where
    userOperations :: Lens.Traversal' Swagger Swagger.Operation
    userOperations = Servant.Swagger.subOperations
        (Proxy :: Proxy UsersAPI)
        (Proxy :: Proxy API)


swaggerInfo :: Swagger.Info
swaggerInfo =
    mempty
        & (Swagger.title .~ "Servant API Example")
        & (Swagger.description ?~ "A silly social network example.")
        & (Swagger.version .~ version)


version :: Text
version = Text.pack (Version.showVersion CabalFile.version)


-- CODEGEN


codeGenServer :: Servant.ServerT CodeGenAPI Handler
codeGenServer = codeGen


codeGen :: String -> Handler Text
codeGen rawTarget = case readCodeGenTarget rawTarget of
    Nothing -> throwError Servant.err401
        { Servant.errBody = LBS.pack rawTarget <> " is not currently supported"
        }
    Just target -> codeGen' target


codeGen' :: CodeGenTarget -> Handler Text
codeGen' = \case
    VanillaJavascript -> codeGenVanillaJavascript


codeGenVanillaJavascript :: Handler Text
codeGenVanillaJavascript =
    pure (Servant.JS.jsForAPI (Proxy :: Proxy UsersAPI) Servant.JS.vanillaJS)



-- UTIL


runQuery
    :: (Database.Connection 'Database.Initialized -> ExceptT e IO a)
    -> Handler (Either e a)
runQuery query = do
    conn   <- Reader.asks connection
    result <- liftIO (runExceptT (query conn))
    pure result


serializeDatabaseError :: Database.Error -> LBS.ByteString
serializeDatabaseError = LBS.pack . show


-- ORPHAN INSTANCES


instance Servant.Swagger.HasSwagger api =>
         Servant.Swagger.HasSwagger (Servant.AuthProtect "cookie-auth" :> api) where
    toSwagger _ =
        Servant.Swagger.toSwagger (Proxy :: Proxy api)


instance Servant.Foreign.HasForeign lang ftype api =>
         Servant.Foreign.HasForeign lang ftype (Servant.AuthProtect "cookie-auth" :> api) where

    type Foreign ftype (Servant.AuthProtect "cookie-auth" :> api) =
        Servant.Foreign.Foreign ftype api

    foreignFor lang ftype Proxy req =
        Servant.Foreign.foreignFor lang ftype (Proxy :: Proxy api) req
