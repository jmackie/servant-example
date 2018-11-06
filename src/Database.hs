{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
-- |
-- Module      : Database
-- Description : Defines all database interactions
--
module Database
    ( State
        ( Open
        , Initialized
        , Closed
        )
    , Connection
    , ConnectError
    , ConnectionInfo
        ( ConnectionInfo
        , connHost
        , connPort
        , connUser
        , connPassword
        , connDatabase
        )
    , defaultPort
    , connect
    , close
    , initialize

    , ID
    , newID

    , User
    , NewUser
    , UserID
    , Post
    , NewPost
    , PostID

    -- ** Queries
    , Error
    , createNewUser
    , checkUserNameAvailability
    , selectAllUsers
    , selectUser
    , updateUser
    )
where

import Prelude hiding (id)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString (unpack)
import qualified Data.Map as Map
import qualified Data.Nat as Nat
import qualified Data.Post as Post
import qualified Data.Set as Set
import qualified Data.Swagger as Swagger
import qualified Data.User as User
import qualified Hasql.Connection
import qualified Hasql.Decoders
import qualified Hasql.Encoders
import qualified Hasql.Session
import qualified Hasql.Statement
import qualified Test.QuickCheck as QuickCheck

import Control.Exception (Exception)
import Control.Lens ((%~), (?~))
import Control.Monad (foldM, replicateM)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor (($>))
import Data.Functor.Contravariant (contramap)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Int (Int64)


-- | The state of a database connection.
data State = Open | Initialized | Closed


-- | The actual database connection.
newtype Connection (s :: State) =
    Connection Hasql.Connection.Connection


-- | An error that might occur while connecting to the database.
newtype ConnectError =
    ConnectError (Maybe ByteString)


instance Show ConnectError where
    show (ConnectError why) =
        "failed to connect to the database: "
        <> maybe "don't know why" ByteString.unpack why


instance Exception ConnectError


-- | An error that might occur while querying the database.
data Error
    = DatabaseError Hasql.Session.QueryError
    | UpdateUserError UpdateUserError
    | MappingError String


instance Show Error where
    show = \case
        DatabaseError (Hasql.Session.QueryError _ _ commandError) ->
            -- TODO
            addContext "database error" (show commandError)

        UpdateUserError err ->
            addContext "error updating user" (show err)

        MappingError why ->
            addContext "error mapping data" why


instance Exception Error


data UpdateUserError
    = UserDoesNotExist UserID


instance Show UpdateUserError where
    show = \case
        UserDoesNotExist userID ->
            "user " <> show userID <> "does not exist"


instance Exception UpdateUserError


-- | Some raw SQL.
type SQL = ByteString


-- | Stuff we need to know in order to open a connection.
data ConnectionInfo = ConnectionInfo
    { connHost     :: ByteString  -- ^ "localhost"
    , connPort     :: Word16      -- ^ 5432
    , connUser     :: ByteString  -- ^ "foobar"
    , connPassword :: ByteString  -- ^ "rllysafe"
    , connDatabase :: ByteString  -- ^ database name
    } deriving Show


-- | The default port for our database.
defaultPort :: Word16
defaultPort = 5432  -- default Postgres port


-- | Map 'ConnectionInfo' to hasql settings.
toSettings :: ConnectionInfo -> Hasql.Connection.Settings
toSettings ConnectionInfo {..} = Hasql.Connection.settings connHost
                                                           connPort
                                                           connUser
                                                           connPassword
                                                           connDatabase


type User = User.User UserID PostID


type NewUser = User.User () PostID


type Post = Post.Post PostID


type NewPost = Post.Post ()


-- | Open a database connection.
connect
    :: (MonadIO m, MonadError ConnectError m)
    => ConnectionInfo
    -> m (Connection 'Open)
connect connectionInfo =
    liftIO (Hasql.Connection.acquire $ toSettings connectionInfo)
        >>= either (throwError . ConnectError) (pure . Connection)


-- | Table schema.
--
-- This is here mostly for reference.
schema :: [SQL]
schema = [usersTable, postsTable]
  where
    usersTable :: SQL
    usersTable = mconcat
        [ "CREATE TABLE IF NOT EXISTS users("
        , "id      serial    primary key,"
        , "name    text      unique not null,"
        , "friends integer[]"
        , ")"
        ]

    postsTable :: SQL
    postsTable = mconcat
        [ "CREATE TABLE IF NOT EXISTS posts("
        , "id       serial    primary key,"
        , "user_id  integer   references users(id),"
        , "content  text      not null"
        , ")"
        ]


-- | Initialize the database.
initialize
    :: (MonadIO m, MonadError Error m)
    => Connection 'Open
    -> m (Connection 'Initialized)
initialize (Connection conn) =
    traverse_ (execute_ (Connection conn) . statement_) schema
        $> Connection conn


-- | Close database connection.
close :: MonadIO m => Connection 'Open -> m (Connection 'Closed)
close (Connection conn) = do
    liftIO (Hasql.Connection.release conn)
    pure (Connection conn)


-- QUERIES


createNewUser
    :: (MonadIO m, MonadError Error m)
    => AvailableUserName
    -> Connection 'Initialized
    -> m User
createNewUser (AvailableUserName userName) conn = do
    let _userRowName :: Text
        _userRowName = User.unwrapName userName

        _userRowFriends :: [Int64]
        _userRowFriends = []

    _userRowID <- execute conn
                          createNewUserStatement
                          (_userRowName, _userRowFriends)

    liftError MappingError $ mapUserRow [] UserRow {..}


checkUserNameAvailability
    :: (MonadIO m, MonadError Error m)
    => User.Name
    -> Connection 'Initialized
    -> m (Maybe AvailableUserName)
checkUserNameAvailability userName conn = do
    taken <- execute conn
                     checkUserNameAvailabilityStatement
                     (User.unwrapName userName)
    pure $ if taken then Nothing else Just (AvailableUserName userName)


-- | Select all users from the database.
selectAllUsers
    :: (MonadIO m, MonadError Error m) => Connection 'Initialized -> m [User]
selectAllUsers conn = do
    userRows <- execute conn selectAllUsersStatement ()
    postRows <- execute conn selectAllPostsStatement ()
    postMap  <- mkPostMap postRows
    processUserRows postMap userRows


-- | Select all users from the database.
selectUser
    :: (MonadIO m, MonadError Error m)
    => UserID
    -> Connection 'Initialized
    -> m (Maybe User)
selectUser (UserID id) conn =
    execute conn selectUserByIDStatement (unwrapID' id) >>= \case
        Nothing      -> pure Nothing
        Just userRow -> do
            postRows <- execute conn
                                selectPostsByUserIDStatement
                                (_userRowID userRow)
            liftError MappingError $ do
                posts <- traverse mapPostRow postRows
                user  <- mapUserRow posts userRow
                pure (Just user)


updateUser
    :: (MonadIO m, MonadError Error m)
    => User
    -> Connection 'Initialized
    -> m User
updateUser user conn = do
    rowsAffected <- execute conn updateUserStatement (mkUserRow user)
    case rowsAffected of
        0 -> throwError . UpdateUserError . UserDoesNotExist $ User.id user
        1 -> pure user
        _ -> undefined -- impossible (I hope...)


-- STATEMENTS


-- | Takes a username and friends list, returns an id.
createNewUserStatement :: Hasql.Statement.Statement (Text, [Int64]) Int64
createNewUserStatement = Hasql.Statement.Statement
    "INSERT INTO users (name, friends) VALUES ($1, $2) RETURNING id"
    params
    (Hasql.Decoders.singleRow (Hasql.Decoders.column Hasql.Decoders.int8))
    prepare
  where
    params :: Hasql.Encoders.Params (Text, [Int64])
    params = mconcat
        [ contramap fst (Hasql.Encoders.param Hasql.Encoders.text)
        , contramap
            snd
            (Hasql.Encoders.param
                (Hasql.Encoders.array
                    (Hasql.Encoders.dimension
                        foldl'
                        (Hasql.Encoders.element Hasql.Encoders.int8)
                    )
                )
            )
        ]

checkUserNameAvailabilityStatement :: Hasql.Statement.Statement Text Bool
checkUserNameAvailabilityStatement = Hasql.Statement.Statement
    "SELECT exists (SELECT 1 FROM users WHERE name = $1 LIMIT 1)"
    (Hasql.Encoders.param Hasql.Encoders.text)
    (Hasql.Decoders.singleRow (Hasql.Decoders.column Hasql.Decoders.bool))
    prepare


selectAllUsersStatement :: Hasql.Statement.Statement () [UserRow]
selectAllUsersStatement = Hasql.Statement.Statement
    "SELECT * FROM users"
    noParams
    (Hasql.Decoders.rowList userRowDecoder)
    noPrepare


selectUserByIDStatement :: Hasql.Statement.Statement Int64 (Maybe UserRow)
selectUserByIDStatement = Hasql.Statement.Statement
    "SELECT * FROM users WHERE id=$1"
    (Hasql.Encoders.param Hasql.Encoders.int8)
    (Hasql.Decoders.rowMaybe userRowDecoder)
    noPrepare


selectAllPostsStatement :: Hasql.Statement.Statement () [PostRow]
selectAllPostsStatement = Hasql.Statement.Statement
    "SELECT * FROM posts"
    noParams
    (Hasql.Decoders.rowList postRowDecoder)
    noPrepare


selectPostsByUserIDStatement :: Hasql.Statement.Statement Int64 [PostRow]
selectPostsByUserIDStatement = Hasql.Statement.Statement
    "SELECT * FROM posts WHERE user_id=$1"
    (Hasql.Encoders.param Hasql.Encoders.int8)
    (Hasql.Decoders.rowList postRowDecoder)
    noPrepare


updateUserStatement :: Hasql.Statement.Statement UserRow Int64
updateUserStatement = Hasql.Statement.Statement
    "UPDATE users SET name = $1, friends = $2 WHERE id = $3"
    params
    Hasql.Decoders.rowsAffected
    prepare
  where
    params :: Hasql.Encoders.Params UserRow
    params = mconcat
        [ contramap _userRowName (Hasql.Encoders.param Hasql.Encoders.text)
        , contramap
            _userRowFriends
            (Hasql.Encoders.param
                (Hasql.Encoders.array
                    (Hasql.Encoders.dimension
                        foldl'
                        (Hasql.Encoders.element Hasql.Encoders.int8)
                    )
                )
            )
        , contramap _userRowID (Hasql.Encoders.param Hasql.Encoders.int8)
        ]


-- QUERY HELPERS


mkPostMap
    :: forall m . MonadError Error m => [PostRow] -> m (Map.Map UserID [Post])
mkPostMap = foldM go Map.empty
  where
    go :: Map.Map UserID [Post] -> PostRow -> m (Map.Map UserID [Post])
    go accum postRow = liftError MappingError $ do
        userID <- mapUserID "invalid post user id" (_postRowUserID postRow)
        post   <- mapPostRow postRow
        pure (Map.insertWith (<>) userID [post] accum)


processUserRows
    :: forall m
     . MonadError Error m
    => Map.Map UserID [Post]
    -> [UserRow]
    -> m [User]
processUserRows postMap = foldM go []
  where
    go :: [User] -> UserRow -> m [User]
    go accum userRow = liftError MappingError $ do
        userID <- mapUserID "invalid user id" (_userRowID userRow)
        user   <- mapUserRow (Map.findWithDefault [] userID postMap) userRow
        pure (user : accum)


-- TYPES


data UserRow = UserRow
    {
    -- | id serial primary key
      _userRowID      :: Int64
    -- | name text not null unique
    , _userRowName    :: Text
    -- | friends integer[]
    , _userRowFriends :: [Int64]
    }


mkUserRow :: User -> UserRow
mkUserRow = undefined


userRowDecoder :: Hasql.Decoders.Row UserRow
userRowDecoder =
    UserRow
        <$> Hasql.Decoders.column Hasql.Decoders.int8
        <*> Hasql.Decoders.column Hasql.Decoders.text
        <*> Hasql.Decoders.column
                (Hasql.Decoders.array
                    (Hasql.Decoders.dimension
                        replicateM
                        (Hasql.Decoders.element Hasql.Decoders.int8)
                    )
                )


mapUserRow :: [Post] -> UserRow -> Either String User
mapUserRow posts UserRow {..} = do
    userID   <- mapUserID "invalid user id" _userRowID
    userName <- User.mkName _userRowName
    friends  <- traverse (mapUserID "invalid friend id") _userRowFriends
    pure
        ( User.new userID userName
        & User.setPosts posts
        & User.setFriends friends
        )


data PostRow = PostRow
    {
    -- | id serial primary key
      _postRowID      :: Int64
    -- | user_id int references users(id)
    , _postRowUserID  :: Int64
    -- | content text not null
    , _postRowContent :: Text
    }


postRowDecoder :: Hasql.Decoders.Row PostRow
postRowDecoder =
    PostRow
        <$> Hasql.Decoders.column Hasql.Decoders.int8
        <*> Hasql.Decoders.column Hasql.Decoders.int8
        <*> Hasql.Decoders.column Hasql.Decoders.text


mapPostRow :: PostRow -> Either String Post
mapPostRow = undefined


newtype AvailableUserName = AvailableUserName User.Name


-- UTIL


mapUserID :: String -> Int64 -> Either String UserID
mapUserID errorContext =
    bimap (addContext errorContext) (UserID . ID) . int64ToNat


addContext :: String -> String -> String
addContext context str = context <> ": " <> str


execute
    :: (MonadIO m, MonadError Error m)
    => Connection s
    -> Hasql.Statement.Statement a b
    -> a
    -> m b
execute (Connection conn) statement params = do
    liftIO (Hasql.Session.run (Hasql.Session.statement params statement) conn)
        >>= liftError DatabaseError


liftError :: MonadError e' m => (e -> e') -> Either e a -> m a
liftError f = either (throwError . f) pure


int64ToNat :: Int64 -> Either String Nat.Nat
int64ToNat int64 =
    note (show int64 <> "is not a natural number") (Nat.fromIntegral int64)


-- | Execute a void statment that encodes nothing and returns nothing.
execute_
    :: (MonadIO m, MonadError Error m)
    => Connection s
    -> Hasql.Statement.Statement () ()
    -> m ()
execute_ conn statement = execute conn statement ()


-- | Prepare a void statment that encodes nothing and returns nothing.
statement_ :: SQL -> Hasql.Statement.Statement () ()
statement_ sql = Hasql.Statement.Statement sql
                                           Hasql.Encoders.unit
                                           Hasql.Decoders.unit
                                           noPrepare


noParams :: Hasql.Encoders.Params ()
noParams = Hasql.Encoders.unit


prepare, noPrepare :: Bool
prepare = True
noPrepare = False


note :: String -> Maybe a -> Either String a
note str = maybe (Left str) Right


-- IDS


-- | A unique identifier.
newtype ID = ID { unwrapID :: Nat.Nat }
    deriving stock   ( Show
                     )
    deriving newtype ( Eq
                     , Ord
                     , Aeson.FromJSON
                     , Aeson.ToJSON
                     , QuickCheck.Arbitrary
                     )


instance Swagger.ToSchema ID where
    declareNamedSchema _ = do
        natSchema <- Swagger.declareNamedSchema (Proxy :: Proxy Nat.Nat)
        pure $ natSchema
            & Swagger.schema %~
                (Swagger.description ?~ "Non-zero unique identifier")


newID :: Set ID -> ID
newID = ID . Nat.succ . unwrapID . Set.findMax


unwrapID' :: Integral a => ID -> a
unwrapID' = Nat.toIntegral . unwrapID


newtype UserID = UserID ID
    deriving stock   ( Show
                     )
    deriving newtype ( Eq
                     , Ord
                     , Aeson.FromJSON
                     , Aeson.ToJSON
                     , QuickCheck.Arbitrary
                     )


instance Swagger.ToSchema UserID where
    declareNamedSchema _ = do
        natSchema <- Swagger.declareNamedSchema (Proxy :: Proxy Nat.Nat)
        pure $ natSchema
            & Swagger.schema %~
                (Swagger.description ?~ "Non-zero unique user identifier")


newtype PostID = PostID ID
    deriving stock   ( Show
                     )
    deriving newtype ( Eq
                     , Ord
                     , Aeson.FromJSON
                     , Aeson.ToJSON
                     , QuickCheck.Arbitrary
                     )


instance Swagger.ToSchema PostID where
    declareNamedSchema _ = do
        natSchema <- Swagger.declareNamedSchema (Proxy :: Proxy Nat.Nat)
        pure $ natSchema
            & Swagger.schema %~
                (Swagger.description ?~ "Non-zero unique post identifier")
