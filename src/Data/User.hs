{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module      : Data.Users
-- Description : Users of our application
--
module Data.User
    ( User
    , new

    -- * Usernames
    , Name
    , mkName
    , unwrapName

    -- * Accessors
    , id
    , username
    , posts
    , modifyPosts
    , setPosts
    , friends
    , setFriends
    , modifyFriends
    )
where

import Prelude hiding (id)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Char as Char
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import qualified Test.QuickCheck as QuickCheck

import Control.Lens ((.~), (?~))
import Control.Monad ((>=>))
import Data.Aeson ((.:), (.=))
import Data.Function ((&))
import Data.Post (Post)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)


data User id pid = User
    { _id       :: id
    , _username :: Name
    , _posts    :: [Post pid]
    , _friends  :: [id]
    }
    deriving stock (Generic, Show)


instance (Aeson.FromJSON id, Aeson.FromJSON pid) => Aeson.FromJSON (User id pid) where
    parseJSON :: Aeson.Value -> Aeson.Parser (User id pid)
    parseJSON = Aeson.withObject "User" $ \object ->
        User
            <$> object .: "id"
            <*> object .: "username"
            <*> object .: "posts"
            <*> object .: "friends"


instance (Aeson.ToJSON id, Aeson.ToJSON pid) => Aeson.ToJSON (User id pid) where
    toEncoding :: User id pid -> Aeson.Encoding
    toEncoding user = Aeson.pairs $ mconcat
        [ "id"        .= _id user
        , "username"  .= _username user
        , "posts"     .= _posts user
        , "friends"   .= _friends user
        ]


instance (Swagger.ToSchema id, Swagger.ToSchema pid) => Swagger.ToSchema (User id pid) where
    declareNamedSchema _ = do
        idSchema        <- Swagger.declareSchemaRef (Proxy :: Proxy id)
        nameSchema      <- Swagger.declareSchemaRef (Proxy :: Proxy Name)
        postsSchema     <- Swagger.declareSchemaRef (Proxy :: Proxy [Post pid])
        friendsSchema   <- Swagger.declareSchemaRef (Proxy :: Proxy [id])
        -- TODO: fix friends schema example

        pure . Swagger.NamedSchema (Just "User") $ mempty
            & Swagger.type_      .~ Swagger.SwaggerObject
            & Swagger.properties .~ [ ("id",        idSchema)
                                    , ("username",  nameSchema)
                                    , ("posts",     postsSchema)
                                    , ("friends", friendsSchema)
                                    ]
            & Swagger.required   .~ [ "id"
                                    , "username"
                                    , "posts"
                                    , "friends"
                                    ]


-- | Create a new user.
new :: id -> Name -> User id pid
new id' name' = User id' name' mempty mempty


-- USERNAME LOGIC


newtype Name = Name Text
    deriving newtype (Show, Eq, Ord, Aeson.ToJSON)


instance QuickCheck.Arbitrary Name where
    arbitrary :: QuickCheck.Gen Name
    arbitrary = Name . Text.pack <$>
        QuickCheck.listOf1
            (QuickCheck.suchThat QuickCheck.arbitrary validNameChar)
      where
        validNameChar :: Char -> Bool
        validNameChar = Char.isAlpha


instance Aeson.FromJSON Name where
    parseJSON :: Aeson.Value -> Aeson.Parser Name
    parseJSON = Aeson.parseJSON >=> either fail pure . mkName


instance Swagger.ToSchema Name where
    declareNamedSchema _ =
        pure . Swagger.NamedSchema Nothing $ mempty
            & Swagger.type_       .~ Swagger.SwaggerString
            & Swagger.description ?~ "Non-empty unique username"
            & Swagger.example     ?~ "foo_bar"


mkName :: Text -> Either String Name
mkName ""   = Left "username cannot be empty"
mkName text = Right (Name text)


unwrapName :: Name -> Text
unwrapName (Name text) = text


-- USER ACCESSORS


-- | Get a user's unique ID.
id :: User id pid -> id
id = _id


-- | Get a user's unique username.
username :: User id pid -> Name
username = _username


-- | Get the posts published by a user.
posts :: User id pid -> [Post pid]
posts = _posts


modifyPosts :: ([Post pid] -> [Post pid]) -> User id pid -> User id pid
modifyPosts f user = user { _posts = f (_posts user) }


setPosts :: [Post pid] -> User id pid -> User id pid
setPosts = modifyPosts . const


-- | Get friends of a 'User'.
friends :: User id pid -> [id]
friends = _friends


setFriends :: [id] -> User id pid -> User id pid
setFriends = modifyFriends . const


modifyFriends :: ([id] -> [id]) -> User id pid -> User id pid
modifyFriends f user = user { _friends = f (_friends user) }
