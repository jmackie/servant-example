{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module      : Data.Post
-- Description : Posts made by a user
--
module Data.Post
    ( Post
    , new
    , fake
    , content
    )
where

import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Lorem as Lorem

import Control.Lens ((.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), (.=))
import Data.Function ((&))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)


data Post id = Post
    { _id      :: id
    , _content :: Text
    }
    deriving stock (Generic, Show)


instance Aeson.FromJSON id => Aeson.FromJSON (Post id) where
    parseJSON :: Aeson.Value -> Aeson.Parser (Post id)
    parseJSON = Aeson.withObject "Post" $ \object ->
        Post
            <$> object .: "id"
            <*> object .: "content"


instance Aeson.ToJSON id => Aeson.ToJSON (Post id) where
    toEncoding :: Post id -> Aeson.Encoding
    toEncoding post = Aeson.pairs $ mconcat
        [ "content" .= _content post
        ]


fake :: MonadIO m => id -> m (Post id)
fake _id = Post _id <$> liftIO (QuickCheck.generate genContent)


genContent :: QuickCheck.Gen Text
genContent = Text.intercalate "\n\n" <$> QuickCheck.listOf1 paragraph
  where
    paragraph = Text.intercalate ". " <$> QuickCheck.listOf1 sentence
    sentence  = Text.intercalate " "
        <$> QuickCheck.listOf1 (QuickCheck.elements Lorem.words)


instance Swagger.ToSchema id => Swagger.ToSchema (Post id) where
    declareNamedSchema _ = do
        idSchema   <- Swagger.declareSchemaRef (Proxy :: Proxy id)
        textSchema <- Swagger.declareSchemaRef (Proxy :: Proxy Text)
        pure . Swagger.NamedSchema (Just "Post") $ mempty
            & Swagger.type_      .~ Swagger.SwaggerObject
            & Swagger.properties .~ [ ("id",      idSchema)
                                    , ("content", textSchema)
                                    ]
            & Swagger.required   .~ [ "id"
                                    , "content"
                                    ]


new :: id -> Text -> Post id
new = Post


content :: Post id -> Text
content = _content
