{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module      : Data.Nat
-- Description : Term-level natural numbers
--
module Data.Nat
    ( Nat
    , Data.Nat.fromIntegral
    , toIntegral
    , zero
    , Data.Nat.succ
    , length
    )
where

import Prelude hiding (length)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson (word64)
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Swagger as Swagger
import qualified Test.QuickCheck as QuickCheck

import Control.Lens ((.~), (?~))
import Data.Function ((&))
import Data.Word (Word64)
import GHC.Generics (Generic)


-- | A non-negative integer.
newtype Nat = Nat Word64
    deriving stock   (Generic)
    deriving newtype (Show, Eq, Ord)


instance Aeson.FromJSON Nat where
    parseJSON :: Aeson.Value -> Aeson.Parser Nat
    parseJSON = Aeson.withScientific "Nat" $ \n ->
        case Scientific.floatingOrInteger n of
             Left (_ :: Double) -> fail ("expecting an integral, got " <> show n)
             Right (i :: Integer) ->
                case Data.Nat.fromIntegral i of
                     Nothing  -> fail ("invalid natural number: " <> show i)
                     Just nat -> pure nat


instance Aeson.ToJSON Nat where
    toEncoding :: Nat -> Aeson.Encoding
    toEncoding (Nat nat) = Aeson.word64 nat


instance Swagger.ToSchema Nat where
    declareNamedSchema _ = do
        pure . Swagger.NamedSchema Nothing $ mempty
            & Swagger.type_       .~ Swagger.SwaggerInteger
            & Swagger.minimum_    ?~ 0
            & Swagger.example     ?~ Aeson.Number 42


instance QuickCheck.Arbitrary Nat where
    arbitrary = Nat <$> QuickCheck.choose (0, 100)


toIntegral :: Integral a => Nat -> a
toIntegral (Nat n) = Prelude.fromIntegral n


-- | Try and construct a 'Nat' from something like an integer.
fromIntegral :: Integral a => a -> Maybe Nat
fromIntegral a | a < 0     = Nothing
               | otherwise = Just . Nat . fromInteger . toInteger $ a


zero :: Nat
zero = Nat 0


succ :: Nat -> Nat
succ (Nat n) = Nat (n + 1)


length :: [a] -> Nat
length = Maybe.fromJust . Data.Nat.fromIntegral . List.length
--       ^^^^^^^^^^^^^^ Safe to use here
