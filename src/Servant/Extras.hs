{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Servant.Extras
-- Description : Utilities that aren't provided by servant (yet)
--
module Servant.Extras
    ( ErrorResponse
    )
where

import Prelude

import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import qualified GHC.TypeLits as GHC
import qualified Servant
import qualified Servant.Foreign
import qualified Servant.Swagger as Servant (HasSwagger, toSwagger)

import Control.Lens ((.~))
import Data.Function ((&))
import Data.Proxy (Proxy(Proxy))
import Servant ((:>))


-- https://github.com/haskell-servant/servant-swagger/issues/59
data ErrorResponse (code :: GHC.Nat) (description :: GHC.Symbol)


instance (Servant.HasSwagger api, GHC.KnownNat code, GHC.KnownSymbol desc) =>
          Servant.HasSwagger (ErrorResponse code desc :> api) where

    toSwagger _ = Servant.toSwagger (Proxy :: Proxy api)
        & Swagger.setResponse (fromInteger code) (pure responseSchema)
      where
        code = GHC.natVal (Proxy :: Proxy code)
        desc = GHC.symbolVal (Proxy :: Proxy desc)
        responseSchema = mempty & Swagger.description .~ Text.pack desc


instance Servant.Foreign.HasForeign lang ftype api =>
         Servant.Foreign.HasForeign lang ftype (ErrorResponse code desc :> api) where

    type Foreign ftype (ErrorResponse code desc :> api) =
        Servant.Foreign.Foreign ftype api

    foreignFor lang ftype Proxy req =
        Servant.Foreign.foreignFor lang ftype (Proxy :: Proxy api) req


instance Servant.HasServer api ctx =>
         Servant.HasServer (ErrorResponse code desc :> api) ctx where

    type ServerT (ErrorResponse code desc :> api) m =
        Servant.ServerT api m

    route _ = Servant.route (Proxy :: Proxy api)

    hoistServerWithContext _ contextProxy =
        Servant.hoistServerWithContext (Proxy :: Proxy api) contextProxy
