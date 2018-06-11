{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Control.Monad.Except
import Data.String.Conversions

import qualified Bilge
import qualified Brig.Types.User as Brig
import qualified Data.Id as Brig
import qualified SAML2.WebSSO as SAML


toUserSSOId :: SAML.UserId -> Brig.UserSSOId
toUserSSOId (SAML.UserId tenant subject) =
  Brig.UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => Brig.UserSSOId -> m SAML.UserId
fromUserSSOId (Brig.UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserId t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


class Monad m => MonadSparToBrig m where
  call :: (Bilge.Request -> Bilge.Request) -> m (Bilge.Response (Maybe LBS))


getUser :: (MonadSparToBrig m) => SAML.UserId -> m (Maybe Brig.UserId)
getUser = undefined

createUser :: (MonadSparToBrig m) => SAML.UserId -> m Brig.UserId
createUser = undefined

-- | Get session token from brig and redirect user past login process.
forwardBrigLogin :: (MonadSparToBrig m) => Brig.UserId -> m SAML.Void
forwardBrigLogin = undefined
