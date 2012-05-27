{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This is an example showing how to use the snap auth snaplet together with
-- digestive functors to creatative to this file.
--
module Form.RegistrationForm
    ( RegistrationData(..)
    , registrationForm
    ) where

import           Data.Text (Text)
import           Control.Applicative
import           Control.Monad

import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Text.Digestive.Form

import           Application
import           Util.Form

------------------------------------------------------------------------------
-- | Data type for digestive registration form.
data RegistrationData = RegistrationData
  { regUsername  :: Text  -- ^ username for login
  , regFirstname :: Text  -- ^ first name of user
  , regLastname  :: Text  -- ^ last name of user
  , regEmail     :: Text  -- ^ email for sending new password (later)
  } deriving (Show)


------------------------------------------------------------------------------
-- | Registration form for a user.
registrationForm :: Form Text AppHandler RegistrationData
registrationForm =
    checkM usernameInUseMsg validUsername $ RegistrationData
      <$> "username"  .: check usernameEmptyMsg  notEmpty (text Nothing)
      <*> "firstname" .: check firstnameEmptyMsg notEmpty (text Nothing)
      <*> "lastname"  .: check lastnameEmptyMsg  notEmpty (text Nothing)
      <*> "email"     .: check emailInvalidMsg validEmail (text Nothing)


------------------------------------------------------------------------------
-- | Checks if with the auth snaplet if a username is already in use.
validUsername :: RegistrationData -> AppHandler Bool
validUsername = liftM not . with auth . usernameExists . regUsername
