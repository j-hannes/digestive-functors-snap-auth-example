{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module contains the login form and the corresponding data type.
-- Internally it also verifies the login data.
module Form.LoginForm
    ( LoginData(..)
    , loginForm
    ) where

import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Control.Applicative
import           Data.Maybe

import           Snap
import           Snap.Snaplet.Auth
import           Text.Digestive.Form

import           Application
import           Util.Form


------------------------------------------------------------------------------
-- | Data type for digestive login form.
data LoginData = LoginData
  { loginUsername :: Text  -- ^ username for login
  , loginPassword :: Text  -- ^ password for login
  , loginRemember :: Bool  -- ^ remember token for login
  } deriving (Show)


------------------------------------------------------------------------------
-- | Login form for a user.
loginForm :: Form Text AppHandler LoginData
loginForm =
    checkM invalidLoginMsg validLogin $ LoginData
      <$> "username" .: check usernameEmptyMsg notEmpty (text Nothing)
      <*> "password" .: check passwordEmptyMsg notEmpty (text Nothing)
      <*> "remember" .: bool (Just False)


------------------------------------------------------------------------------
-- | Validates the username password combination.
validLogin :: LoginData -> AppHandler Bool
validLogin loginData = do
    authMgr <- with auth get
    authUser <- liftIO . lookupByLogin authMgr $ loginUsername loginData
    return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText . T.encodeUtf8 $ loginPassword loginData
