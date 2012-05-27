{-# LANGUAGE OverloadedStrings  #-}

------------------------------------------------------------------------------
-- | This module contains handler for login and logout functionality.
--
module Handler.Login
    ( loginHandler
    , logoutHandler
    ) where

import           Control.Monad
import qualified Data.Text.Encoding as T

import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Text.Digestive.View
import           Text.Templating.Heist

import           Application
import           Form.LoginForm
import           Handler.Home
import           Util.Form

------------------------------------------------------------------------------
-- | Handler to display and process the login form.
loginHandler :: AppHandler ()
loginHandler = do
    (form, loginData) <- runForm "form" loginForm
    maybe (showForm "login" form) loginUserHandler loginData


------------------------------------------------------------------------------
-- | Handler that is called after successful login.
loginUserHandler :: LoginData -> AppHandler ()
loginUserHandler loginData = do
    with auth . loginByUsername username password $ loginRemember loginData
    homeHandler
  where
    username = T.encodeUtf8 $ loginUsername loginData
    password = ClearText    . T.encodeUtf8 $ loginPassword loginData


------------------------------------------------------------------------------
-- | Handler to process the logout.
logoutHandler :: AppHandler ()
logoutHandler = do
    with auth logout
    heistLocal (bindString "message" message) $ render "home"
  where
    message = "You are now logged out."
