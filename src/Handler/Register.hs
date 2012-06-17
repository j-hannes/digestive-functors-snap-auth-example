{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module contains a handler for displaying and processing a
-- registration form.
--
module Handler.Register
    ( registrationHandler
    ) where


import qualified Data.Text.Encoding as T

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive.Snap
import           Text.Templating.Heist

import           Application
import           Form.RegistrationForm
import           Util.Form


------------------------------------------------------------------------------
-- | Handler to display and process the registration form.
registrationHandler :: AppHandler ()
registrationHandler = do 
    (form, registrationData) <- runForm "form" registrationForm
    maybe (showForm "registration" form) createNewUser registrationData


------------------------------------------------------------------------------
-- | Create a new user from the entered registration data and store it in a
-- json file. Note: here only the username and the password are stored.
createNewUser :: RegistrationData -> AppHandler ()
createNewUser regData = do
    password <- liftIO createRandomPassword
    --with auth $ createUser (regUsername regData) password
    user <- with auth $ createUser (regUsername regData) password
    with auth . saveUser $ user { userRoles = [Role "Subscriber"] }
    heistLocal (bindStrings $ messages password) $ render "registration-done"
  where
    messages password = [
        ("name",     regFirstname regData)
      , ("password", T.decodeUtf8 password)
      ]
