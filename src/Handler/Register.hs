{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This is an example showing how to use the snap auth snaplet together with
module Handler.Register
    ( registrationHandler
    ) where


import           Control.Monad
import qualified Data.Text.Encoding as T

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.Form
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
    with auth $ createUser (regUsername regData) password
    heistLocal (bindStrings $ messages password) $ render "registration-done"
  where
    messages password = [
        ("name",     regFirstname regData)
      , ("password", T.decodeUtf8 password)
      ]
