{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Handler to process and display a home page.
module Handler.Home
    ( homeHandler
    ) where

import           Data.Text (Text)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Templating.Heist

import           Application


------------------------------------------------------------------------------
-- | Handler called by the toplevel route '/'. Of course as the auth splices
-- (see main function) are used the call of isLoggedIn might be redundant
-- here, but it does illustrate how the auth monad can be accessed from the
-- handler monad as well.
homeHandler :: AppHandler ()
homeHandler = ifTop $ do
    loginStatus <- with auth isLoggedIn
    heistLocal (bindString "message" $ message loginStatus) $ render "home"
  where
    message :: Bool -> Text
    message False = "You are not logged in."
    message True  = "Now you are logged in."
