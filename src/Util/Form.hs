{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | A library with some helper functions.
--
module Util.Form
    ( showForm
    , createRandomPassword
    , notEmpty
    , validEmail
    , usernameEmptyMsg
    , firstnameEmptyMsg
    , lastnameEmptyMsg
    , passwordEmptyMsg
    , emailInvalidMsg
    , usernameInUseMsg
    , invalidLoginMsg
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           System.Random

import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.View
import           Text.Email.Validate as E

import           Application

------------------------------------------------------------------------------
-- | Bind the elements from the digestive form to the corresponding view
-- template.
showForm :: String -> View Text -> AppHandler ()
showForm prefix form =
    heistLocal (bindDigestiveSplices form) $ render template
  where
    template = BS8.pack $ "forms/" ++ prefix ++ "-form"


------------------------------------------------------------------------------
-- | Create a random password.
createRandomPassword :: IO ByteString
createRandomPassword = do
    gen <- newStdGen
    let pw = take 10 $ randomRs ('a', 'z') gen
    return $ BS8.pack pw


------------------------------------------------------------------------------
-- | Check if a text has the length null.
notEmpty :: Text -> Bool
notEmpty = not . T.null


-----------------------------------------------------------------------------
-- | Function to verify the email via Text.Email.Validate.
validEmail :: Text -> Bool
validEmail = E.isValid . T.unpack


-----------------------------------------------------------------------------
-- Some error messages...
usernameEmptyMsg :: Text
usernameEmptyMsg  = "please enter a username"

firstnameEmptyMsg :: Text
firstnameEmptyMsg = "please enter a first name"

lastnameEmptyMsg :: Text
lastnameEmptyMsg  = "please enter a last name"

passwordEmptyMsg :: Text
passwordEmptyMsg  = "please enter a password"

emailInvalidMsg :: Text
emailInvalidMsg   = "please enter a valid email"

usernameInUseMsg :: Text
usernameInUseMsg  = "username already in use"

invalidLoginMsg :: Text
invalidLoginMsg   = "wrong username/password combination"
