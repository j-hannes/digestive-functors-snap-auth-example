{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This is an example showing how to use the snap auth snaplet together with
-- digestive functors to create a basic login functionality. This code is
-- available as project form in a structured filesystem as well as a
-- standalone version where all code is kept in one file. This one is the
-- standalone version.
--
-- The imports may look a bit funny here, but when somebody uses Snap the
-- first time it may not be so easy to assign external functions to the
-- corresponding library (path), so explicit imports are used here to make it
-- easier to find the right documentation of a function or library.
--
module Main where

------------------------------------------------------------------------------
-- IMPORTS                                                                  --
------------------------------------------------------------------------------

import           Control.Applicative ((<$>), (<*>))
import           Control.Exception (SomeException, try)
import           Control.Monad (liftM)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Lens.Template (makeLens)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           System.IO (hPutStrLn, stderr)
import           System.Random (newStdGen, randomRs)

import           Snap (liftIO)
import           Snap.Core (ifTop)
import           Snap.Http.Server (defaultConfig, httpServe)
import           Snap.Snaplet (Handler, Snaplet, SnapletInit, addRoutes,
                               makeSnaplet, runSnaplet, nestSnaplet,
                               subSnaplet, with)
import           Snap.Snaplet.Auth (AuthManager, Password(..), addAuthSplices,
                                    authenticatePassword, createUser,
                                    defAuthSettings, isLoggedIn, logout,
                                    loginByUsername, lookupByLogin,
                                    usernameExists)
import           Snap.Snaplet.Auth.Backends.JsonFile (initJsonFileAuthManager,
                                                      mkJsonAuthMgr)
import           Snap.Snaplet.Heist (HasHeist, Heist, heistInit, heistLens,
                                     heistLocal, render)
import           Snap.Snaplet.Session (SessionManager)
import           Snap.Snaplet.Session.Backends.CookieSession
                   (initCookieSessionManager)
import           Snap.Util.FileServe (serveDirectory)
import           Text.Digestive.Heist (bindDigestiveSplices)
import           Text.Digestive.Snap (runForm)
import           Text.Digestive.Form (Form, (.:), bool, check, checkM, text)
import           Text.Digestive.View (View)
import qualified Text.Email.Validate as E
import           Text.Templating.Heist (bindString, bindStrings)




------------------------------------------------------------------------------
-- CODE                                                                     --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | The app data type which is used as snaplet itself and contains further
-- snaplets which seperate functionality. For more information see the
-- documentation of "Snap.Snaplet".
data App = App
    { _heist   :: Snaplet (Heist App)
    , _sess    :: Snaplet SessionManager
    , _auth    :: Snaplet (AuthManager App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App


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
    authMgr  <- liftIO $ mkJsonAuthMgr "users.json"
    authUser <- liftIO $ lookupByLogin authMgr $ loginUsername loginData
    return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText $ T.encodeUtf8 $ loginPassword loginData
                          

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


------------------------------------------------------------------------------
-- | Bind the elements from the digestive form to the corresponding view
-- template.
showForm :: String -> View Text -> AppHandler ()
showForm prefix form =
    heistLocal (bindDigestiveSplices form) $ render template
  where
    template = BS8.pack $ prefix ++ "-form"


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
usernameEmptyMsg  = "please enter a username"
firstnameEmptyMsg = "please enter a first name"
lastnameEmptyMsg  = "please enter a last name"
passwordEmptyMsg  = "please enter a password"
emailInvalidMsg   = "please enter a valid email"
usernameInUseMsg  = "username already in use"
invalidLoginMsg   = "wrong username/password combination"


------------------------------------------------------------------------------
-- | Application snaplet that nests subsnaplets like heist, sess, auth or
-- more, initializes the routes and returns the application record to be
-- served by runSnaplet. Also already adds the auth splices <ifLoggedIn> and
-- <ifLoggedOut>, but this is optional.
--
-- Session is initialized cookie based and the authentication simply as JSON
-- variant. Other variants, as HDBC persistent storage are also possible.
--
-- Note: For more complex routing a definition of routes as toplevel is good
-- practice.
app :: SnapletInit App App
app = makeSnaplet "app" "digestive-functors example application" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt"
         "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings
         sess "users.json"
    addRoutes [
        ("/",         homeHandler)
      , ("/login",    loginHandler)
      , ("/logout",   logoutHandler)
      , ("/register", registrationHandler)
      , ("",          serveDirectory "resources")
      ]
    addAuthSplices auth
    return $ App h s a


------------------------------------------------------------------------------
-- | Serving entry point. Runs the applicaton snaplet and serves the resulting
-- site. See main function or Main.hs source file of a standard initialized
-- snap project (via command snap init) for more information.
main :: IO ()
main = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    _ <- try $ httpServe defaultConfig site :: IO (Either SomeException ())
    cleanup
