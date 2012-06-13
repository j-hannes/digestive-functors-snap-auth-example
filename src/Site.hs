{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This is an example showing how to use the snap auth snaplet together with
module Site
    ( app
    ) where

import           Data.ByteString

import           Database.HDBC.Sqlite3
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Hdbc
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Application
import           Handler.Home
import           Handler.Login
import           Handler.Register

------------------------------------------------------------------------------
-- | Some route definitions.
routes :: [(ByteString, AppHandler ())]
routes = [
      ("/",         homeHandler)
    , ("/login",    loginHandler)
    , ("/logout",   logoutHandler)
    , ("/register", registrationHandler)
    , ("",          serveDirectory "resources")
    ]


------------------------------------------------------------------------------
-- | Application snaplet that nests subsnaplets like heist, sess, auth or
-- more, initializes the routes and returns the application record to be
-- served by runSnaplet. Also already adds the auth splices <ifLoggedIn> and
-- <ifLoggedOut>, but this is optional.
--
-- Session is initialized cookie based and the authentication simply as JSON
-- variant. Other variants, as HDBC persistent storage are also possible.
app :: SnapletInit App App
app = makeSnaplet "app" "digestive-functors example application" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess sessionInit
    a <- nestSnaplet "auth" auth hdbcAuthInit
    d <- nestSnaplet "hdbc" db $ hdbcInit sqli
    addRoutes routes
    addAuthSplices auth
    return $ App h s a d
  where
    jsonAuthInit = initJsonFileAuthManager defAuthSettings sess "users.json"
    hdbcAuthInit = initHdbcAuthManager defAuthSettings sess sqli defAuthTable
                                       defQueries
    sessionInit  = initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    sqli         = connectSqlite3 "example.db"
