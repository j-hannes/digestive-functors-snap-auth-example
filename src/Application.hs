{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module exports the application snaplet containing the three
-- subsnaplets Heist, Session and Auth to make them accessible application
-- wide.
-- 
module Application
    ( App(App)
    , heist
    , sess
    , auth
    , db
    , AppHandler
    ) where

import           Data.Lens.Template

import           Database.HDBC.Sqlite3
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Util.FileServe


------------------------------------------------------------------------------
-- | The app data type which is used as snaplet itself and contains further
-- snaplets which seperate functionality. For more information see the
-- documentation of "Snap.Snaplet".
data App = App
    { _heist   :: Snaplet (Heist App)
    , _sess    :: Snaplet SessionManager
    , _auth    :: Snaplet (AuthManager App)
    , _db      :: Snaplet (HdbcSnaplet Connection IO)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App
