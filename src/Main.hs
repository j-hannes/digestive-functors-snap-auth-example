------------------------------------------------------------------------------
-- | This is an example showing how to use the snap auth snaplet together with
-- digestive functors to create a basic login functionality. This code is
-- available as project form in a structured filesystem as well as a
-- standalone version where all code is kept in one file. This one is the
-- structured version.
--
module Main
    ( main
    ) where

import           Control.Exception
import qualified Data.Text as T
import           System.IO

import           Snap.Snaplet
import           Snap.Http.Server

import           Site


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
