{-# LANGUAGE TypeApplications #-}
-- |

module LunchLine.App where

import Data.Pool
import Database.Persist.Sqlite
import Control.Monad.Reader
import Control.Monad.Logger
import LunchLine.Models

data Env = Env
  { envPool :: Pool SqlBackend }

newtype AppT a = AppT
  {unAppT :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runAppT :: MonadIO m => AppT a -> Env -> m a
runAppT body env = liftIO $ runReaderT (unAppT body) env

runDB :: ReaderT SqlBackend IO a -> AppT a
runDB body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool

-- EXERCISES: compare to the MWB and Yesod implementations of the above:
-- MWB does the same, but with the args flipped and @Env@ becomes 'App'
-- https://github.com/MercuryTechnologies/mercury-web-backend/blob/70aa056ab6ce6d2d7cbc03917f2983a7b5896134/src/App.hs#L411-L412
-- As for RunDB, we're in the YesodDB instance, and run in a Handler context:
-- https://github.com/MercuryTechnologies/mercury-web-backend/blob/70aa056ab6ce6d2d7cbc03917f2983a7b5896134/src/App.hs#L194-L208

runApp :: AppT ()
runApp = do
  lineItems <- runDB $ do
    runMigration migrateAll
    insert_ $ LineItem "Pizza" 11.0
    insert_ $ LineItem "Burger" 12.0
    selectList [] []
  -- liftIO $ print (lineItems :: [Entity LineItem])
  liftIO $ print (lineItems :: [Entity LineItem])


appMain :: IO ()
appMain = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT runApp env
