{-# LANGUAGE TypeApplications #-}
-- |

module LunchLine.App where

import Data.Pool
import Database.Persist.Sqlite hiding (SqlPersistT)
import Control.Monad.Reader
    ( MonadIO(..), ReaderT(..), MonadReader, asks )
import Control.Monad.Logger
import LunchLine.Models
import Data.List (foldl')
import Data.Maybe
import Database.Esqueleto.Experimental
import Data.Time.Clock (getCurrentTime, UTCTime (UTCTime))
import Colonnade
import Control.Monad (void)

data Env = Env
  { envPool :: Pool SqlBackend
  }

newtype AppT a = AppT
  {unAppT :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runAppT :: MonadIO m => AppT a -> Env -> m a
runAppT body env = liftIO $ runReaderT (unAppT body) env

-- NOTE: (exercise): try defining @SqlPersistT m a@  and @DB a@
-- From: https://hackage.haskell.org/package/persistent-2.13.3.4/docs/Database-Persist-Sql.html#t:SqlPersistT
-- NOTE: removing the SqlPersistT alias since it actually comes from Esqueleto, too:
--type SqlPersistT = ReaderT SqlBackend
type DB = SqlPersistT IO

runDB :: DB a -> AppT a
runDB body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool

-- EXERCISES: compare to the MWB and Yesod implementations of the above:
-- MWB does the same, but with the args flipped and @Env@ becomes 'App'
-- https://github.com/MercuryTechnologies/mercury-web-backend/blob/70aa056ab6ce6d2d7cbc03917f2983a7b5896134/src/App.hs#L411-L412
-- As for RunDB, we're in the YesodDB instance, and run in a Handler context:
-- https://github.com/MercuryTechnologies/mercury-web-backend/blob/70aa056ab6ce6d2d7cbc03917f2983a7b5896134/src/App.hs#L194-L208

runApp :: AppT ()
runApp = addStuff >> summary

addStuff :: AppT ()
addStuff = runDB $ do
  addItem (AddLineItem "Pernil" 8.24)
  addItem (AddLineItem "Pizza" 2)

-- | Print the remaining budget, as well as the meals from the past week.
summary :: AppT ()
summary = do
  theSummary <- runDB $ do
    settings <- selectOne getSettings
    case settings of
      Nothing -> pure Nothing
      Just (Entity _ settings') -> do
        Just <$> weeklySummary settings'
  liftIO $ case theSummary of
    Nothing -> putStrLn "No budget set"
    Just (rb, allItems) -> do
      putStrLn $ "Remaining Budget " <> show rb
      putStr $ ascii colItem $ map entityVal allItems

colItem :: Colonnade Headed LineItem String
colItem =
  mconcat
    [ headed "Name" lineItemName
    , headed "Amount" (show . lineItemAmount)
    , headed "Added" (show . lineItemAdded)
    ]

appMain :: IO ()
appMain = do
  --env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runNoLoggingT $ withSqlitePool ":memory:" 10 $ \pool -> do
    -- NOTE: the persistent book uses `runResourceT` here
    -- NOTE: from the exercise "figure out a way to move migrateAll to main"
    -- from: https://www.yesodweb.com/book/persistent#persistent_integration_with_yesod
    --runSqlPool (runMigration migrateAll) pool
    flip runSqlPool pool $ do
      void $ runMigrationQuiet migrateAll
      bootstrapInitialSettings
    let env = Env pool
    runAppT runApp env
