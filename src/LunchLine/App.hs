{-# LANGUAGE TypeApplications #-}
-- |

module LunchLine.App where

import Data.Pool
import Database.Persist.Sqlite hiding (SqlPersistT)
import Control.Monad.Reader
import Control.Monad.Logger
import LunchLine.Models
import Data.List (foldl')
import Data.Maybe
import Database.Esqueleto.Experimental

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

weeklyBudget :: Double
weeklyBudget = 100

spend :: Double -> LineItem -> Double
spend n e = n - lineItemAmount e


getLineItemTotal
  :: (Num a, MonadIO m, PersistField a)
  => SqlPersistT m a
getLineItemTotal =
  selectSum $ do
    items <- from $ table @LineItem
    pure $ sum_ $ items ^. LineItemAmount
  where
    selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne

runApp :: AppT ()
runApp = do
  total <- runDB $ do
    insert_ $ LineItem "Pizza" 11.0
    insert_ $ LineItem "Burger" 12.0
    getLineItemTotal
  let remainingBudget = weeklyBudget - total
  liftIO . putStrLn $ "Remaining Budget:" <> show remainingBudget


appMain :: IO ()
appMain = do
  --env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runStderrLoggingT $ withSqlitePool ":memory:" 10 $ \pool -> do
    -- NOTE: the persistent book uses `runResourceT` here
    -- NOTE: from the exercise "figure out a way to move migrateAll to main"
    -- from: https://www.yesodweb.com/book/persistent#persistent_integration_with_yesod
    runSqlPool (runMigration migrateAll) pool
    let env = Env pool
    runAppT runApp env
