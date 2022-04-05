{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
-- NOTE: these extensions are now required by persistent,
{-# LANGUAGE TypeFamilies, GADTs, StandaloneDeriving, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses#-}
{-# LANGUAGE OverloadedLabels #-}
-- |

module LunchLine.Models where

import Database.Persist.TH
import Database.Esqueleto.Experimental
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
LineItem
  name String
  amount Double
  added Day
  created_at UTCTime
  updated_at UTCTime
  deriving Show

Settings
  weeklyBudget Double default=100
  weekStartsSunday Bool default=false
|]

data AddLineItem = AddLineItem
  { addName :: String
  , addAmount :: Double
  }

-- | Initialize settings unless already initialized
bootstrapInitialSettings
  :: (MonadIO m)
  => SqlPersistT m ()
bootstrapInitialSettings = do
  existingSettings <- selectOne $ do
    settingsSingleton <- from $ table @Settings
    limit 1
    pure settingsSingleton
  case existingSettings of
    Nothing -> insert_ $ Settings 100 False
    Just _es -> pure ()


getSettings :: SqlQuery (SqlExpr (Entity Settings))
getSettings = do
  s <- from $ table @Settings
  limit 1
  pure s

getItemsInInterval :: (Day, Day) -> SqlQuery (SqlExpr (Entity LineItem))
getItemsInInterval (iStart, iEnd) = do
  items <- from $ table @LineItem
  where_ $ items ^. #added `between` (val iStart, val iEnd)
  pure items


getLineItemTotal
  :: (Num a, MonadIO m, PersistField a)
  => (Day, Day) -> SqlPersistT m a
getLineItemTotal interval =
  selectSum $ do
    items <- getItemsInInterval interval
    pure $ sum_ $ items ^. LineItemAmount
  where
    selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne

addItem
  :: MonadIO m
  => AddLineItem -> SqlPersistT m ()
addItem AddLineItem{addName, addAmount} = do
  now@(UTCTime today _) <- liftIO getCurrentTime
  let li =
        LineItem
          addName
          addAmount
          today
          now
          now
  insert_ li
