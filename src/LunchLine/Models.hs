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
import Data.Time.Calendar.Week (firstDayOfWeekOnAfter)
import Data.Bool (bool)

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

weeklySummary
  :: MonadIO m
  => Settings
  -> SqlPersistT m (Double, [Entity LineItem])
weeklySummary Settings{settingsWeeklyBudget, settingsWeekStartsSunday} = do
  now <- liftIO getCurrentTime
  let theWeek = weekInterval settingsWeekStartsSunday now
  total <- getLineItemTotal theWeek
  items <- select $ getItemsInInterval theWeek
  pure (settingsWeeklyBudget - total, items)

-- NOTE: the below would probably not work: can't really use aggregations
-- without grouping (or, in sqlite, it would just group implicitly)
-- weeklySummary
--   :: MonadIO m
--   => Settings
--   -> SqlPersistT m [(Entity LineItem, Value (Maybe Double))]
-- weeklySummary Settings{settingsWeeklyBudget, settingsWeekStartsSunday} = do
--   now@(UTCTime today _) <- liftIO getCurrentTime
--   let theWeek = weekInterval settingsWeekStartsSunday now
--   select $ do
--     items <- getItemsInInterval theWeek
--     let totalSpent = sum_ $ items ^. LineItemAmount
--     pure (items, totalSpent)




-- NON-DB HELPERS

-- | Find the start of the current and next weeks, given
-- whether or not to start the week on Sunday, and a moment in the current day
weekInterval :: Bool -> UTCTime -> (Day, Day)
weekInterval startsOnSunday (UTCTime today _) =
  (thisWeekStart, nextWeekStart)
  where
    nextWeekStart =
      firstDayOfWeekOnAfter
        (bool Sunday Monday startsOnSunday)
        today
    thisWeekStart =
      addDays (-7) nextWeekStart
