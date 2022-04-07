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
  appKey String
  Primary appKey
|]


singletonSettingsKey :: String
singletonSettingsKey = "lunchline-settings"

data AddLineItem = AddLineItem
  { addName :: String
  , addAmount :: Double
  , addAdded :: Maybe Day
  }

data AddSettings = AddSettings
  { setBudget :: Double
  , setWeekStartsSunday :: Maybe Bool
  }

-- | Initialize settings unless already initialized
bootstrapInitialSettings
  :: (MonadIO m)
  => SqlPersistT m ()
bootstrapInitialSettings = do
  existingSettings <- selectOne getSettings
  case existingSettings of
    Nothing -> insert_ $ Settings 100 False singletonSettingsKey
    Just _es -> pure ()


getSettings :: SqlQuery (SqlExpr (Entity Settings))
getSettings = do
  s <- from $ table @Settings
  where_ $ s ^. #appKey ==. val singletonSettingsKey
  pure s

configureSettings :: MonadIO m => AddSettings -> SqlPersistT m ()
configureSettings AddSettings{setBudget, setWeekStartsSunday}= do
  update $ \s -> do
    set s [ SettingsWeeklyBudget =. val setBudget,
            SettingsWeekStartsSunday =. val (fromMaybe False setWeekStartsSunday)]
    where_ $ s  ^. #appKey ==. val singletonSettingsKey


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
addItem AddLineItem{addName, addAmount, addAdded} = do
  now@(UTCTime today _) <- liftIO getCurrentTime
  let li =
        LineItem
          addName
          addAmount
          (fromMaybe today addAdded)
          now
          now
  insert_ li

-- | Nicer to digest but slightly wasteful version of 'weeklySummary'
-- The generated SQL looks like:
-- [Debug#SQL] SELECT "settings"."id", "settings"."weekly_budget", "settings"."week_starts_sunday"
-- FROM "settings"
--  LIMIT 1; []
-- [Debug#SQL] SELECT SUM("line_item"."amount")
-- FROM "line_item"
-- WHERE ("line_item"."added" >= ?) AND ("line_item"."added" <= ?)
--  LIMIT 1; [PersistDay 2022-04-03,PersistDay 2022-04-10]
-- [Debug#SQL] SELECT "line_item"."id", "line_item"."name", "line_item"."amount", "line_item"."added", "line_item"."created_at", "line_item"."updated_at"
-- FROM "line_item"
-- WHERE ("line_item"."added" >= ?) AND ("line_item"."added" <= ?)
-- ; [PersistDay 2022-04-03,PersistDay 2022-04-10]
weeklySummary'
  :: MonadIO m
  => Settings
  -> SqlPersistT m (Double, [Entity LineItem])
weeklySummary' Settings{settingsWeeklyBudget, settingsWeekStartsSunday} = do
  now <- liftIO getCurrentTime
  let theWeek = weekInterval settingsWeekStartsSunday now
  total <- getLineItemTotal theWeek
  items <- select $ getItemsInInterval theWeek
  pure (settingsWeeklyBudget - total, items)

-- | Get all items /and/ their accumulated amount in one trip to the DB,
-- using CTEs. Not as succinct as 'weeklySummary'' but hopefully marginally
-- more efficient on the DB side (since we don't have to ask for line
-- items twice.) Mostly done as an experiment to see if we could
-- simulate windowing functions in some contexts.
-- The generated SQL looks like:
-- [Debug#SQL] WITH "cte" AS (SELECT "line_item"."id" AS "v_id", "line_item"."name" AS "v_name", "line_item"."amount" AS "v_amount", "line_item"."added" AS "v_added", "line_item"."created_at" AS "v_created_at", "line_item"."updated_at" AS "v_updated_at"
-- FROM "line_item"
-- WHERE ("line_item"."added" >= ?) AND ("line_item"."added" <= ?)
-- ),
-- "cte2" AS (SELECT SUM("cte"."v_amount") AS "v2"
-- FROM "cte"
-- )
-- SELECT "cte2"."v2", "cte"."v_id", "cte"."v_name", "cte"."v_amount", "cte"."v_added", "cte"."v_created_at", "cte"."v_updated_at"
-- FROM "cte", "cte2"
weeklySummary
  :: MonadIO m
  => Settings
  -> SqlPersistT m (Double, [Entity LineItem])
weeklySummary Settings{settingsWeeklyBudget, settingsWeekStartsSunday} = do
  now <- liftIO getCurrentTime
  let theWeek = weekInterval settingsWeekStartsSunday now
  summary <- select $ do
    is <-
      with $ do
        getItemsInInterval theWeek
    cnt <-
      with $ do
        items <- from is
        pure $ sum_ $ items ^. LineItemAmount
    items <- from is
    agg   <- from cnt
    pure (agg, items)
  let mTotal =
        maybe 0 (fromMaybe 0 . unValue . fst)
        $ listToMaybe summary
      allItems = map snd summary
  pure (settingsWeeklyBudget - mTotal, allItems)


-- NON-DB HELPERS

-- | Find the start of the current and next weeks, given
-- whether or not to start the week on Sunday, and a moment in the current day
weekInterval :: Bool -> UTCTime -> (Day, Day)
weekInterval startsOnSunday (UTCTime today _) =
  (thisWeekStart, addDays (-1) nextWeekStart)
  where
    nextWeekStart =
      firstDayOfWeekOnAfter
        (bool Sunday Monday startsOnSunday)
        today
    thisWeekStart =
      addDays (-7) nextWeekStart
