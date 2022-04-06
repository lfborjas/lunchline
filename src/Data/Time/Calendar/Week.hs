-- | Unofficial backport of
-- https://hackage.haskell.org/package/time-1.12.1/docs/src/Data.Time.Calendar.Week.html#firstDayOfWeekOnAfter
-- Unfortunately can't upgrade to time 1.12.1 currently due to cabal2nix entering an infinite
-- recursion loop due to its dependence on unix + directory.

module Data.Time.Calendar.Week where

import Data.Time.Calendar
import Data.Fixed (mod')


-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
