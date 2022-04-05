{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- NOTE: these extensions are now required by persistent,
{-# LANGUAGE TypeFamilies, GADTs, StandaloneDeriving, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses#-}
-- |

module LunchLine.Models where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
LineItem
  name String
  amount Double
  deriving Show
|]
