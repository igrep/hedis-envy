{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Redis.Envy.Instance () where


import qualified Database.Redis      as R
import           Database.Redis.Envy
import           System.Envy         (FromEnv, fromEnv)


instance FromEnv R.ConnectInfo where
  fromEnv Nothing    = connectInfoFromEnv
  fromEnv (Just def) = connectInfoFromEnvWithDefault def
