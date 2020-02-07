module Database.Redis.Envy
  ( connectInfoFromEnv
  , connectInfoFromEnvWithDefault
  , parsePortId
  ) where


import           Data.Foldable   (asum)
import           Data.Scientific (Scientific)
import           Data.Time       (NominalDiffTime)
import           Data.Typeable   (Typeable)
import qualified Database.Redis  as R
import           System.Envy     (Parser, Var, envMaybe, fromVar, toVar, (.!=))
import           Text.Read       (readMaybe)


newtype ReadShowVar a = ReadShowVar { unReadShowVar :: a }

instance (Typeable a, Show a, Read a) => Var (ReadShowVar a) where
  toVar = show . unReadShowVar
  fromVar = fmap ReadShowVar . readMaybe


-- | Get 'R.ConnectInfo' from these environment variables:
--
-- * @REDIS_HOST@:            'R.connectHost'
-- * @REDIS_PORT_ID@:         'R.connectPort'
-- * @REDIS_AUTH@:            'R.connectAuth'
-- * @REDIS_DATABASE_INDEX@:  'R.connectDatabase'
-- * @REDIS_MAX_CONNECTIONS@: 'R.connectMaxConnections'
-- * @REDIS_MAX_IDLE_TIME@:   'R.connectMaxIdleTime'
-- * @REDIS_TIMEOUT@:         'R.connectTimeout'
--
-- The corresponding field of 'R.defaultConnectInfo' is used as the default value.
--
-- NOTE: 'R.connectTLSParams' is NOT supported.
-- This is hard to express as environment variables.
connectInfoFromEnv :: Parser R.ConnectInfo
connectInfoFromEnv = connectInfoFromEnvWithDefault R.defaultConnectInfo

-- | A variant of 'connectInfoFromEnv' which you can provide the
-- default value by yourself.
--
-- This is necessary to implement the instance of 'Env.FromEnv' for
-- 'R.ConnectInfo' correctly.
connectInfoFromEnvWithDefault :: R.ConnectInfo -> Parser R.ConnectInfo
connectInfoFromEnvWithDefault def = R.ConnInfo
  <$> envMaybe "REDIS_HOST" .!= R.connectHost def
  <*> pPortId
  <*> envMaybe "REDIS_AUTH" .!= R.connectAuth def
  <*> envMaybe "REDIS_DATABASE_INDEX" .!= R.connectDatabase def
  <*> envMaybe "REDIS_MAX_CONNECTIONS" .!= R.connectMaxConnections def
  <*>
    (fmap nominalDiffTimeFromVal <$> envMaybe "REDIS_MAX_IDLE_TIME")
      .!= R.connectMaxIdleTime def
  <*> timeout
  <*> pure (R.connectTLSParams def)
 where
  nominalDiffTimeFromVal :: ReadShowVar Scientific -> NominalDiffTime
  nominalDiffTimeFromVal = realToFrac . unReadShowVar

  timeout :: Parser (Maybe NominalDiffTime)
  timeout =
    maybe
      (R.connectTimeout def)
      (Just . nominalDiffTimeFromVal)
      <$> envMaybe "REDIS_TIMEOUT"

  pPortId =
    maybe
      (pure (R.connectPort def))
      parsePortId
      =<< envMaybe "REDIS_PORT_ID"


-- | Parse the value of @REDIS_PORT_ID@.
-- The value must be a port number or the absolute path to the UNIX domain
-- socket.
--
-- Internally used by 'connectInfoFromEnv'
parsePortId
  :: String -- ^ Value of environment variable
  -> Parser R.PortID
parsePortId envVarVal = maybe (fail emsg) pure $ asum
  [ R.PortNumber <$> readMaybe envVarVal
  , R.UnixSocket <$> parsePath envVarVal
  ]
 where
  parsePath path@('/': _) = Just path
  parsePath _other        = Nothing
  emsg =
    "The value of PortID must be a number (TCP port), or an absolute path to the UNIX domain socket!"
