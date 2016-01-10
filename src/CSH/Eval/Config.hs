{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CSH.Eval.Config
Description : Configuration utilities
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Provides functionality for reading config values.
-}

module CSH.Eval.Config (
    Command (..)
  , ServerCmd (..)
  , DBInitCmd (..)
  , PoolSettings (..)
  , poolSettings
  , evalConfig
  , cxCfgSettings
  , poolCfgSettings
  , module Data.Configurator
  ) where

import Control.Monad (mfilter)
import Data.Configurator
import Data.Configurator.Types
import Data.Word (Word16)
import Data.Time.Clock (NominalDiffTime)
import Hasql.Settings
import Hasql.Pool
import System.Environment
import qualified Data.ByteString.Char8 as C

-- | Toplevel 'csh-eval' command configuration type.
data Command = Members ServerCmd -- ^ Members only site configuration type
             | Intro ServerCmd   -- ^ Freshman site configuration type
             | InitDB DBInitCmd  -- ^ DB initialization command configuation type

-- | Configuation type for launching either site
data ServerCmd = ServerCmd { withTLS :: Bool -- ^ Launch with TLS support
                           , port    :: Int  -- ^ Which port to launch on
                           }

-- | Configuration type for running the db initialization script
data DBInitCmd = DBInitCmd
                { dbHost :: C.ByteString -- ^ Database hostname
                , dbPort :: Word16       -- ^ Database port
                , dbUser :: C.ByteString -- ^ Database username
                , dbName :: C.ByteString -- ^ Database name
                }

-- | Hasql pool settings.
data PoolSettings = PoolSettings {
    poolSize     :: Int             -- ^ Maximum number of connections.
  , poolTimeout  :: NominalDiffTime -- ^ Individual connection timeout.
  , poolSettings :: Settings        -- ^ Hasql connection settings.
  }

poolSettings :: Int             -- ^ Maxumum number of connections.
             -> NominalDiffTime -- ^ Individual connection timeout.
             -> Settings        -- ^ Hasql connection settings.
             -> Maybe PoolSettings
poolSettings c t s = PoolSettings <$> connOK c <*> timeOK t <*> pure s
    where connOK = mfilter (>= 1)  . pure  -- Must have connections >= 1.
          timeOK = mfilter (>= 60) . pure -- Must have connetion lifetime >= 1 minute.

-- | Load a config from the path given by the 'CSH_EVAL_CONFIG' environment
--   variable, or default to 'config/csh-eval.rc'
evalConfig :: IO Config
evalConfig = lookupEnv "CSH_EVAL_CONFIG"
         >>= maybe (load [Required "config/csh-eval.rc"])
                   (\x -> load [Required x])

-- | Derive hasql postgres connection settings from configuration.
cxCfgSettings :: Config -> IO Settings
cxCfgSettings cfg = settings
                <$> (lookupDefault "localhost" cfg "db.host")
                <*> (lookupDefault 5432 cfg "db.port")
                <*> (lookupDefault "" cfg "db.user")
                <*> (lookupDefault "" cfg "db.password")
                <*> (lookupDefault "" cfg "db.dbname")

-- | Derive hasql pool settings from configuration.
poolCfgSettings :: Config -> IO PoolSettings
poolCfgSettings cfg = do
    max_con      <- lookupDefault 1 cfg "db.pool.max_con"
    idle_timeout <- lookupDefault 60 cfg "db.pool.idle_timeout"
    settings     <- cxCfgSettings Config
    case poolSettings max_con idle_timeout settings of
        Just x -> pure x
        Nothing -> fail ("Bad pool settings config:\n  db.pool.max_con = "
                       ++ (show max_con)
                       ++ "\n  db.pool.idle_timeout: "
                       ++ (show idle_timeout))
