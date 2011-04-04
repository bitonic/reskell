{-# Language OverloadedStrings #-}

module Config (
    Config (..)
  , ConfigT
  , unpackConfigT
  ) where

import Control.Monad.Reader

import Happstack.Server        (Conf, ServerPartT, UnWebT)

import Database.MongoDB        (Database (..), ConnPool, Host, newConnPool, host)

import Network.Abstract        (NetworkIO)

data Config = Config { httpConf :: Conf
                     , static   :: FilePath
                     , database :: Database
                     , connPool :: ConnPool Host
                     }

type ConfigT m a = ServerPartT (ReaderT Config m) a
              
unpackConfigT :: Monad m => Config -> UnWebT (ReaderT Config m) a -> UnWebT m a
unpackConfigT config ct = runReaderT ct config >>= return