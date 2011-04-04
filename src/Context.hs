{-# Language OverloadedStrings, Rank2Types #-}

module Context (
    Context (..)
  , ContextM
  , unpackContext
  ) where

import Control.Monad.Reader

import Happstack.Server        (Conf, ServerPartT, UnWebT)

import Database.MongoDB        (Database (..), ConnPool, Host)

import DB.User                 (User)

data Context = Context { httpConf :: Conf
                       , static   :: FilePath
                       , database :: Database
                       , connPool :: ConnPool Host
                       , user     :: Maybe User
                       }

type ContextM = ServerPartT (ReaderT Context IO)

unpackContext :: Context -> UnWebT (ReaderT Context IO) a -> UnWebT IO a
unpackContext context ct = runReaderT ct context >>= return