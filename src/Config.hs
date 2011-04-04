{-# Language OverloadedStrings #-}

module Config (
    database
  , connPool  
  ) where

import Database.MongoDB        (Database (..), ConnPool, Host, newConnPool, host)

import Network.Abstract        (NetworkIO)



database :: Database
database = Database "hsnews"

connPool :: NetworkIO m => m (ConnPool Host)
connPool = newConnPool 1 (host "127.0.0.1")