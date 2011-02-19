{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}

module State (
  AppState(..),
  Users, GetUsers(..), InsertUser(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, put, modify)

import Data.Data (Data, Typeable)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toChunks)
import qualified Data.HashMap as M

import Crypto.PasswordStore (makePasswordSalt, makeSalt)

import Happstack.State (Component(..), End, Query, Update, Version, deriveSerialize,
                        mkMethods, getRandom)
import Happstack.Data.Serialize (serialize)
  
import State.Users


-- | State
data AppState = AppState { users :: Users
                         }
              deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState { users = M.empty
                          }
  
--------------------------------------------------------------------------------
-- Useful methods to operate with the state
  
-- Methods relative to the users

getUsers :: Query AppState Users
getUsers = asks users

hashStrength = 12

-- | Gets username and password and updates the users map
insertUser :: ByteString -> ByteString -> Update AppState ()
insertUser username passwd = do
  rand <- getRandom
  let salt    = (makeSalt . B.concat . toChunks . serialize) (rand :: Int)
      hashedp = makePasswordSalt passwd salt hashStrength
  modify (\s -> s { users = M.insert username hashedp (users s) })


-- Generate the query events
$(mkMethods ''AppState [ 'getUsers, 'insertUser
                       ])