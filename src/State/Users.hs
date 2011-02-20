{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}

module State.Users (
  Users, UsersMap,
  GetUsers(..), InsertUser(..)
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

import Data.Data (Data, Typeable)

import Happstack.Data.Serialize (Serialize(..), Version(..), Mode(..), safeGet,
                                 safePut, contain, serialize)
import Happstack.State (Component(..), End, Query, Update, Version, deriveSerialize,
                        mkMethods, getRandom)
  
import Data.HashMap (HashMap)
import qualified Data.HashMap as M
import Data.Hashable (Hashable)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toChunks)

import Crypto.PasswordStore (makePasswordSalt, makeSalt)

-- Version and serialize instances for HashMap, I have to put this
-- somewhere else.
instance Version (HashMap a b) where mode = Primitive
instance (Serialize a, Serialize b, Ord a, Hashable a) => Serialize (HashMap a b) where
    getCopy = contain $ fmap M.fromList safeGet
    putCopy = contain . safePut . M.toList

type UsersMap = HashMap ByteString ByteString

data Users = Users { users :: UsersMap
                   }
             deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version Users
$(deriveSerialize ''Users)

instance Component Users where
  type Dependencies Users = End
  initialValue = Users M.empty

getUsers :: Query Users UsersMap
getUsers = asks users

hashStrength :: Int
hashStrength = 12

-- | Gets username and password and updates the users map
insertUser :: ByteString -> ByteString -> Update Users ()
insertUser username passwd = do
  rand <- getRandom
  let salt    = (makeSalt . B.concat . toChunks . serialize) (rand :: Int)
      hashedp = makePasswordSalt passwd salt hashStrength
  modify (\s -> s { users = M.insert username hashedp (users s) })


-- Generate the query events
$(mkMethods ''Users [ 'getUsers, 'insertUser
                    ])





