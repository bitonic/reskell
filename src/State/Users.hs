{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances, 
  StandaloneDeriving, OverloadedStrings #-}

module State.Users (
  Users, UsersMap, SessionId,
  GetUsers(..), InsertUser(..), InsertSession(..)
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

import Data.Data (Data, Typeable)

import Happstack.Data.Serialize (Serialize(..), Version(..), Mode(..), safeGet,
                                 safePut, contain, serialize)
import Happstack.State (Component(..), End, Query, Update, Version, deriveSerialize,
                        mkMethods)
  
import Data.HashMap (HashMap)
import qualified Data.HashMap as M

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Data.Hashable (Hashable)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toChunks)

import qualified Text.Read as Read
import qualified Text.ParserCombinators.ReadP as ReadP

import Crypto.PasswordStore (Salt, makePasswordSalt)

-- Version and serialize instances for HashMap and HashSet, I have to
-- put this somewhere else.
instance Version (HashMap a b) where mode = Primitive
instance (Serialize a, Serialize b, Ord a, Hashable a) => Serialize (HashMap a b) where
    getCopy = contain $ fmap M.fromList safeGet
    putCopy = contain . safePut . M.toList

instance Version (HashSet a) where mode = Primitive
instance (Serialize a, Ord a, Hashable a) => Serialize (HashSet a) where
    getCopy = contain $ fmap S.fromList safeGet
    putCopy = contain . safePut . S.toList

deriving instance Typeable Salt
instance Version Salt where
$(deriveSerialize ''Salt)

---------------------------------------------------------------------


type Username = ByteString
type Password = ByteString

-- | A map with all the users
type UsersMap = HashMap Username Password

-- | The SessionId looks like this: username|randomSalt. The salt is
-- provided externally.
type SessionId = ByteString

-- | Returns the username
decodeSessionId :: SessionId -> Maybe Username
decodeSessionId sid | B.length salt < 2 = Nothing
                    | otherwise         = Just username
  where (username, salt) = B.break (/= '|') sid

-- | A session with all the logged 
type UsersSessions = HashSet SessionId

-- | The Users data type
data Users = Users { users :: UsersMap
                   , session :: UsersSessions
                   }
             deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version Users where mode = Primitive
$(deriveSerialize ''Users)

instance Component Users where
  type Dependencies Users = End
  initialValue = Users M.empty S.empty

-- Query / Updates

-- | Gets the map of users
getUsers :: Query Users UsersMap
getUsers = asks users

-- | Defines how many passes of the hash function
hashStrength :: Int
hashStrength = 12

-- | Gets username and password and updates the users map
insertUser :: ByteString -> ByteString -> Salt -> Update Users ()
insertUser username passwd salt =
  modify (\s -> s { users = M.insert username hashedp (users s) })
  where hashedp = makePasswordSalt passwd salt hashStrength

-- | Inserts a new session. Accepts a salt since I'll generate the
-- random bit with genSaltIO.
insertSession :: Username -> Salt -> Update Users String
insertSession username salt = do
  modify (\s -> s { session = S.insert sid (session s) })
  return $ B.unpack sid
  where
    sid = B.concat [username, "|", B.pack . show $ salt]

deleteSession :: SessionId -> Update Users ()
deleteSession sid = modify (\s -> s { session = S.delete sid (session s) })

-- Generate the query events
$(mkMethods ''Users [ 'getUsers, 'insertUser
                    , 'insertSession, 'deleteSession
                    ])