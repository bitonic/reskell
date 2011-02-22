{-# Language DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances, 
  StandaloneDeriving, OverloadedStrings, FlexibleInstances #-}

module State.Users (
  UserRank(..), Username, Password, User(..),
  Users, UsersMap, SessionId,
  GetUsers(..), InsertUser(..),
  InsertSession(..), DeleteSession(..), GetSessions(..), CheckSession(..)
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

import Data.Data (Data, Typeable)

import Happstack.Data.Serialize (Serialize(..), Version(..), Mode(..), safeGet,
                                 safePut, contain)
import Happstack.State (Component(..), End, Query, Update, deriveSerialize,
                        mkMethods)
  
import Data.HashMap (HashMap)
import qualified Data.HashMap as M

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Data.Hashable (Hashable)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Crypto.PasswordStore (Salt, makePasswordSalt, exportSalt)

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


data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)
instance Version UserRank
$(deriveSerialize ''UserRank)

type Username = ByteString
type Password = ByteString

-- | The User data type, stores all the information about a single
-- user
data User = User { userName :: Username
                 , userPassword :: Password
                 , userRank :: UserRank
                 }
            deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | A map with all the users
type UsersMap = HashMap Username User

instance Version User
$(deriveSerialize ''User)

-- | The SessionId looks like this: username|randomSalt. The salt is
-- provided externally.
type SessionId = ByteString

-- | A set with all the session ids
type UsersSessions = HashSet SessionId

-- | The Users data type, stores all the User(s) and the set of
-- sessions for the logged in users
data Users = Users { usersMap :: UsersMap
                   , usersSessions :: UsersSessions
                   }
             deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version Users
$(deriveSerialize ''Users)

instance Component Users where
  type Dependencies Users = End
  initialValue = Users M.empty S.empty

-- Query / Updates

-- | Gets the map of users
getUsers :: Query Users UsersMap
getUsers = asks usersMap

-- | Defines how many passes of the hash function
hashStrength :: Int
hashStrength = 12

-- | Gets username and password and updates the users map
insertUser :: ByteString -> ByteString -> Salt -> Update Users ()
insertUser un passwd salt =
  modify (\s -> s { usersMap = M.insert un user' (usersMap s) })
  where
    hashedp = makePasswordSalt passwd salt hashStrength
    user'   = User un hashedp Member

-- | Get the set with all the active sessions
getSessions :: Query Users UsersSessions
getSessions = asks usersSessions

-- | Inserts a new session. Accepts a Salt since I'll generate the
-- random bit with genSaltIO.
insertSession :: Username -> Salt -> Update Users String
insertSession un salt = do
  modify (\s -> s { usersSessions = S.insert sid (usersSessions s) })
  return $ B.unpack sid
  where
    sid = B.concat [un, "|", exportSalt salt]

-- | Deletes a session from the session set.
deleteSession :: SessionId -> Update Users ()
deleteSession sid =
  modify (\s -> s { usersSessions = S.delete sid (usersSessions s) })

-- | Returns the User if the session is present, Nothing if the
-- session is not. It also returns Nothing if the session id is
-- malformed.
checkSession :: SessionId -> Query Users (Maybe User)
checkSession sid
  | B.length salt < 2 = return Nothing
  | otherwise         = do
    sessions <- getSessions
    if S.member sid sessions
      then do
        users <- getUsers
        case M.lookup username users of
          Just user -> return $ Just user
          Nothing   -> return Nothing
      else return Nothing
  where (username, salt) = B.break (== '|') sid

-- Generate the update/query
$(mkMethods ''Users [ 'getUsers, 'insertUser
                    , 'getSessions, 'insertSession, 'deleteSession, 'checkSession
                    ])
