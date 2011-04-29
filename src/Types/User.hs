{-# Language TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.User
       ( -- * Types definitions
         UserRank (..)
       , User (..)
       , UserNameIx (..)
       , RankIx (..)
       , CreatedIx (..)
       , UserName
       , Password
       , SessionId
       , Session (..)
       , genSessionId
       , sessionCookie
       , UserDB (..)
       , hashStrength
         
         -- * Query / Updates
       , openUserDB
       , NewUser (..)
       , GetUser (..)
       , UpdateUser (..)
         
       , NewSession (..)
       , CheckSession (..)
       , DeleteSession (..)
       ) where


import Control.Monad.Reader
import Control.Monad.State     (modify)

import System.FilePath         ((</>))

import Data.Bson               (ObjectId (..), genObjectId)
import Data.Data               (Data, Typeable)
import Data.Time.Clock         (UTCTime)
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Acid

import Data.SafeCopy

import Happstack.Data.IxSet    (IxSet, (@=), Indexable, ixSet, ixFun)
import qualified Happstack.Data.IxSet as Ix

import Crypto.PasswordStore

import Numeric                 (showHex)

import Types.Common            ()

data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

type UserName = String
type Password = ByteString

data User = User { uName     :: UserName
                 , uPassword :: Password
                 , uRank     :: UserRank
                 , uAbout    :: String
                 , uCreated  :: UTCTime
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)

hashStrength :: Int
hashStrength = 12

newtype UserNameIx = UserNameIx UserName
                   deriving (Eq, Ord, Data, Typeable)
newtype RankIx     = RankIx UserRank
                   deriving (Eq, Ord, Data, Typeable)
newtype CreatedIx  = CreatedIx UTCTime
                   deriving (Eq, Ord, Data, Typeable)

instance Indexable User where
  empty = ixSet [ ixFun (\u -> [UserNameIx (uName u)])
                , ixFun (\u -> [RankIx (uRank u)])
                , ixFun (\u -> [CreatedIx (uCreated u)])
                ]

sessionCookie :: String
sessionCookie = "session"

type SessionId = ByteString
                 
data Session = Session { sessionId       :: ByteString
                       , sessionUserName :: UserName
                       , sessionTime     :: UTCTime
                       }
             deriving (Eq, Ord, Read, Show, Data, Typeable)

genSessionId :: IO SessionId
genSessionId = do
  (Oid a b) <- genObjectId
  salt <- liftM (B8.unpack . exportSalt) genSaltIO
  return $ B8.pack (showHex a . showHex b $ salt)


data UserDB = UserDB { userSessions :: HashMap SessionId Session
                     , userSet      :: IxSet User
                     }

$(deriveSafeCopy 0 'base ''UserRank)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''UserDB)

  
----------------------------------------------------------------------  

  
openUserDB :: FilePath -> IO (AcidState UserDB)
openUserDB fp = openAcidStateFrom (fp </> "UserDB") $ UserDB HM.empty Ix.empty

newUser :: User -> Update UserDB ()
newUser user = modify (\s -> s {userSet = Ix.insert user (userSet s)})

getUser :: UserName -> Query UserDB (Maybe User)
getUser userName = do
  users <- asks userSet
  return $ Ix.getOne (users @= (UserNameIx userName))

updateUser :: User -> Update UserDB ()
updateUser user =
  modify (\s -> s {userSet = Ix.updateIx (UserNameIx (uName user)) user (userSet s)})


newSession :: SessionId -> UserName -> UTCTime -> Update UserDB ()
newSession sId userName now = do
  let session = Session sId userName now
  modify (\s -> s {userSessions = HM.insert sId session (userSessions s)})

checkSession :: SessionId -> Query UserDB (Maybe User)
checkSession sId = do
  db <- ask
  return $ do
    Session _ userName _ <- HM.lookup sId (userSessions db)
    Ix.getOne $ userSet db @= UserNameIx userName


deleteSession :: SessionId -> Update UserDB ()
deleteSession sId = modify (\s -> s {userSessions = HM.delete sId (userSessions s)})

$(makeAcidic ''UserDB [ 'newUser
                      , 'getUser
                      , 'updateUser
                        
                      , 'newSession
                      , 'checkSession
                      , 'deleteSession
                      ])
