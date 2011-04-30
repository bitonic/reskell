{-# Language TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.User
       ( -- * Types definitions
         UserRank (..)
       , User (..)
       , UserName
       , Password
       , SessionId
       , Session (..)
       , sessionCookie
       , hashStrength
         
         -- * Indexes
       , UserNameIx (..)
       , RankIx (..)
       , CreatedIx (..)
         
         -- * DB
       , openUserDB
       , UserDB (..)
         
         -- * Query / Updates
       , NewUser (..)
       , GetUser (..)
       , UpdateUser (..)
         
       , NewSession (..)
       , CheckSession (..)
       , DeleteSession (..)
         
         -- * Utils
       , genSessionId
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




type UserName = String
type Password = ByteString

data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)


data User = User { uName     :: UserName
                 , uPassword :: Password
                   -- ^ A hashed password, to be generated with
                   -- 'makePassword'.
                 , uRank     :: UserRank
                 , uAbout    :: String
                 , uCreated  :: UTCTime
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)


-- | The strength to be used with 'makePassword'.
hashStrength :: Int
hashStrength = 12



-- | The 'SessionId' that will be stored in the cookie.
type SessionId = ByteString
                 
data Session = Session { sessionId       :: ByteString
                       , sessionUserName :: UserName
                       , sessionTime     :: UTCTime
                       }
             deriving (Eq, Ord, Read, Show, Data, Typeable)


-- | The name of the cookie in which to store the 'SessionId'.
sessionCookie :: String
sessionCookie = "session"


-------------------------------------------------------------------------------

newtype UserNameIx = UserNameIx UserName
                   deriving (Eq, Ord, Data, Typeable)
newtype RankIx = RankIx UserRank
               deriving (Eq, Ord, Data, Typeable)
newtype CreatedIx = CreatedIx UTCTime
                  deriving (Eq, Ord, Data, Typeable)

instance Indexable User where
  empty = ixSet [ ixFun (\u -> [UserNameIx (uName u)])
                , ixFun (\u -> [RankIx (uRank u)])
                , ixFun (\u -> [CreatedIx (uCreated u)])
                ]

-------------------------------------------------------------------------------


data UserDB = UserDB { userSessions :: HashMap SessionId Session
                     , userSet      :: IxSet User
                     }

$(deriveSafeCopy 0 'base ''UserRank)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''UserDB)

  
-- | Initializes the 'AcidState', to be called when the application
-- starts.
openUserDB :: FilePath -> IO (AcidState UserDB)
openUserDB fp = openAcidStateFrom (fp </> "UserDB") $ UserDB HM.empty Ix.empty
  
----------------------------------------------------------------------  

  
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

-------------------------------------------------------------------------------

-- | Generates a safe 'SessionId'. Uses 'genObjectId' from bson, that
-- getsa unique id based on various factors (the current time and the
-- process id) + a random salt generated by 'genSaltIO'.
genSessionId :: IO SessionId
genSessionId = do
  (Oid a b) <- genObjectId
  salt <- liftM (B8.unpack . exportSalt) genSaltIO
  return $ B8.pack (showHex a . showHex b $ salt)
