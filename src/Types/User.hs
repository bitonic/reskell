{-# Language TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.User
       ( -- * Types definitions
         UserRank (..)
       , User (..)
       , UserName
       , Password
       , Karma
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
       , UpdateKarma (..)
         
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
import Data.Time.Clock         (UTCTime, getCurrentTime)
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
type Karma    = Int

data UserRank = Member | Moderator | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserRank)


data User = User { uName     :: UserName
                 , uPassword :: Password
                   -- ^ A hashed password, to be generated with
                   -- 'makePassword'.
                 , uRank     :: UserRank
                 , uAbout    :: String
                 , uCreated  :: UTCTime
                 , uKarma    :: Karma
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'extension ''User)

-- | The strength to be used with 'makePassword'.
hashStrength :: Int
hashStrength = 12



type SessionId = ByteString
                 
data Session = Session { sessionId       :: SessionId
                         -- ^ The 'SessionId' that will be stored in the cookie.
                         -- Should be generated to be unguessable, see 'genSessionId'.
                       , sessionUserName :: UserName
                       , sessionTime     :: UTCTime
                       }
             deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Session)


-- | The name of the cookie in which to store the 'SessionId'.
sessionCookie :: String
sessionCookie = "session"


-------------------------------------------------------------------------------

-- Migrations

data User_v0 = User_v0 UserName Password UserRank String UTCTime

$(deriveSafeCopy 0 'base ''User_v0)
  
instance Migrate User where
  type MigrateFrom User = User_v0
  migrate (User_v0 username password rank about time) =
    User username password rank about time 0

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
$(deriveSafeCopy 0 'base ''UserDB)

  
-- | Initializes the 'AcidState', to be called when the application
-- starts.
openUserDB :: FilePath
              -- ^ The base path in which to save the state, a
              -- subdirectory named "UserDB" will be used to store the
              -- 'UserDB' 'AcidState'.
              -> IO (AcidState UserDB)
openUserDB fp = do
  adminPass <- makePassword (B8.pack "admin") hashStrength
  time <- getCurrentTime
  let admin = User { uName     = "admin"
                   , uPassword = adminPass
                   , uRank     = Admin
                   , uAbout    = ""
                   , uCreated  = time
                   , uKarma    = 0
                   }
  openAcidStateFrom (fp </> "UserDB") $ UserDB HM.empty (Ix.fromList [admin])
  
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

updateKarma :: UserName -> Karma -> Update UserDB ()
updateKarma userName karma =
  runQuery (getUser userName) >>= maybe (return ()) (\user ->
    updateUser (user {uKarma = uKarma user + karma}))


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
                      , 'updateKarma
                        
                      , 'newSession
                      , 'checkSession
                      , 'deleteSession
                      ])

-------------------------------------------------------------------------------

-- | Generates a safe 'SessionId'. Uses 'genObjectId' from bson, that
-- gets a unique id based on various factors (the current time and the
-- process id) + a random salt generated by 'genSaltIO'.
genSessionId :: IO SessionId
genSessionId = do
  (Oid a b) <- genObjectId
  salt <- liftM (B8.unpack . exportSalt) genSaltIO
  return $ B8.pack (showHex a . showHex b $ salt)
