{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module Types.App
       ( -- * The Context
         Context (..)
       , MonadContext (..)
       , askContext

         -- * Error reporting
       , AppError (..)
       , notFoundError
       , serverError
       , forbiddenError
         
         -- * The application monads
       , AppM
       , unpackApp
       , PageM
       , TemplateM
         
    
         -- * Utilities to query the DBs
       , userQuery
       , userUpdate
       , postQuery
       , postUpdate
       , dbTime
       ) where




import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error

import Data.Acid
import Data.Time.Clock
  
import Happstack.Server hiding (Host)

import Web.Routes              (RouteT)

import HSP                     (XMLGenT)
import qualified HSX.XMLGenerator as HSX



-- instances
import Happstack.Server.HSP.HTML ()
import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()
import HSP.ServerPartT         ()
import Text.Digestive.Forms.Happstack ()
import Happstack.Server.HSX    ()



import Types.User
import Types.Post
import Types.Route




-- | The application context.
data Context = Context { sessionUser :: Maybe User
                         -- ^ The 'User' that's logged in, if present
                       , postDB      :: AcidState PostDB
                         -- ^ The 'AcidState' for the 'PostDB'
                       , userDB      :: AcidState UserDB
                         -- ^ The 'AcidState' for the 'UserDB'
                       }


-- | A 'Monad' with a 'Context' in it.
class Monad m => MonadContext m where
  getContext :: m Context

instance Monad m => MonadContext (ReaderT Context m) where
  getContext = ask

instance MonadContext (ServerPartT (ErrorT AppError (ReaderT Context IO))) where
  getContext = ask
  
instance MonadContext m => MonadContext (RouteT url m) where
  getContext = lift getContext

instance MonadContext m => MonadContext (XMLGenT (RouteT url m)) where
  getContext = lift getContext

askContext :: MonadContext m => (Context -> r) -> m r
askContext = (`liftM` getContext)



-------------------------------------------------------------------------------

-- | Data type to invoke errors easily anywhere in the application
data AppError = NotFound
              | ServerError String
              | Forbidden
              | OtherError String
              deriving (Show)

instance Error AppError where
  noMsg  = OtherError ""
  strMsg = OtherError


notFoundError  :: MonadError AppError m => m a
notFoundError  = throwError NotFound
serverError    :: MonadError AppError m => String -> m a
serverError    = throwError . ServerError
forbiddenError :: MonadError AppError m => m a
forbiddenError = throwError Forbidden


-------------------------------------------------------------------------------

-- | The main 'Monad' of the application
type AppM = ServerPartT (ErrorT AppError (ReaderT Context IO))

-- | Transforms 'AppM' to a 'ServerPartT'
unpackApp :: Context
             -> UnWebT (ErrorT AppError (ReaderT Context IO)) a
             -> UnWebT IO a
-- Runs the 'ErrorT' and the 'ReaderT'
unpackApp context = unpackContext . unpackError
  where
    unpackContext ct = runReaderT ct context

    -- Error reporting, one day it will be better
    unpackError et = do
      eitherV <- runErrorT et
      return $ case eitherV of
        Left err -> Just (Left $ toResponse $ 
                          "Catastrophic failure: " ++ show err,
                          filterFun $ \r -> r { rsCode = getCode err })
        Right x -> x

    getCode NotFound  = 400
    getCode Forbidden = 403
    getCode _         = 500


-- Monads relevant to the routing and templating
type PageM = RouteT Route AppM
type TemplateM = XMLGenT PageM (HSX.XML PageM)


-------------------------------------------------------------------------------


ctxUpdate :: (MonadContext m, UpdateEvent event, MonadIO m)
             => (Context -> AcidState (EventState event))
             -> event
             -> m (EventResult event)
ctxUpdate db e = askContext db >>= \es -> update' es e
ctxQuery :: (MonadContext m, QueryEvent event, MonadIO m)
            => (Context -> AcidState (EventState event))
            -> event
            -> m (EventResult event)
ctxQuery db e = askContext db >>= \es -> query' es e

-- | Queries 'UserDB'
userQuery :: ( MonadContext m
            , MonadIO m
            , QueryEvent event
            , EventState event ~ UserDB
            ) => event -> m (EventResult event)
userQuery = ctxQuery userDB

-- | Updates 'UserDB'
userUpdate :: ( MonadContext m
             , MonadIO m
             , UpdateEvent event
             , EventState event ~ UserDB
             ) => event -> m (EventResult event)
userUpdate = ctxUpdate userDB


-- | Queries 'PostDB'
postQuery :: ( MonadContext m
            , MonadIO m
            , QueryEvent event
            , EventState event ~ PostDB
            ) => event -> m (EventResult event)
postQuery = ctxQuery postDB

-- | Updates 'PostDB'
postUpdate :: ( MonadContext m
             , MonadIO m
             , UpdateEvent event
             , EventState event ~ PostDB
             ) => event -> m (EventResult event)
postUpdate = ctxUpdate postDB

-- | Wrapper for acid-state functions that need the time.
dbTime :: MonadIO m => (t -> m b) -> (UTCTime -> t) -> m b
dbTime q act = do
  time <- liftIO getCurrentTime
  q (act time)