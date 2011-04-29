{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module Types.App
       ( AppM
       , unpackApp
       , AppError (..)
       , notFoundError
       , serverError
       , forbiddenError
       , PageM
       , TemplateM
         
    
       , Context (..)
       , MonadContext (..)
       , askContext
         
       , userQuery
       , userUpdate
       , postQuery
       , postUpdate
       , dbTime
       -- ** Stuff that's often useful  
       , MonadError
       , MonadIO
       , ReaderT
       , liftIO
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


data Context = Context { sessionUser :: Maybe User
                       , postDB      :: AcidState PostDB
                       , userDB      :: AcidState UserDB
                       }


data AppError = NotFound
              | ServerError String
              | Forbidden
              | OtherError String
              deriving (Show)

instance Error AppError where
  noMsg  = OtherError ""
  strMsg = OtherError

type AppM = ServerPartT (ErrorT AppError (ReaderT Context IO))

notFoundError  :: MonadError AppError m => m a
notFoundError  = throwError NotFound
serverError    :: MonadError AppError m => String -> m a
serverError    = throwError . ServerError
forbiddenError :: MonadError AppError m => m a
forbiddenError = throwError Forbidden


unpackApp :: Context
             -> UnWebT (ErrorT AppError (ReaderT Context IO)) a
             -> UnWebT IO a
unpackApp context = unpackContext . unpackError
  where
    unpackContext ct = runReaderT ct context

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

userQuery :: ( MonadContext m
            , MonadIO m
            , QueryEvent event
            , EventState event ~ UserDB
            ) => event -> m (EventResult event)
userQuery = ctxQuery userDB
userUpdate :: ( MonadContext m
             , MonadIO m
             , UpdateEvent event
             , EventState event ~ UserDB
             ) => event -> m (EventResult event)
userUpdate = ctxUpdate userDB

postQuery :: ( MonadContext m
            , MonadIO m
            , QueryEvent event
            , EventState event ~ PostDB
            ) => event -> m (EventResult event)
postQuery = ctxQuery postDB
postUpdate :: ( MonadContext m
             , MonadIO m
             , UpdateEvent event
             , EventState event ~ PostDB
             ) => event -> m (EventResult event)
postUpdate = ctxUpdate postDB

dbTime :: MonadIO m => (t -> m b) -> (UTCTime -> t) -> m b
dbTime q act = do
  time <- liftIO getCurrentTime
  q (act time)


type PageM = RouteT Route AppM
type TemplateM = XMLGenT PageM (HSX.XML PageM)