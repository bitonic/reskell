{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}

module Types.App (
    AppM
  , unpackApp
  , AppError (..)
  , notFoundError
  , serverError
  , databaseError
  , forbiddenError
  , PageM
  , TemplateM
    
    
  , Context (..)
  , MonadContext (..)
  , askContext
    
  -- ** Stuff that's often useful  
  , MonadError
  , MonadIO
  , ReaderT
  ) where




import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.MVar

import Data.Time.Clock         (UTCTime)

import Happstack.Server hiding (Host)

import Database.MongoDB        (Database (..), ConnPool, Failure, Service)

import Web.Routes              (RouteT)

import HSP                     (XMLGenT)
import qualified HSX.XMLGenerator as HSX


-- instances
import Happstack.Server.HSP.HTML ()
import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()
import HSP.ServerPartT         ()
import Text.Digestive.Forms.Happstack ()

import Types.User
import Types.Route


data Context = forall s. Service s => Context { database    :: Database
                                        , connPool    :: ConnPool s
                                        , sessionUser :: Maybe User
                                        , currTime    :: UTCTime
                                                
                                        , userMVar    :: MVar ()
                                        }


data AppError = DatabaseError Failure
              | NotFound
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
databaseError  :: MonadError AppError m => Failure -> m a
databaseError  = throwError . DatabaseError
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

instance MonadContext (ServerPartT (ErrorT AppError (ReaderT Context IO))) where
  getContext = ask
  
instance MonadContext m => MonadContext (RouteT url m) where
  getContext = lift getContext

instance MonadContext m => MonadContext (XMLGenT (RouteT url m)) where
  getContext = lift getContext

askContext :: MonadContext m => (Context -> r) -> m r
askContext = (`liftM` getContext)


type PageM = RouteT Route AppM
type TemplateM = XMLGenT PageM (HSX.XML PageM)