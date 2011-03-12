module Handlers (
  handlers
  ) where

import Control.Monad (msum)

import Happstack.Server

import Handlers.User

import Config
import Templates

handlers :: ServerPart Response
handlers = do
  decodeBody appPolicy
  msum [ dir "user" userHandlers
       , renderTemplate indexTemplate
       , serveDirectory DisableBrowsing [] resourcesDir
       ]
