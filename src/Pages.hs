

module Pages ( 
    routes
  ) where

import Happstack.Server

import Config

routes :: ConfigT IO Response
routes = ok $ toResponse "blah"