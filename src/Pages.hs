

module Pages ( 
    routes
  ) where

import Happstack.Server

import Context

routes :: ContextM Response
routes = ok $ toResponse "blah"