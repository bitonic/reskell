module Routes (
    runRoutes
  ) where

import Happstack.Server

import Web.Routes
import Web.Routes.Happstack

import Types
import Pages
import Routes.Types



siteSpec :: Site Route (ContextM Response)
siteSpec = setDefault (R_Listing Links New)
           Site { handleSite = \f u -> unRouteT (dispatch u) f
                , formatPathSegments = \u -> (toPathSegments u, [])
                , parsePathSegments  = parseSegments fromPathSegments
                }

runRoutes :: Context -> ServerPart Response
runRoutes context = mapServerPartT (unpackContext context) $ implSite "/" "" siteSpec