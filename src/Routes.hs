module Routes (
    runRoutes
  ) where

import Happstack.Server

import Web.Routes
import Web.Routes.Happstack

import Pages
import Types


siteSpec :: Site Route (AppM Response)
siteSpec = setDefault home
           Site { handleSite = \f u -> unRouteT (dispatch u) f
                , formatPathSegments = \u -> (toPathSegments u, [])
                , parsePathSegments  = parseSegments fromPathSegments
                }


runRoutes :: AppM Response
runRoutes = implSite "/" "" siteSpec
