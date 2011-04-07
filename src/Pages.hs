{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages ( 
    dispatch
  ) where



import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Web.Routes

import Types
import Routes.Types
import DB
import Pages.Common
import Pages.Post




dispatch :: Route -> RouteT Route ContextM Response
dispatch R_404          = e404
dispatch r@(R_Post id') = query (getPost id') >>= maybe e404 (postPage r)
dispatch r = render $ template r (tt "", Nothing, <h2> not yet implemented </h2>)