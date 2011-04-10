{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages ( 
    dispatch
  ) where



import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Types
import Routes.Types
import DB
import Pages.Common
import Pages.Post




dispatch :: Route -> PageM Response
dispatch r@(R_Post id') = query (getPost id') >>= maybe notFoundError (postPage r)
dispatch r = render $ template r ("", Nothing, [<h2> not yet implemented </h2>])