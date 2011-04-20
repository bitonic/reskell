{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  ) where  


import Happstack.Server

import HSP

import Types
import Pages.Common



loginPage :: [TemplateM] -> PageM Response
loginPage form = render $ template ( "Login"
                                   , Just [<span>Login</span>]
                                   , [renderForm form "Submit"]
                                   )
