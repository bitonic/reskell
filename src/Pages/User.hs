{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  , registerPage
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

registerPage :: [TemplateM] -> PageM Response
registerPage form = render $ template ( "Register"
                                      , Just [<span>Register</span>]
                                      , [renderForm form "Register"]
                                      )
