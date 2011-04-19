{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  ) where  


import Happstack.Server

import HSP

import Types
import Pages.Common



loginPage :: Route -> [TemplateM] -> PageM Response
loginPage r form = render $ template r ( "Login"
                                       , Just [<span>Login</span>]
                                       , [renderForm form r "Submit"]
                                       )
