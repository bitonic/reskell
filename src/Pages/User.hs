{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  ) where  


import HSP

import Pages.Common



loginPage r form = render $ template r ( "Login"
                                       , Just [<span>Login</span>]
                                       , [renderForm form r "Submit"])