{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  , registerPage
  , userPage
  ) where  


import Data.Time.Format
import System.Locale           (defaultTimeLocale)
  
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


userPage :: User -> PageM Response
userPage user = render $ template
                ( uName user
                , Just [<span><% uName user %></span>]
                , [ <p><b>Member since:</b>
                      <% " " ++ formatTime defaultTimeLocale "%F" (uCreated user) %>
                    </p>
                  , <p><b>Rank:</b> <% show (uRank user) %></p>
                  , <p><b>About:</b> <% uAbout user %></p>
                  ]
                )
