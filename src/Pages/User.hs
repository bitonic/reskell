{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.User (
    loginPage
  , registerPage
  , userPage
  , cpPage
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
                  ] ++ (if null (uAbout user)
                        then []
                        else [<p><b>About:</b> <% uAbout user %></p>]) ++
                  [ <p><b><a href=(R_Submissions Submissions 0 (Just (uName user)) Top)>Submissions</a></b>, <b><a href=(R_Comments 0 (uName user) Top)>Comments</a></b></p>
                  ]
                )

cpPage :: [TemplateM] -> PageM Response
cpPage form = render $ template
              ( "User CP"
              , Nothing
              , [ <p>Here you can edit your profile. To change the password, you have to insert the old password.</p>
                , renderForm form "Save"
                ]
              )
                