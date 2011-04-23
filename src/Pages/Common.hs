{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Common (
    template
  , PageM
  , TemplateM
  , render
  , renderForm
  , separator
  , seeOther'
  , postsPerPage
--, e404
--, e500
  ) where


import Control.Monad           (liftM)

import Happstack.Server
import Happstack.Server.SURI   (ToSURI)

import HSP hiding (Request)
import qualified HSX.XMLGenerator as HSX

import Types


render :: PageM XML -> PageM Response
render = (ok . toResponse =<<)

template :: (String, Maybe [TemplateM], [TemplateM])
            -> PageM (HSX.XML PageM)
template (title, heading, content) =
  unXMLGenT $
    <html>
      
      <head>

        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

        <link href=(R_Static ["css", "reset.css"]) media="screen" rel="stylesheet" type="text/css" />
        <link href=(R_Static ["css", "style.css"]) media="screen" rel="stylesheet" type="text/css" />

        <title> <% "Reskell - " ++ title %> </title>

      </head>
      
      <body>

        <div id="header">
          <h1><a href=(home)>Reskell</a></h1>
          
          <a href=(R_Submissions Asks Top 0)>ask</a>
          <% separator %>
          <a href=(R_Submissions Links Top 0)>links</a>
          <% separator %>
          <a href=(R_Submit)>submit</a>


          <div id="headerRight">
            <% do user <- askContext sessionUser
                  register <- routeRedirect R_Register
                  login <- routeRedirect R_Login
                  case user of
                    Nothing -> <%>
                                <a href=register>register</a>
                                <% separator %>
                                <a href=login>login</a>
                              </%>
                    Just u -> <%>
                               <a href=(R_CP)><% uName u %></a>
                               <% separator %>
                               <a href=(R_Logout)>logout</a>
                             </%>
            %>
          </div>
        </div>
        
        <div id="content">
          <% case heading of 
               Nothing -> []
               Just h  -> [<h2> <% h %> </h2>]
          %>  
          <% content %>
        </div>
        
        <div id="footer">
          rostayob industries.
        </div>

      </body>
      
    </html>


renderForm :: [TemplateM] -> String -> TemplateM
renderForm form submit = do
  { Request {rqUri = uri, rqQuery = query} <- askRq 
  ; <form method="POST" enctype="multipart/form-data" action=(uri ++ query)>
      <% form %>
      <input type="submit" value=submit />
    </form>
  }


separator :: String
separator = " · "


seeOther' :: (ToSURI uri, FilterMonad Response m) => uri -> m Response
seeOther' uri = liftM toResponse (seeOther uri "")

postsPerPage :: PageNumber
postsPerPage = 50


{-
e404 :: PageM Response
e404 = do
  let c = <h2> 404 - The page you're looking for does not exist. </h2>
  notFound . toResponse =<< template R_404 ("404 - Not Found", Nothing, [c])

e500 :: PageM Response
e500 = do
  let c = <h2> 500 - Internal server error. </h2>
  internalServerError . toResponse =<<
    template R_404 ("500 - Internal Server Error", Nothing, [c])
-}
