{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Common (
    template
  , PageM
  , TemplateM
  , render
  , renderForm
--, e404
--, e500
  ) where


import Happstack.Server

import HSP
import HSP.ServerPartT         ()
import qualified HSX.XMLGenerator as HSX

import Happstack.Server.HSP.HTML ()

import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()

import Types


render :: PageM XML -> PageM Response
render = (=<<) (ok . toResponse) 

template :: Route
            -> (String, Maybe [TemplateM], [TemplateM])
            -> PageM (HSX.XML PageM)
template r (title, heading, content) =
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
          <h1><a href=(R_Listing Links New)>Reskell</a></h1>
          
          <% case r of
               R_Listing Submissions _ -> <span>all</span>
               _ -> <a href=(home)>all</a>
             %>
          <% " · " %>
          <% case r of
               R_Listing Asks _ -> <span>ask</span>
               _ -> <a href=(R_Listing Asks Top)>ask</a>
             %>
          <% " · " %>
          <% case r of
               R_Listing Links _ -> <span>links</span>
               _ -> <a href=(R_Listing Links Top)>links</a>
             %>
          <% " · " %>
          <% case r of
               R_Submit -> <span>submit</span>
               _ -> <a href=(R_Submit)>submit</a>
             %>

          <div id="headerRight">
            <% do user <- askContext sessionUser
                  case user of
                    Nothing -> <%><% case r of
                                      R_Register _ -> <span>register</span>
                                      _ -> <a href=(R_Register r)>register</a>
                                    %>
                                 <% " · " %>
                                 <% case r of
                                     R_Login _ -> <span>login</span>
                                     _ -> <a href=(R_Login r)>login</a>
                                   %>
                                 </%>
                    Just u -> <%><% if r == (R_User (uName u))
                                   then <span><% uName u %></span>
                                   else <a href=(R_User (uName u))><% uName u %></a>
                                   %>
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


renderForm :: [TemplateM] -> Route -> String -> TemplateM
renderForm form action submit =
  <form method="POST" enctype="multipart/form-data" action=action>
    <% form %>
    <input type="submit" value=submit />
  </form>


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
