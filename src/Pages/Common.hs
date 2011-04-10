{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Common (
    template
  , PageM
  , TemplateM
  , render
--, e404
--, e500
  ) where


import Happstack.Server

import HSP
import HSP.ServerPartT         ()
import qualified HSX.XMLGenerator as HSX

import Happstack.Server.HSP.HTML ()

import Web.Routes
import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()

import Types
import Routes.Types


type PageM = RouteT Route AppM
type TemplateM = XMLGenT PageM (HSX.XML PageM)


render :: PageM XML -> PageM Response
render = (=<<) (ok . toResponse) 

template :: Route
            -> (String, Maybe [TemplateM], [TemplateM])
            -> PageM (HSX.XML PageM)
template r (title, heading, content) =
  unXMLGenT $
    <html>
      
      <head>
        <link href=(R_Static ["css", "reset.css"]) media="screen" rel="stylesheet" type="text/css" />
        <link href=(R_Static ["css", "style.css"]) media="screen" rel="stylesheet" type="text/css" />
        <title> <% "Reskell - " ++ title %> </title>
      </head>
      
      <body>
        <div id="header">
          <h1><a href=(R_Listing Links New)>Reskell</a></h1>
          
          menu...
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