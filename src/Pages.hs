{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages ( 
    dispatch
  ) where

import HSP
import HSP.ServerPartT         ()

import Happstack.Server.HSP.HTML ()

import Web.Routes
import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()

import Types
import Routes.Types



dispatch :: Route -> RouteT Route ContextM XML
dispatch u@(R_Post id') = postPage id' >>= layout u
dispatch _              = uxg $ <h2> not yet implemented </h2>

uxg = unXMLGenT

baseTitle :: String
baseTitle = "Reskell - "

type Page = (String, String, XML)

layout :: Route -> Page -> RouteT Route ContextM XML
layout u (title, heading, content) = do
  url <- showURL u
  uxg $
    <html>
      
      <head>
        <title> <% baseTitle ++ title %> </title>
      </head>
      
      <body>
        <div id="header">
          <h1> <% heading %> </h1>
          
          menu...
        </div>
        
        <div id="content">
          <% content %>
        </div>
        
        <div id="footer">
          footer...
        </div>
      </body>
      
    </html>

postPage :: PostId -> RouteT Route ContextM Page
postPage id' = do
  c <- uxg $ <h2> post post post </h2>
  return ("Post n " ++ (show id'), "heading for teh post!", c)
