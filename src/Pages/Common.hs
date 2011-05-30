{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Common
       ( template
       , render
       , renderForm
       , separator
       , seeOther'
       , postsPerPage
       , markdownComment
       , markdownSubmission
         --, e404
         --, e500
       ) where


import Control.Monad           (liftM)

import qualified Data.ByteString.UTF8 as UTF8
import Happstack.Server
import Happstack.Server.SURI   (ToSURI)

import HSP hiding (Request)
import qualified HSX.XMLGenerator as HSX

import Text.Upskirt
import Text.Upskirt.Renderers.Html

import Types



{-|
Renders the 'XML' and returns 200:
 
@
    (ok . toResponse =<<)
@
-}
render :: PageM XML -> PageM Response
render = (ok . toResponse =<<)

-- | Base template.
template :: (String, Maybe [TemplateM], [TemplateM])
            -- ^ A title, an optional heading that will be enclosed in
            -- @<h2>@, and the body.
            -> PageM (HSX.XML PageM)
template (title, heading, content) =
  unXMLGenT $
    <html>
      
      <head>

        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

        <link href=(R_Static ["css", "style.css"]) media="screen" rel="stylesheet" type="text/css" />

        <title> <% "Reskell - " ++ title %> </title>

      </head>
      
      <body>

        <div id="header">
          <h1><a href=(home)>Reskell</a></h1>
          
          <a href=(R_Submissions Asks 0 Nothing Top)>ask</a>
          <% separator %>
          <a href=(R_Submissions Links 0 Nothing Top)>links</a>
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
          <% separator %>
          <a href="https://github.com/rostayob/reskell">
            fork me on github
          </a>
        </div>

      </body>
      
    </html>


renderForm :: [TemplateM]
              -- ^ Form already in the proper form - see 'eitherHappstackForm'.
              -> String
              -- ^ String to put in the submit button.
              -> TemplateM
renderForm form submit = do
  { Request {rqUri = uri, rqQuery = query} <- askRq 
  ; <form method="POST" enctype="multipart/form-data" action=(uri ++ query)>
      <% form %>
      <input type="submit" value=submit />
    </form>
  }


-- | A simple separator - @&middot;@.
separator :: String
separator = " · "

-- | 'seeOther' but with an empty string as a message.
seeOther' :: (ToSURI uri, FilterMonad Response m) => uri -> m Response
seeOther' uri = liftM toResponse (seeOther uri "")

-- | The number of posts to display in one page.
postsPerPage :: PageNumber
postsPerPage = 50

-- | Markdown comment
markdownComment :: String -> XML
markdownComment text = cdata html
  where
    html = UTF8.toString $ renderHtml (UTF8.fromString $ text)
                                      noExtensions noHtmlModes

-- | Markdown submission
markdownSubmission :: String -> XML
markdownSubmission text = cdata html
  where
    html = UTF8.toString $ renderHtml (UTF8.fromString text)
                                      noExtensions noHtmlModes

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
