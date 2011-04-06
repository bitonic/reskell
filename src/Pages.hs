{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages ( 
    dispatch
  ) where


import Control.Monad           (liftM)
import Control.Monad.Trans     (lift, liftIO, MonadIO)
import Control.Monad.Reader    (ask)

import Data.Maybe              (maybe)
import Data.Time.Clock
import Data.Text               (Text)
import qualified Data.Text as T

import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Web.Routes
import Web.Routes.XMLGenT      ()
import Web.Routes.Happstack    ()

import Types
import Routes.Types
import DB



import Data.Maybe (fromJust)


tt :: String -> Text
tt = T.pack

getContext = lift ask
askContext = (`liftM` getContext)



dispatch :: Route -> RouteT Route ContextM Response
dispatch R_404          = e404
dispatch r@(R_Post id') = query (getPost id') >>= maybe e404 (postPage r)
dispatch _ = render (uxg $ <h2> not yet implemented </h2>)


render :: RouteT Route ContextM XML -> RouteT Route ContextM Response
render = (=<<) (ok . toResponse) 

e404 :: RouteT Route ContextM Response
e404 = do
  c <- uxg $ <h2> 404 - The page you're looking for does not exist. </h2>
  notFound . toResponse =<< layout R_404 (tt "404 - Not Found", Nothing, c)

e500 :: RouteT Route ContextM Response
e500 = do
  c <- uxg $ <h2> 500 - Internal server error. </h2>
  internalServerError . toResponse =<< layout R_404 (tt "500 - Internal Server Error", Nothing, c)

uxg = unXMLGenT

type Page = (Text, Maybe Text, XML)

layout :: Route -> Page -> RouteT Route ContextM XML
layout r (title, heading, content) = do
  url <- showURL r
  uxg $
    <html>
      
      <head>
        <title> <% T.concat [tt "Reskell - ", title] %> </title>
      </head>
      
      <body>
        <div id="header">
          <h1> Reskell </h1>
          
          menu...
        </div>
        
        <div id="content">
          <%
            case heading of 
              Nothing -> []
              Just h  -> [<h2> h </h2>]
          %>  
          <% content %>
        </div>
        
        <div id="footer">
          footer...
        </div>
      </body>
      
    </html>



showTimeDiff :: UTCTime -> UTCTime -> String
showTimeDiff t1 t2 | diff < min   = "just now"
                   | diff < hour  = plural "minute" $ diff /// min
                   | diff < day   = plural "hour" $   diff /// hour
                   | diff < month = plural "day" $    diff /// day
                   | diff < year  = plural "month" $  diff /// month
                   | otherwise    = plural "year" $   diff /// year
                   
  where
    min = 60
    hour = min * 60
    day = hour * 24
    month = day * 30
    year = day * 365
    
    (///) = (truncate .) . (/)
    
    diff = diffUTCTime t1 t2
    
    plural t n = " " ++ show n ++ " " ++ t ++ s ++ "ago"
      where s | n > 1 = "s "
              | otherwise = " "    
    
whenPosted p = do
  now <- askContext currTime
  return $ T.concat [tt (show $ postVotes p), tt " points by ",
                     (postUserName p), tt (showTimeDiff now $ postTime p)]

{-
renderComments :: Post a => a -> RouteT Route ContextM XML
renderComments p = do
  comments <- query $ getComments p
  let showComm
-}

-- renderComment :: Comment -> RouteT Route ContextM
-- renderComment c = do
  
submissionDetails s = do
  posted <- whenPosted s
  url <- showURL $ R_Post (submissionId s)
  uxg $
    <div class="submissionDetails">
      <% posted %> |
      <a href=url> <%comments (submissionVotes s)%> </a>
    </div>
  where
    comments 0 = "discuss"
    comments 1 = "1 comment"
    comments n = show n ++ " comment"

commentDetails c sM = do
  posted <- whenPosted c
  linkURL <- showURL $ R_Post (commentId c)
  uxg $
    <div class="commentDetails">
      <% posted %> |
      <a href=linkURL>link</a> |
      <%
        case sM of
          Nothing -> []
          Just s  -> [do
            parentURL <- showURL $ R_Post (commentParent c)
            submissionURL <- showURL $ R_Post (submissionId s)
            <%>
              <a href=parentURL>parent</a> |
              on: <a href=submissionURL><% submissionTitle s %></a>
              </%>]
      %>
    </div>

            
{-            
  maybe (noTop posted) (top posted) sM
  where
    
    top posted s = do
      linkURL <- showURL $ R_Post (commentId c)
      parentURL <- showURL $ R_Post (commentParent c)
      submissionURL <- showURL $ R_Post (submissionId s)
      uxg $
        <div class="commentDetails">
          <% posted %> |
          <a href=linkURL>link</a> |
          <a href=parentURL>parent</a> |
          on: <a href=submissionURL><% submissionTitle s %></a>
        </div>

    noTop posted = do
      linkURL <- showURL $ R_Post (commentId c)
      uxg $
        <div class="commentDetails">
          <% posted %> |
          <a href=linkURL> link </a> |
        </div>
-}

postPage r (Left p) = render $ layout r =<< do
  details <- submissionDetails p
  let title = submissionTitle p
  return (title, Just title, details)
postPage r (Right p) = do
  sM <- query $ getSubmission (commentSubmission p)
  case sM of
    Nothing -> e500
    Just s  -> render $ layout r =<< do
      details <- commentDetails p (Just s)
      return (tt "blah", Nothing, details)
