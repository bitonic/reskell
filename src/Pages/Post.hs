{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Post (
    postPage
  ) where


import Data.Time.Clock
import Data.Text               (Text)
import qualified Data.Text as T

import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Web.Routes

import Types
import Routes.Types
import DB
import Pages.Common


showTimeDiff :: UTCTime -> UTCTime -> String
showTimeDiff t1 t2 | diff < min'  = " just now"
                   | diff < hour  = plural "minute" $ diff /// min'
                   | diff < day   = plural "hour" $   diff /// hour
                   | diff < month = plural "day" $    diff /// day
                   | diff < year  = plural "month" $  diff /// month
                   | otherwise    = plural "year" $   diff /// year
                   
  where
    min' = 60
    hour = min' * 60
    day = hour * 24
    month = day * 30
    year = day * 365
    
    (///) :: RealFrac a => a -> a -> Int
    (///) = (truncate .) . (/)
    
    diff = diffUTCTime t1 t2
    
    plural t n = " " ++ show n ++ " " ++ t ++ s ++ "ago"
      where s | n > 1 = "s "
              | otherwise = " "    

whenPosted :: (Post a, MonadContext m) => a -> m Text
whenPosted p = do
  now <- askContext currTime
  return $ T.concat [tt (show $ pVotes p), tt " points by ",
                     (pUserName p), tt (showTimeDiff now $ pTime p)]

{-
renderComments :: Post a => a -> RouteT Route ContextM XML
renderComments p = do
  comments <- query $ getComments p
  let showComm
-}

-- renderComment :: Comment -> RouteT Route ContextM
-- renderComment c = do

  
submissionDetails :: Submission -> MonadTemplate
submissionDetails s = do
  posted <- whenPosted s
  <div class="submissionDetails">
    <% posted %> | <a href=(R_Post (sId s))> 
    <% comments 0 %> </a>
    </div>
  where
    comments 0 = "discuss"
    comments 1 = "1 comment"
    comments n = show n ++ " comment"

commentDetails :: Comment -> Maybe Submission -> MonadTemplate
commentDetails c sM = do
  posted <- whenPosted c
  <div class="commentDetails">
    <% posted %> |
    <a href=(R_Post (cId c))>link</a>
    <%
      case sM of
        Nothing -> []
        Just s  -> [do
          submissionURL <- showURL $ R_Post (sId s)
          if sId s == cParent c
            then <%> | on: <a href=submissionURL>
                     <% sTitle s %></a> </%>
            else do
              <%> | <a href=(R_Post (cParent c))>parent</a> |
                   on: <a href=submissionURL><% sTitle s %></a>
                  </%>]
    %>
    </div>


truncateText :: Text -> Int -> Text
truncateText t n | T.length t < n = t 
                 | otherwise      = T.concat [T.take n t, tt "..."]

postPage :: Route -> Either Submission Comment -> MonadPage Response
postPage r (Left p) = render $
                      template r (title, Just titleLink, submissionDetails p)
  where
    title = sTitle p
    titleLink = case sContent p of
      Ask _  -> <a href=(R_Post (sId p))> <% title %> </a>
      Link l -> <a href=l> <% title %> </a>
postPage r (Right p) = do
  sM <- query $ getSubmission (cSubmission p)
  case sM of
    Nothing -> e500
    Just s  -> render $ template r $
               (truncateText (cText p) 200, Nothing, commentDetails p (Just s))
