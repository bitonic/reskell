{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Post (
    postPage
  ) where


import Data.Time.Clock

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

whenPosted :: (Post a, MonadContext m) => a -> m String
whenPosted p = do
  now <- askContext currTime
  return $ (show $ pVotes p) ++ " points by " ++
           (pUserName p) ++ (showTimeDiff now $ pTime p)


renderComment :: Comment -> TemplateM
renderComment comment = do
  comments <- query $ getComments comment
  <div class="comment">
    <% commentDetails comment Nothing %>
    <div class="postText">
      <% cText comment %>
    </div>

    <% if null comments
       then []
       else [<div class="comments">
               <% map renderComment comments %>
               </div>]
       %>
    </div>

renderComments :: [Comment] -> [TemplateM]
renderComments = map renderComment

submissionDetails :: Submission -> TemplateM
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

commentDetails :: Comment -> Maybe Submission -> TemplateM
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

truncateText :: String -> Int -> String
truncateText t n | length t < n = t 
                 | otherwise    = take n t ++ "..."

postPage :: Route -> Either Submission Comment -> PageM Response
postPage r (Left p) = do
  comments <- query $ getComments p
  render $ template r (title, Just titleLink, content comments)
  where
    title = sTitle p

    titleLink = case sContent p of
      Ask _    -> [<a href=(R_Post (sId p))> <% title %> </a>]
      Link l d -> [ <a href=l><% title %></a>
                 , <span class="linkDomain"> (<% d %>)</span>
                 ]
    
    content comments = submissionDetails p : text ++ form ++ (renderComments comments)

    text = case sContent p of
      Ask t -> [<div class="postText"><% t %></div>]
      _     -> []

    form = []

        
postPage r (Right p) = do
  sM <- query $ getSubmission (cSubmission p)
  s <- maybe (serverError "Could not find comment's submission in the db.") return sM
  comments <- query $ getComments p
  render $ template r $
    (truncateText (cText p) 200, Nothing,
     commentDetails p (Just s) : (renderComments comments))
