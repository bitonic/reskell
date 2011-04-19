{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Post (
    postPage
  , submitPage
  ) where


import Control.Monad.Trans     (liftIO)
import Data.Time.Clock

import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Web.Routes

import Types
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

whenPosted p =
  <%>
    <% show (pVotes p) %> points by 
    <a href=(R_User $ pUserName p)><% pUserName p %></a>
    <% do now <- liftIO getCurrentTime
          <%><% showTimeDiff now $ pTime p %></%>
    %>
  </%>

renderComment :: Comment -> TemplateM
renderComment comment = do
  { comments <- query $ getComments comment
  ; <div class="comment">
      <% commentDetails comment Nothing %>
      <div class="postText">
        <% cText comment %>
      </div>

      <% if null comments
         then []
         else [<% renderComments comments %>]
      %>
    </div>
  }

renderComments :: [Comment] -> TemplateM
renderComments cs = <div class="comments"><% map renderComment cs %></div >

submissionDetails :: Submission -> Bool -> TemplateM
submissionDetails s listing = do
  { commentsN <- query $ countComments s
  ; <div class="submissionDetails">
      <% whenPosted s %>
      <% if listing
         then [separator, <a href=(R_Post (sId s))><% comments commentsN %></a>]
         else []
      %>
    </div>
  }
  where
    comments 0 = "discuss"
    comments 1 = "1 comment"
    comments n = show n ++ " comment"

commentDetails :: Comment -> Maybe Submission -> TemplateM
commentDetails c sM =
  <div class="commentDetails">
    <% whenPosted c %><% separator %>
    <a href=(R_Post (cId c))>link</a>
    <% case sM of
         Nothing -> []
         Just s  -> return $ do
           submissionURL <- showURL $ R_Post (sId s)
           if sId s == cParent c
             then <%>
                    <% separator %>on: <a href=submissionURL>
                    <% sTitle s %></a>
                  </%>
             else <%>
                    <% separator %><a href=(R_Post (cParent c))>parent</a>
                    <% separator %>on: <a href=submissionURL><% sTitle s %></a>
                  </%>
    %>
  </div>

truncateText :: String -> Int -> String
truncateText t n | length t < n = t 
                 | otherwise    = take n t ++ "..."

postPage :: Route -> [TemplateM] -> Either Submission Comment -> PageM Response
postPage r form (Left p) = do
  comments <- query $ getComments p
  render $ template r (title, Just titleLink, content comments)
  where
    title = sTitle p

    titleLink = case sContent p of
      Ask _    -> [<a href=(R_Post (sId p))> <% title %> </a>]
      Link l d -> [ <a href=l><% title %></a>
                 , <span class="linkDomain"> (<% d %>)</span>
                 ]
    
    content comments = submissionDetails p False : text ++
                       form ++ [renderComments comments]

    text = case sContent p of
      Ask t -> [<div class="postText"><% t %></div>]
      _     -> []

        
postPage r form (Right p) = do
  sM <- query $ getSubmission (cSubmission p)
  s <- maybe (serverError "Could not find comment's submission in the db.") return sM
  comments <- query $ getComments p
  render $ template r $
    ( truncateText (cText p) 200
    , Nothing
    , commentDetails p (Just s) : <div class="postText"><% cText p %></div> :
       form ++ [renderComments comments]
    )



submitPage :: Route -> [TemplateM] -> PageM Response
submitPage r form = render $ template r $
                    ( "Submit"
                    , Just [<span>Submit</span>]
                    , [ renderForm form r "Submit"
                      ]
                    )
