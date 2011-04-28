{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Post (
    postPage
  , submitPage
  , submissionsPage
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


whenPosted :: Post a => a -> GenChildList PageM
whenPosted p =
  <%>
    posted by <a href=(R_User $ pUserName p)><% pUserName p %></a>
    <% do now <- liftIO getCurrentTime
          <%><% showTimeDiff now $ pTime p %></%>
    %>
  </%>

renderComment :: Comment -> PostSort -> TemplateM
renderComment comment psort = do
  { comments <- query $ getComments comment psort
  ; <div class="comment">
      <% voteArrows comment %>
      <% commentDetails comment Nothing psort %>
      <div class="postText">
        <% cText comment %>
      </div>

      <% if null comments
         then []
         else [<% renderComments comments psort %>]
      %>
    </div>
  }

renderComments :: [Comment] -> PostSort -> TemplateM
renderComments cs psort =
  <div class="comments"><% map (`renderComment` psort) cs %></div >

submissionDetails :: Submission -> Bool -> TemplateM
submissionDetails s listing = do
  { commentsN <- query $ countComments s
  ; <div class="submissionDetails">
      <% whenPosted s %>
      <% if listing
         then [ <span><% separator %></span>
              , <a href=(R_Post (sId s) Top)><% comments commentsN %></a>
              ]
         else []
      %>
    </div>
  }
  where
    comments 0 = "discuss"
    comments 1 = "1 comment"
    comments n = show n ++ " comment"

commentDetails :: Comment -> Maybe Submission -> PostSort -> TemplateM
commentDetails c sM psort =
  <div class="commentDetails">
    <% whenPosted c %><% separator %>
    <a href=(R_Post (cId c) Top)>link</a>
    <% case sM of
         Nothing -> []
         Just s  -> return $ do
           submissionURL <- showURL $ R_Post (sId s) psort
           if sId s == cParent c
             then <%>
                    <% separator %>on: <a href=submissionURL>
                    <% sTitle s %></a>
                  </%>
             else <%>
                    <% separator %><a href=(R_Post (cParent c) Top)>parent</a>
                    <% separator %>on: <a href=submissionURL><% sTitle s %></a>
                  </%>
    %>
  </div>

truncateText :: String -> Int -> String
truncateText t n | length t < n = t 
                 | otherwise    = take n t ++ "..."

voteArrows :: Post a => a -> TemplateM
voteArrows p = do
  { userM <- askContext sessionUser
  ; <div class="voteArrows">
      <% if maybe False (\u -> uName u `elem` pVoters p) userM
         then []
         else [ <a href=(R_Vote (pId p) True)>
                  <img src=(R_Static ["images", "arrowUp.png"]) alt="Vote up" />
                </a>
              , <a href=(R_Vote (pId p) False)>
                  <img src=(R_Static ["images", "arrowDown.png"]) alt="Vote down" />
                </a>
              ]
      %>
    </div>
  }

submissionLink :: Submission -> Bool -> TemplateM
submissionLink s listing =
  <div class="submissionLink">
    <% voteArrows s %>
    <h2> <% case sContent s of
              Ask _    -> [<a href=(R_Post (sId s) Top)><% sTitle s %></a>]
              Link l d -> [ <a href=l><% sTitle s %></a>
                         , <span class="linkDomain"> (<% d %>)</span>
                         ]
         %>
    </h2>
    <% submissionDetails s listing %>
  </div>
      

postPage :: [TemplateM] -> Either Submission Comment -> PostSort -> PageM Response
postPage form (Left p) psort = do
  comments <- query $ getComments p psort
  render $ template (sTitle p, Nothing, content comments)
  where
    content comments = submissionLink p False : text ++
                       form ++ [renderComments comments psort]

    text = case sContent p of
      Ask t -> [<div class="postText"><% t %></div>]
      _     -> []


postPage form (Right p) psort = do
  sM <- query $ getSubmission (cSubmission p)
  s <- maybe (serverError "Could not find comment's submission in the db.") return sM
  comments <- query $ getComments p psort
  render $ template $
    ( truncateText (cText p) 200
    , Nothing
    , commentDetails p (Just s) psort : <div class="postText"><% cText p %></div> :
       form ++ [renderComments comments psort]
    )



submitPage :: [TemplateM] -> PageM Response
submitPage form = render $ template $
                  ( "Submit"
                  , Just [<span>Submit</span>]
                  , [ renderForm form "Submit"
                    ]
                  )


submissionsPage :: Submissions
                   -> PostSort
                   -> PageNumber
                   -> Maybe UserName
                   -> PageM Response
submissionsPage submissions psort page userM = do
  posts <- query $ getSubmissions submissions psort postsPerPage (postsPerPage * page) userM
  render $ template $
    ( show submissions ++ " - " ++ show psort
    , Nothing
    , map (`submissionLink` True ) posts
    )