{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages.Post
       ( postPage
       , submitPage
       , submissionsPage
       ) where


import Control.Monad.Trans     (liftIO)

import qualified Data.Set as S
import Data.Time.Clock

import HSP
import HSP.ServerPartT ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

import Web.Routes

import Types
import Pages.Common
import Auth




-- | Function that shows a message like "4 minutes ago"
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


-- Displays the message "posted by xxx 6 minutes ago"
whenPosted :: Post a => a -> GenChildList PageM
whenPosted p =
  <%>
    posted by <a href=(R_User $ pUserName p)><% pUserName p %></a>
    <% do now <- liftIO getCurrentTime
          <%><% showTimeDiff now $ pTime p %></%>
    %>
  </%>


-- Renders a comment
renderComment :: Comment -> PostSort -> TemplateM
renderComment comment psort = do
  { comments <- postQuery $ GetComments psort (Just $ cId comment) Nothing
  ; <div class="comment" name=(cId comment)>
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


-- deleteLink :: Post a => a -> [TemplateM]
deleteLink post = do
  { userM <- askContext sessionUser
  ; <%>
      <% case userM of
           Nothing   -> []
           Just user -> if deletePost user
                        then [ <span><% separator %></span>
                             , <a href=(R_Delete (pId post))>x</a>
                             ]
                        else []
      %>
    </%>
  }


submissionDetails :: Submission -> Bool -> TemplateM
submissionDetails s listing = do
  { commentsN <- postQuery $ CountComments (sId s)
  ; <div class="submissionDetails">
      <% whenPosted s %>
      <% if listing
         then [ <span><% separator %></span>
              , <a href=(R_Post (sId s) Top)><% comments commentsN %></a>
              ]
         else []
      %>
      <% deleteLink s %>
    </div>
  }
  where
    comments 0 = "discuss"
    comments 1 = "1 comment"
    comments n = show n ++ " comments"


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
    <% deleteLink c %>
  </div>


truncateText :: String -> Int -> String
truncateText t n | length t < n = t 
                 | otherwise    = take n t ++ "..."


voteArrows :: Post a => a -> TemplateM
voteArrows p = do
  { userM <- askContext sessionUser
  ; let (arrowUp, arrowDown) = case userM of
          Nothing -> ("arrowUp.png", "arrowDown.png")
          Just user -> (if S.member (uName user) (pVotesUp p)
                       then "arrowUpRed.png"
                       else "arrowUp.png"
                      ,if S.member (uName user) (pVotesDown p)
                       then "arrowDownRed.png"
                       else "arrowDown.png")
  ; <div class="voteArrows">
         <a href=(R_Vote (pId p) True)>
           <img src=(R_Static ["images", arrowUp]) alt="Vote up" />
         </a>
         <a href=(R_Vote (pId p) False)>
           <img src=(R_Static ["images", arrowDown]) alt="Vote down" />
         </a>
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
      

commentsSortLinks :: (PostSort -> Route) -> PostSort -> [TemplateM]
commentsSortLinks proute psort =
  <span>comments sorted by: </span> : links
  where
    links = case psort of
      New -> [ <a href=(proute Top) class="commentSort">top</a>
            , <span><% separator %></span>
            , <span>new</span>
            ]
      Top -> [ <span>top</span>
            , <span><% separator %></span>
            , <a href=(proute New) class="commentSort">new</a>
            ]


    
postPage :: [TemplateM] -> Either Submission Comment -> PostSort -> PageM Response
postPage form (Left p) psort = do
  comments <- postQuery $ GetComments psort (Just $ pId p) Nothing
  render $ template (sTitle p, Nothing, content comments)
  where
    content comments = submissionLink p False : text ++
                       form ++ commentsSortLinks (R_Post (pId p)) psort ++
                       [renderComments comments psort]

    text = case sContent p of
      Ask t -> [<div class="postText"><% markdownSubmission t %></div>]
      _     -> []
postPage form (Right p) psort = do
  sM <- postQuery $ GetSubmission (cSubmission p)
  s <- maybe (serverError "Could not find comment's submission in the db.") return sM
  comments <- postQuery $ GetComments psort (Just $ pId p) Nothing
  render $ template $
    ( truncateText (cText p) 200
    , Nothing
    , commentDetails p (Just s) psort :
       <div class="postText"><% markdownComment (cText p) %></div> :
       form ++ commentsSortLinks (R_Post (pId p)) psort ++
       [renderComments comments psort]
    )



submitPage :: [TemplateM] -> PageM Response
submitPage form = render $ template $
                  ( "Submit"
                  , Just [<span>Submit</span>]
                  , [ renderForm form "Submit"
                    ]
                  )


listingSortLinks :: (PostSort -> Route) -> PostSort -> TemplateM
listingSortLinks proute psort =
  <div id="listingSort">
    <% case psort of
         Top -> [ <span class="activeSort">top</span>
               , <span><% separator %></span>
               , <a href=(proute New)>new</a>
               ]
         New -> [ <a href=(proute Top)>top</a>
               , <span><% separator %></span>
               , <span class="activeSort">new</span>
               ]
    %>
  </div>

submissionsPage :: Submissions
                   -> PageNumber
                   -> Maybe UserName
                   -> PostSort
                   -> PageM Response
submissionsPage submissions page userM psort = do
  posts <- postQuery $ GetSubmissions submissions psort postsPerPage (postsPerPage * page) userM
  render $ template $
    ( show submissions ++ " - " ++ show psort
    , Nothing
    , listingSortLinks (R_Submissions submissions page userM) psort :
      map (`submissionLink` True ) posts
    )
