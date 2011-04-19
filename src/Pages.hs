{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages ( 
    dispatch
  ) where


import HSP

import Happstack.Server

import Text.Digestive.Forms.Happstack

import Web.Routes.Happstack

import Types
import DB
import Pages.Common
import Pages.Post
import Pages.User
import Forms
import Auth




dispatch :: Route -> PageM Response

dispatch r =
  render $ template r ("", Nothing, [<h2> not yet implemented </h2>])
  

dispatch r@(R_Post id') = do
  post <- query (getPost id') >>= \postM -> case postM of
    Nothing -> notFoundError
    Just p  -> return p
  resp' <- eitherHappstackForm commentForm "commentForm"
  case resp' of
    Left form -> do
      userM <- askContext sessionUser
      case userM of
        Nothing -> postPage r [] post
        Just _  -> postPage r [renderForm form r "Comment"] post
    Right comment -> checkUser r anyUser $ \user -> do
      case post of
        Left s -> query $ newComment (uName user) comment (sId s) s
        Right c -> query $ newComment (uName user) comment (cSubmission c) c
      seeOtherURL r

dispatch r@(R_Login redir) = do
  resp' <- eitherHappstackForm loginForm "loginForm"
  case resp' of
    Left form -> loginPage r form
    Right (userName, _) -> do
      makeSession userName
      seeOtherURL redir

dispatch r@R_Submit =
  checkUser r anyUser $ \user -> do
    resp' <- eitherHappstackForm submitForm "submitForm"
    case resp' of
      Left form -> submitPage r form
      Right (title, link, ask) -> do
        content <- if null ask
                   then case getDomain link of
                     Nothing -> serverError "Received invalid url from the submit form."
                     Just d  -> return $ Link link d
                   else return $ Ask ask
        submission <- query $ newSubmission (uName user) title content
        seeOtherURL $ R_Post (sId submission)

dispatch (R_Logout redir) = expireSession >> seeOtherURL redir

dispatch (R_Vote id' up redir) = do
  postM <- query $ getPost id'
  maybe notFoundError ((>> seeOtherURL redir) . query . (`votePost` up)) postM
  
dispatch r = render $ template r ("", Nothing, [<h2> not yet implemented </h2>])
