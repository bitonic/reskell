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

dispatch (R_Post id') = do
  post <- query (getPost id') >>= \postM -> case postM of
    Nothing -> notFoundError
    Just p  -> return p
  resp' <- eitherHappstackForm commentForm "commentForm"
  case resp' of
    Left form -> do
      userM <- askContext sessionUser
      case userM of
        Nothing -> postPage [] post
        Just _  -> postPage [renderForm form "Comment"] post
    Right comment -> checkUser anyUser $ \user -> do
      case post of
        Left s -> query $ newComment (uName user) comment (sId s) s
        Right c -> query $ newComment (uName user) comment (cSubmission c) c
      seeOtherURL (R_Post id')

dispatch R_Login = do
  resp' <- eitherHappstackForm loginForm "loginForm"
  case resp' of
    Left form -> loginPage form
    Right (userName, _) -> do
      makeSession userName
      redirectPage

dispatch R_Submit =
  checkUser anyUser $ \user -> do
    resp' <- eitherHappstackForm submitForm "submitForm"
    case resp' of
      Left form -> submitPage form
      Right (title, link, ask) -> do
        content <- if null ask
                   then case getDomain link of
                     Nothing -> serverError "Received invalid url from the submit form."
                     Just d  -> return $ Link link d
                   else return $ Ask ask
        submission <- query $ newSubmission (uName user) title content
        seeOtherURL $ R_Post (sId submission)

dispatch R_Logout = expireSession >> redirectPage

dispatch (R_Vote id' up) =
  checkUser anyUser $ \user -> do
    postM <- query $ getPost id'
    maybe notFoundError ((>> redirectPage) . query . (\p -> votePost p up user)) postM

dispatch (R_Submissions submissions psort page) =
  submissionsPage submissions psort page
  
dispatch _ = render $ template ("", Nothing, [<h2> not yet implemented </h2>])
