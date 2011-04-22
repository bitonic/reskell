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
  userM <- askContext sessionUser
  case userM of
    Just _ -> render $ template ("Login", Just [<h2>You are already logged in</h2>], [])
    Nothing -> do
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

dispatch R_Logout = expireSession >> redirectPageReferer

dispatch (R_Vote id' up) = do
  post <- query (getPost id') >>= maybe notFoundError return
  checkUser (checkIfVoter post) $ \user -> query (votePost post up user) >>
                                           redirectPageReferer
  where
    checkIfVoter (Left s) user  = not (uName user `elem` sVoters s)
    checkIfVoter (Right c) user = not (uName user `elem` cVoters c)

dispatch (R_Submissions submissions psort page) =
  submissionsPage submissions psort page


dispatch R_Register = do
  user <- askContext sessionUser
  case user of
    Just _ -> render $ template ("Login", Just [<h2>You are already registered</h2>], [])
    Nothing -> do
      resp' <- eitherHappstackForm registerForm "registerForm"
      case resp' of
        Left form -> registerPage form
        Right (userName, password) -> do
          query $ newUser userName password Member ""
          makeSession userName
          redirectPage
          
  
dispatch _ = render $ template ("", Nothing, [<h2> not yet implemented </h2>])
