{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages
       ( dispatch
       ) where


import Data.Time.Clock         (getCurrentTime)
  
import HSP

import Happstack.Server

import Text.Digestive.Forms.Happstack

import Web.Routes.Happstack

import Crypto.PasswordStore

import Types
import Pages.Common
import Pages.Post
import Pages.User
import Forms
import Auth




dispatch :: Route -> PageM Response

dispatch r@(R_Post id' psort) = do
  post <- postQuery (GetPost id') >>= \postM -> case postM of
    Nothing -> notFoundError
    Just p  -> return p
  resp' <- eitherHappstackForm commentForm "commentForm"
  case resp' of
    Left form -> do
      userM <- askContext sessionUser
      case userM of
        Nothing -> postPage [] post psort
        Just _  -> postPage [renderForm form "Comment"] post psort
    Right comment -> checkUser anyUser $ \user -> do
      case post of
        Left s -> dbTime postUpdate $ NewComment (uName user) comment (sId s) (sId s)
        Right c -> dbTime postUpdate $ NewComment (uName user) comment (cSubmission c) (cId c)
      seeOtherURL r

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
        submission <- dbTime postUpdate $ NewSubmission (uName user) title content
        seeOtherURL $ R_Post (sId submission) Top

dispatch R_Logout = expireSession >> redirectPageReferer

dispatch (R_Vote id' up) =
  checkUser anyUser $ \user -> postUpdate (VotePost id' up user) >> redirectPageReferer

dispatch (R_Submissions submissions psort page userM) =
  submissionsPage submissions psort page userM


dispatch R_Register = do
  user <- askContext sessionUser
  case user of
    Just _ -> render $ template ("Login", Just [<h2>You are already registered</h2>], [])
    Nothing -> do
      resp' <- eitherHappstackForm registerForm "registerForm"
      case resp' of
        Left form -> registerPage form
        Right (userName, password, _) -> do
          hashedPassword <- liftIO $ makePassword password hashStrength
          time <- liftIO getCurrentTime
          let user' = User { uName = userName
                           , uPassword = hashedPassword
                           , uRank = Member
                           , uAbout = ""
                           , uCreated = time
                           }
          userUpdate $ NewUser user'
          makeSession userName
          redirectPage

dispatch R_CP =
  checkUser anyUser $ \user -> do
    resp' <- eitherHappstackForm (cpForm user) "cpForm"
    case resp' of
      Left form -> cpPage form
      Right (about, _, new, _) -> do
        let user' = user {uAbout = about, uPassword = new}
        userUpdate $ UpdateUser user'
        seeOtherURL R_CP


dispatch (R_User userName) =
  userQuery (GetUser userName) >>= maybe notFoundError userPage
          
  
dispatch _ = render $ template ("", Nothing, [<h2> not yet implemented </h2>])
