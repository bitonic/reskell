{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Pages
       ( dispatch
       ) where


import Control.Monad.Trans     (liftIO)

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




{-|
Landing function, that gets the 'Route' and returns the 'Response'.

It will call the other pages functions, and handle form data.
-}
dispatch :: Route -> PageM Response

dispatch r@(R_Post id' psort) = do
  -- 404 if we can't find the post
  post <- postQuery (GetPost id') >>= \postM -> case postM of
    Nothing -> notFoundError
    Just p  -> return p
  
  -- Process the form
  resp' <- eitherHappstackForm commentForm "commentForm"
  case resp' of
    Left form -> do
      -- Gets the user
      userM <- askContext sessionUser
      case userM of
        -- If there is no user, don't display the form
        Nothing -> postPage [] post psort
        Just _  -> postPage [renderForm form "Comment"] post psort
    
    -- Check that we have a user, and add the comment
    Right comment -> checkUser anyUser $ \user -> do
      case post of
        Left s -> dbTime postUpdate $ NewComment (uName user) comment (sId s) (sId s)
        Right c -> dbTime postUpdate $ NewComment (uName user) comment (cSubmission c) (cId c)
      seeOtherURL r

dispatch (R_Delete id') =
  checkUser deletePost $ \_ -> postUpdate (DeletePost id') >> redirectPageReferer

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
        -- Check that the url is valid. We check this in the form
        -- itself as well, but we want to be safe from other POST
        -- requests.
        content <- if null ask
                   then case getDomain link of
                     Nothing -> serverError "Received invalid url from the submit form."
                     Just d  -> return $ Link link d
                   else return $ Ask ask
        submission <- dbTime postUpdate $ NewSubmission (uName user) title content
        seeOtherURL $ R_Post (sId submission) Top

dispatch R_Logout = expireSession >> redirectPageReferer

dispatch (R_Vote id' up) = postQuery (GetPost id') >>= maybe notFoundError vote 
  where
    vote post = checkUser anyUser $ \user -> do
      postUpdate $ VotePost post up user
      let karma | up = 1
                | otherwise = -1
          pUser = either pUserName pUserName post
      if uName user /= pUser
        then userUpdate $ UpdateKarma pUser karma
        else return ()
      redirectPageReferer

dispatch (R_Submissions submissions page userM psort) =
  submissionsPage submissions page userM psort
  
dispatch R_Register = do
  user <- askContext sessionUser
  case user of
    Just _ -> render $ template ("Login", Just [<h2>You are already registered</h2>], [])
    Nothing -> do
      resp' <- eitherHappstackForm registerForm "registerForm"
      case resp' of
        Left form -> registerPage form
        Right (userName, password, _) -> do
          -- Hash the password
          hashedPassword <- liftIO $ makePassword password hashStrength
          time <- liftIO getCurrentTime
          let user' = User { uName = userName
                           , uPassword = hashedPassword
                           , uRank = Member
                           , uAbout = ""
                           , uCreated = time
                           , uKarma = 0
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
        hashedPassword <- liftIO $ makePassword new hashStrength
        let user' = user {uAbout = about, uPassword = hashedPassword}
        userUpdate $ UpdateUser user'
        seeOtherURL R_CP


dispatch (R_User userName) =
  userQuery (GetUser userName) >>= maybe notFoundError userPage
          

dispatch _ = render $ template ("", Nothing, [<h2> not yet implemented </h2>])
