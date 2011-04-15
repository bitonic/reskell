{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages ( 
    dispatch
  ) where


import HSP
import HSP.ServerPartT         ()

import Happstack.Server
import Happstack.Server.HSP.HTML ()

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

dispatch r@(R_Post id') = query (getPost id') >>= maybe notFoundError (postPage r)

dispatch r@(R_Login redir) = do
  resp <- eitherHappstackForm loginForm "loginForm"
  case resp of
    Left form -> loginPage r form
    Right (userName, _) -> do
      makeSession userName
      seeOtherURL redir

dispatch r@R_Submit =
  checkUser r anyUser $ \user -> do
    resp <- eitherHappstackForm submitForm "submitForm"
    case resp of
      Left form -> submitPage r form
      Right (title, link, ask) -> do
        content <- if null ask
                   then case getDomain link of
                     Nothing -> serverError "Received invalid url from the submit form."
                     Just d  -> return $ Link link d
                   else return $ Ask ask
        submission <- query $ newSubmission (uName user) title content
        seeOtherURL $ R_Post (sId submission)

dispatch r@(R_Logout redir) = expireSession >> seeOtherURL redir

dispatch r = render $ template r ("", Nothing, [<h2> not yet implemented </h2>])