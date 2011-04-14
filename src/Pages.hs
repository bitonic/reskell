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

dispatch r = render $ template r ("", Nothing, [<h2> not yet implemented </h2>])