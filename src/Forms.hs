{-# LANGUAGE OverloadedStrings #-}

module Forms (
  UserData(..), registerForm, loginForm,
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as B

import qualified Data.HashMap as M

import Crypto.PasswordStore (verifyPassword)

import Text.Digestive.Types
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Text.Digestive.Validate

import Text.Blaze.Html5 (Html)

import State

data UserData = UserData Username Password

registerForm :: (Monad m, Functor m)
                => UsersMap -> HappstackForm m Html BlazeFormHtml UserData
registerForm users = (`validateMany` [vUsername, vPassword]) $
                     UserData
                     <$> label "Username: " ++> (B.pack <$> inputText Nothing)
                     <*> label "Password: " ++> (B.pack <$> inputPassword)
  where
    vUsername =
      mconcat [ check "Username missing" $ \(UserData u _) -> B.length u /= 0
              , check "Username already taken" $ \(UserData u _) ->
                 not $ u `M.member` users
              ]
    vPassword =
      check "The password must be longer than 5 characters" $ \(UserData _ p) ->
        B.length p > 4

loginForm :: (Monad m, Functor m)
             => UsersMap -> HappstackForm m Html BlazeFormHtml UserData
loginForm users = (`validate` vUser) $ UserData
                  <$> label "Username: " ++> (B.pack <$> inputText Nothing)
                  <*> label "Password: " ++> (B.pack <$> inputPassword)
  where
    vUser = check "Incorrect username/password" $ \(UserData u p) ->
      fromMaybe False $ fmap ((verifyPassword p) . userPassword) $ M.lookup u users