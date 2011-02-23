{-# LANGUAGE OverloadedStrings #-}

module Forms (
  RegisterData(..), registerForm,
  LoginData(..), loginForm,
  UserCPData(..), userCPForm
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

data RegisterData = RegisterData Username Password Password

registerValidator :: Monad m
                     => UsersMap -> Validator m Html RegisterData
registerValidator users = mconcat
  [ check "Username missing" $ \(RegisterData u _ _) -> B.length u /= 0
  , check "Username already taken" $ \(RegisterData u _ _) ->
     not $ u `M.member` users
  , check "The passwords don't match" $ \(RegisterData _ p1 p2) -> p1 == p2
  , check "The password must be longer than 5 characters" $ \(RegisterData _ p _) ->
     B.length p > 4
  ]

registerForm :: (Monad m, Functor m)
                => UsersMap -> HappstackForm m Html BlazeFormHtml RegisterData
registerForm users = (`validate` registerValidator users) $
                     RegisterData
                     <$> label "Username: " ++> (B.pack <$> inputText Nothing)
                     <*> label "Password: " ++> (B.pack <$> inputPassword)
                     <*> label "Verify password: " ++> (B.pack <$> inputPassword)
      
      

data LoginData = LoginData Username Password

loginForm :: (Monad m, Functor m)
             => UsersMap -> HappstackForm m Html BlazeFormHtml LoginData
loginForm users = (`validate` validator) $ LoginData
                  <$> label "Username: " ++> (B.pack <$> inputText Nothing)
                  <*> label "Password: " ++> (B.pack <$> inputPassword)
  where
    validator = check "Incorrect username/password" $ \(LoginData u p) ->
      fromMaybe False $ fmap ((verifyPassword p) . userPassword) $ M.lookup u users



data UserCPData = UserCPData Password Password Password

userCPValidator user users = mconcat
  [ check "New password missing" $ \(UserCPData _ p _) -> B.length p /= 0
  , check "Wrong old password" $ \(UserCPData p _ _) ->
     fromMaybe False $ fmap ((verifyPassword p) . userPassword) $ M.lookup user users
  , check "The new passwords don't match" $ \(UserCPData _ p1 p2) -> p1 == p2
  ]

userCPForm :: (Monad m, Functor m)
              => Username -> UsersMap -> HappstackForm m Html BlazeFormHtml UserCPData
userCPForm user users = (`validate` userCPValidator user users) $ UserCPData
                        <$> label "Old password: " ++> (B.pack <$> inputPassword)
                        <*> label "New password: " ++> (B.pack <$> inputPassword)
                        <*> label "Verify new password: " ++> (B.pack <$> inputPassword)