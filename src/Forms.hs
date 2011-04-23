{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Forms (
    loginForm
  , submitForm
  , commentForm
  , registerForm
  ) where


import Control.Monad           (liftM)
import Control.Applicative     ((<$>), (<*>))

import Data.Monoid             (mconcat)
import Data.Maybe              (isJust, isNothing)
import qualified Data.ByteString.Char8 as B8

import Text.Digestive.Types
import Text.Digestive.Validate
import Text.Digestive.HSP.Html4

import Happstack.Server        (Input)


import Types
import DB





type AppForm = Form PageM Input [Char] [TemplateM]

loginForm :: AppForm (UserName, Password)
loginForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,)
           <$> label "Username: " ++> inputString Nothing
           <*> label "Password: " ++> (B8.pack <$> inputPassword)
    
    validator = checkM "Incorrect username/password" $ \(userName, password) ->
      liftM isJust (query $ checkLogin userName password)


registerForm :: AppForm (UserName, Password, Password)
registerForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,,)
           <$> label "Username: " ++> inputString Nothing
           <*> label "Password: " ++> (B8.pack <$> inputPassword)
           <*> label "Confirm password: " ++> (B8.pack <$> inputPassword)
    
    validator = mconcat
                [ checkM "Username already exist" $ \(userName, _, _) ->
                   liftM isNothing (query $ getUser userName)
                , checkM "Password too short (min 5 characters)" $ \(_, password, _) ->
                   return (B8.length password > 5)
                , checkM "Password too long (max 50 chars)" $ \(_, password, _) ->
                   return (B8.length password < 50)
                , checkM "Username too long (max 100 chars)" $ \(userName, _, _) ->
                   return (length userName < 100)
                , checkM "The two passwords don't match" $ \(_, p1, p2) ->
                   return $ p1 == p2
                ]



submitForm :: AppForm (String, String, String)
submitForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,,)
           <$> label "Title: " ++> inputString Nothing
           <*> label "Url: " ++> inputString (Just linkBase)
           <*> label "or message: " ++> inputTextArea (Just 13) (Just 70) Nothing
    
    linkBase = "http://"
    validLink l = isJust $ getDomain l
    
    validator = mconcat
                [ check "Missing title" $ \(t, _, _) -> length t /= 0
                , check "Insert an url or a message (not both)" $ \(_, l, m) ->
                   validLink l /= not (null m)
                , check "Invalid url" $ \(_, l, _) -> case getDomain l of
                     Nothing -> l == linkBase || null l
                     Just _  -> True
                ]

commentForm :: AppForm String
commentForm = childErrors ++> inputTextArea (Just 8) (Just 70) Nothing


