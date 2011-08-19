module Forms
       ( AppForm
       , loginForm
       , submitForm
       , commentForm
       , registerForm
       , cpForm
       ) where


import Control.Monad           (liftM)
import Control.Applicative     ((<$>), (<*>))

import Data.Monoid             (mconcat)
import Data.Maybe              (isJust, isNothing)
import qualified Data.ByteString.Char8 as B8

import Text.Digestive.Types
import Text.Digestive.Validate
import Text.Digestive.HSP.Html4 hiding (form)

import Happstack.Server        (Input)

import Crypto.PasswordStore    (verifyPassword)

import Types


type AppForm = Form PageM [Input] [Char] [TemplateM]

{-|

Login form. Checks that the 'UserName' and 'Password' match, and
returns them.

-}
loginForm :: AppForm (UserName, Password)
loginForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,)
           <$> label "Username: " ++> inputString Nothing
           <*> label "Password: " ++> (fmap B8.pack inputPassword)
    
    validator = checkM "Incorrect username/password" $ \(userName, password) -> do
      userM <- userQuery $ GetUser userName
      return $ case userM of
        Nothing -> False
        Just user -> verifyPassword password (uPassword user)


{-|

Form to register a new user. Returns the username and two passwords
(the second is the verification password).

Checks that:

    * The username is not present in the DB already

    * The password length is > 5 && < 50

    * The username length is > 0 && < 100

    * The password and the verification password match.

-}
registerForm :: AppForm (UserName, Password, Password)
registerForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,,)
           <$> label "Username: " ++> inputString Nothing
           <*> label "Password: " ++> (B8.pack <$> inputPassword)
           <*> label "Confirm password: " ++> (B8.pack <$> inputPassword)
    
    validator = mconcat
                [ checkM "Username already exist" $ \(userName, _, _) ->
                   liftM isNothing (userQuery $ GetUser userName)
                , checkM "Password too short (min 5 characters)" $ \(_, password, _) ->
                   return (B8.length password > 5)
                , checkM "Password too long (max 50 chars)" $ \(_, password, _) ->
                   return (B8.length password < 50)
                , checkM "Username too long (max 100 chars)" $ \(userName, _, _) ->
                   return (length userName < 100)
                , checkM "Username required." $ \(userName, _, _) ->
                   return $ not (null userName)
                , checkM "The two passwords don't match" $ \(_, p1, p2) ->
                   return $ p1 == p2
                ]


{-|

Form used when submitting a 'Submission'.

Checks that:

    * The title is present

    * Either a valid link (see 'getDomain') or a message are present.

-}
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


{-|

Form for the user control panel.

Checks that, if a new password is input, the old one is correct.

-}
cpForm :: User -> AppForm (String, Password, Password, Password)
cpForm user = childErrors ++> form
  where
    form = (`validate` validator) $ (,,,)
           <$> label "About: " ++> inputTextArea (Just 15) (Just 70) (Just $ uAbout user)
           <*> label "Old password" ++> (B8.pack <$> inputPassword)
           <*> label "New password: " ++> (B8.pack <$> inputPassword)
           <*> label "Confirm new password: " ++> (B8.pack <$> inputPassword)
    
    validator = mconcat
                [ check "You have to insert the old password to edit the new one" $
                  \(_, old, new, _) -> B8.length old /= 0 || B8.length new == 0
                , check "Wrong old password" $ \(_, old, _, _) ->
                  B8.length old == 0 || verifyPassword old (uPassword user)
                , check "The two passwords don't match" $ \(_, _, new1, new2) ->
                  (B8.length new1 == 0 && B8.length new2 == 0) || new1 == new2
                ]