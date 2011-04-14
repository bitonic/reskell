
module Forms (
    loginForm
  ) where


import Control.Monad           (liftM)
import Control.Applicative     ((<$>), (<*>))

import Data.Maybe              (isJust)
import qualified Data.ByteString.Char8 as B8

import Text.Digestive.Types
import Text.Digestive.Forms.Happstack ()
import Text.Digestive.Validate
import Text.Digestive.HSP.Html4

import Happstack.Server        (Input)

import Types
import DB
import Pages.Common



import Control.Monad.Trans (liftIO)


type AppForm = Form PageM Input [Char] [TemplateM]

loginForm :: AppForm (UserName, Password)
loginForm = childErrors ++> form
  where
    form = (`validate` validator) $ (,)
           <$> label "Username: " ++> inputString Nothing
           <*> label "Password: " ++> (B8.pack <$> inputPassword)
    
    validator = checkM "Incorrect username/password" $ \(userName, password) ->
      liftM isJust (query $ checkLogin userName password)