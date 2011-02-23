{-# LANGUAGE OverloadedStrings #-}

module Templates (
  baseTemplate,
  
  indexTemplate,
  
  formHtml,
  
  registerTemplate, registerSuccessTemplate,
  loginTemplate,
  userCPTemplate,
  logoutTemplate,
  
  renderTemplate
  ) where

import Happstack.Server

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Text.Blaze (text, stringValue, unsafeByteString, toValue)
import Text.Blaze.Internal (HtmlM)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Forms.Html (FormHtml, renderFormHtml)

import State
import Utils



data TemplateContext = TemplateContext { tmplUser :: Maybe User
                                       , tmplUri :: String
                                       }
                       
type Template = Reader TemplateContext H.Html

baseTemplate :: Text -> H.Html -> Template
baseTemplate title body = liftM layout ask
  where
    layout ctx = do
      H.docType
      H.html $ do
        H.head $ do
          H.title $ text $ T.concat ["Haskell News - ", title]
          H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/style.css"
        H.body $ do
          H.div ! A.id "header" $ do
            H.img ! A.src "/images/haskellLogo.png"
                  ! A.alt "Haskell logo" ! A.id "logo"
            H.h1 $ H.a ! A.href "/" $ text "Haskell News"
            menuSeparator
            text "bla"
            H.div ! A.id "user-menu" $ do
              H.img ! A.src "/images/s.gif" ! A.alt "Spacing, sorry"
              case tmplUser ctx of
                Just user -> loggedIn
                Nothing   -> notLoggedIn ctx
          H.div ! A.id "container" $ body
    menuSeparator = unsafeByteString " &middot; "
    loggedIn = H.a ! A.href "/user/logout"  $ text "Logout"
    notLoggedIn ctx = do
      H.a ! A.href (toValue $ "/user/login?redir=" ++ tmplUri ctx) $ text "Login"
      menuSeparator
      H.a ! A.href "/user/register" $ text "Register"

indexTemplate :: Template
indexTemplate = baseTemplate "Home" $
                H.h2 $ text "Welcome to Haskell News."

formHtml :: FormHtml (HtmlM a) -> String -> String -> H.Html
formHtml form action submit = do
  let (formHtml, enctype) = renderFormHtml form
  H.form ! A.enctype (H.stringValue $ show enctype)
    ! A.method "POST" ! A.action (stringValue action) $ do
      formHtml
      H.input ! A.type_ "submit" ! A.value (stringValue submit)

registerTemplate :: H.Html -> Template
registerTemplate form  = baseTemplate "Register" $ do
  H.h2 $ text "Create an account"
  form

registerSuccessTemplate :: Template
registerSuccessTemplate = baseTemplate "Register" $
                          H.h2 $ text "Your registration was successful."
                          
loginTemplate :: H.Html -> Template
loginTemplate form = baseTemplate "Login" $ do
  H.h2 $ text "Login"
  form

userCPTemplate :: User -> H.Html -> Template
userCPTemplate user form = baseTemplate "User CP" $ do
  H.h2 $ text $ T.concat [E.decodeUtf8 $ userName user, " - Edit your details:"]
  form

logoutTemplate :: Template
logoutTemplate = baseTemplate "Logout" $ H.h2 $ text "You are now logged out."

renderTemplate :: Template -> ServerPart Response
renderTemplate templ = do
  user <- getUser
  uri <- getFullUri
  let html = runReader templ (TemplateContext user uri)
  ndResponse html