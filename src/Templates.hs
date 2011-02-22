{-# LANGUAGE OverloadedStrings #-}

module Templates (
  baseTemplate,
  registerTemplate, registerSuccessTemplate,
  loginTemplate,
  formTemplate
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze (text, stringValue, unsafeByteString)
import Text.Blaze.Internal (HtmlM)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Forms.Html (FormHtml, renderFormHtml)

baseTemplate :: Text -> H.Html -> H.Html
baseTemplate title body user = do
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
        menuSeparator
        text "loal"
        H.div ! A.id "userMenu" $ do
          text "lol"
          H.img ! A.src "/images/s.gif" ! A.alt "Spacing, sorry"
      H.div ! A.id "container" $ body
  where
    menuSeparator = unsafeByteString " &middot; "

registerTemplate :: H.Html -> H.Html
registerTemplate form = baseTemplate "Register" $ do
  H.h2 $ text "Create an account"
  form

registerSuccessTemplate :: H.Html
registerSuccessTemplate = baseTemplate "Register" $
                          text "Your registration was successful."
                          
loginTemplate :: H.Html -> H.Html
loginTemplate form = baseTemplate "Login" $ do
  H.h2 $ text "Login"
  form

formTemplate :: FormHtml (HtmlM a) -> String -> String -> H.Html
formTemplate form action submit = do
  let (formHtml, enctype) = renderFormHtml form
  H.form ! A.enctype (H.stringValue $ show enctype)
         ! A.method "POST" ! A.action (stringValue action) $ do
    formHtml
    H.input ! A.type_ "submit" ! A.value (stringValue submit)