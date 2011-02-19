{-# LANGUAGE OverloadedStrings #-}

module Templates (
  baseTemplate,
  registerTemplate, registerSuccessTemplate,
  loginTemplate,
  formTemplate
  ) where

import Data.Text (Text)
import Data.Text as T

import Text.Blaze (text, stringValue)
import Text.Blaze.Internal (HtmlM)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Forms.Html (FormHtml, renderFormHtml)

import Happstack.State (Query)

import State

baseTemplate :: Text -> H.Html -> H.Html
baseTemplate title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ text $ T.concat ["Hs News - ", title]
      H.link ! A.rel "stylesheet" ! A.type_ "text" ! A.src "/style.css"
    H.body body

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