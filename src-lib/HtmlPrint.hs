{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module HtmlPrint where

import Foundation (($),(.),String,Maybe(Nothing,Just),toList,return,(<>),maybe)

import Text.Blaze.Html5 (ToValue(toValue),ToMarkup(toMarkup),(!),Html,string,stringValue,docTypeHtml,head,title,script,link,style,body,pre,code,br,span,toHtml,dataAttribute)
import qualified Text.Blaze.Html.Renderer.Text as TB
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.Lazy (Text, unlines)
import Data.Foldable (foldMap)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Sigma,Prod),SourcePos(SourcePos),AnnType(Inferred,UserGiven))

instance ToValue String where
    toValue str = stringValue $ toList str

instance ToMarkup String where
    toMarkup str = string $ toList str

renderHtml :: Term -> Text
renderHtml term = TB.renderHtml $ docTypeHtml $ do
     head $ do
         title "Alava"
         script ! A.type_ "text/javascript" ! A.src "../../tippy.min.js" $ return ()
         link ! A.rel "stylesheet" ! A.href "../../tippy.css"
         style ! A.type_ "text/css" $
            toHtml $ unlines [ ""
                ,"body { padding: 1em; }"
                ,".a-let, .a-sig, .a-def { display: block; }"
                ,".a-sig, .a-def { margin-left: 1em; }"
                ,".a-let > span { margin-left: 1em; }"

                ,".a-comment { color: #7f7f7f; }"
                ,".a-keyword { color: #BF772F; }"
                ,".a-name { color: #2FBFBF; }"
                ,".a-var { color: #5F7FBF; }"
                ,".a-def, .a-sig { color: #BCBC5E; }"
              ]
     body $
        pre $
            code $
                render term
     script ! A.type_ "text/javascript" $
        toHtml ("tippy('span[title]', {theme: 'menu light'})" :: String)

wrap :: String -> Html -> Html
wrap cls = span ! A.class_ (toValue $ "a-" <> cls)

wrap2 :: String -> Term -> Html -> Html
wrap2 cls etype = span ! A.class_ (toValue $ "a-" <> cls) ! A.title (toValue $ TB.renderHtml $ render etype)

txt :: String -> Html
txt = toHtml . toList

keyword :: String -> Html
keyword str = toHtml (toList str) ! A.class_ "a-keyword"

name :: String -> Html
name str = toHtml (toList str) ! A.class_ "a-name"

render :: Term -> Html
render Type                              = wrap "type" $ keyword "Type"
render (Var tname)                       = wrap "var" $ name tname
render (Lam tname Nothing b)                             = wrap  "lam"   $ keyword "\\" <> name tname <>                             keyword ". " <> render b
render (Lam tname (Just (Ann (Comment _) t Inferred)) b) = wrap2 "lam" t $ keyword "\\" <> name tname <>                             keyword ". " <> render b
render (Lam tname (Just t) b)                            = wrap  "lam"   $ keyword "\\" <> name tname <> keyword ": " <> render t <> keyword ". " <> render b
render (App f param)                     = wrap "app" $ render f <> txt " " <> render param
render (Pi Nothing ptype rtype)          = wrap "pi" $ render ptype <> keyword " -> " <> render rtype
render (Pi (Just tname) ptype rtype)     = wrap "pi" $ keyword "(" <> name tname <> keyword ": " <> render ptype <> keyword ") -> " <> render rtype

render (Ann term t UserGiven)            = wrap  "ann"   $ keyword "(" <> render term <> keyword ": " <> render t <> keyword ")"
render (Ann term t Inferred)             = wrap2 "ann" t $ render term

render (Let decls b)                     = wrap "let" $ keyword "let " <> br <> foldMap render decls <> keyword "in" <> br <> render b
render (Sig tname t)                     = wrap "sig" $ br <> name tname <> keyword " : " <> render t
render (Def tname b)                     = wrap "def" $ name tname <> keyword " = " <> render b

render (Comment text)                    = wrap "comment" $ txt "{-" <> txt text <> txt "-}"
render (Paren t)                         = wrap "paren" $ keyword "(" <> render t <> keyword ")"
render (Pos (SourcePos line col) t)      = span ! dataAttribute "line" (toValue line) ! dataAttribute "column" (toValue col) $ render t

render s@Sigma{}                       = wrap "sigma" $ txt "{" <> dispS s <> txt "}"
render p@(Prod _ _)                      = wrap "prod" $ txt "(" <> dispP p <> txt ")"

dispS :: Term -> Html
dispS (Sigma Nothing Nothing Nothing) = txt ""
dispS (Sigma ma (Just b) c)           = maybe "" (\a -> txt a <> txt ": ") ma <> dispS b <> maybe (txt "") (\x -> txt ", " <> dispS x) c
dispS x                               = render x

dispP :: Term -> Html
dispP (Prod Nothing Nothing) = txt ""
dispP (Prod (Just a) b)      = dispP a <> maybe (txt "") (\x -> txt ", " <> dispP x) b
dispP x                      = render x
