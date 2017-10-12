{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module HtmlPrint where

import Foundation (($),(.),Maybe(Nothing,Just),toList,return,(<>),(==),maybe)

import Prelude (Show)
import qualified Prelude as P (show)

import Text.Blaze.Html5 (ToValue(toValue),(!),Html,docTypeHtml,title,script,link,style,body,pre,code,br,span,toHtml,dataAttribute)
import qualified Text.Blaze.Html5 as Blaze (head,html,footer)
import qualified Text.Blaze.Html.Renderer.Text as TB
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.Lazy (Text,unlines,last,pack)
import Data.Foldable (foldMap)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Sigma,Prod),SourcePos(SourcePos),AnnType(Inferred,UserGiven))
import Error (Error(NotInScope,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedFunctionType,ExpectedType,LambdaMustHaveFunctionType,NotEqual,MustAnnotateLambda))
import PrettyPrint (display)

show :: Show a => a -> Text
show = pack . P.show

html :: Html -> Html
html codeContent = docTypeHtml $
  Blaze.html $ do
    head
    body $ do
      pre $
        code codeContent
      footer

head :: Html
head = Blaze.head $ do
     title "Alava"
     script ! A.type_ "text/javascript" ! A.src "tippy.min.js" $ return ()
     link ! A.rel "stylesheet" ! A.href "tippy.css"
     link ! A.rel "stylesheet" ! A.href "https://lahteenmaki.net/style.css"
     style ! A.type_ "text/css" $
        toHtml $ unlines [ ""
            ,".section { max-width: 35%; float: right; }"
            ,".section.menu { width: 20em; display: block; float: none; }"

            ,".a-let, .a-sig, .a-def { display: block; }"
            ,".a-sig, .a-def { margin-left: 1em; }"
            ,".a-let > span { margin-left: 1em; }"

            ,".a-comment { color: #7f7f7f; }"
            ,".a-keyword { color: #BF772F; }"
            ,".a-name { color: #2FBFBF; }"
            ,".a-var { color: #5F7FBF; }"
            ,".a-def, .a-sig { color: #8C8C5E; }"
          ]

footer :: Html
footer =
  Blaze.footer $
    script ! A.type_ "text/javascript" $
      "tippy('span[title]', {theme: 'menu light'})"

wrap :: Text -> Html -> Html
wrap cls = span ! A.class_ (toValue $ "a-" <> cls)

wrap2 :: Text -> Term -> Html -> Html
wrap2 cls etype = span ! A.class_ (toValue $ "a-" <> cls) ! A.title (toValue $ TB.renderHtml $ term etype)

txt :: Text -> Html
txt = toHtml . toList

keyword :: Text -> Html
keyword str = toHtml (toList str) ! A.class_ "a-keyword"

name :: Text -> Html
name str = toHtml (toList str) ! A.class_ "a-name"

term :: Term -> Html
term Type                              = wrap "type" $ keyword "Type"
term (Var tname)                       = wrap "var" $ name tname
term (Lam tname Nothing b)                             = wrap  "lam"   $ keyword "\\" <> name tname <>                             keyword ". " <> term b
term (Lam tname (Just (Ann (Comment _) t Inferred)) b) = wrap2 "lam" t $ keyword "\\" <> name tname <>                             keyword ". " <> term b
term (Lam tname (Just t) b)                            = wrap  "lam"   $ keyword "\\" <> name tname <> keyword ": " <> term t <> keyword ". " <> term b
term (App f param)                     = wrap "app" $ term f <> txt " " <> term param
term (Pi Nothing ptype rtype)          = wrap "pi" $ term ptype <> keyword " -> " <> term rtype
term (Pi (Just tname) ptype rtype)     = wrap "pi" $ keyword "(" <> name tname <> keyword ": " <> term ptype <> keyword ") -> " <> term rtype

term (Ann tr t UserGiven)              = wrap  "ann"   $ keyword "(" <> term tr <> keyword ": " <> term t <> keyword ")"
term (Ann (Comment _) t Inferred)      = wrap2 "ann" t $ term t

term (Let decls b)                     = wrap "let" $ keyword "let " <> br <> "\n" <> foldMap term decls <> br <> "\n" <> keyword "in" <> br <> "\n" <> term b

-- FIXME:
term (Sig tname _) | last tname == '_' = ""
term (Sig tname t)                     = wrap "sig" $ "\n" <> name tname <> keyword " : " <> term t <> "\n"
term (Def tname _) | last tname == '_' = ""
term (Def tname (Lam x (Just t) b@Sigma{})) = wrap "def" $ name tname <> " (" <> name x <> keyword ": " <> term t <> ")" <> keyword " = " <> term b
term (Def tname b)                     = wrap "def" $ name tname <> keyword " = " <> term b


term (Comment text)                    = wrap "comment" $ txt "{-" <> txt text <> txt "-}"
term (Paren t)                         = wrap "paren" $ keyword "(" <> term t <> keyword ")"
term (Pos (SourcePos line col) t)      = span ! dataAttribute "line" (toValue line) ! dataAttribute "column" (toValue col) $ term t

term s@Sigma{}                         = wrap "sigma" $ txt "{" <> dispS s <> txt "}"
term p@(Prod _ _)                      = wrap "prod" $ txt "{" <> dispP p <> txt "}"

dispS :: Term -> Html
dispS (Sigma Nothing Nothing Nothing) = txt ""
dispS (Sigma ma (Just b) c)           = maybe "" (\a -> txt a <> txt ": ") ma <> dispS b <> maybe (txt "") (\x -> txt ", " <> dispS x) c
dispS x                               = term x

dispP :: Term -> Html
dispP (Prod Nothing Nothing) = txt ""
dispP (Prod (Just a) b)      = dispP a <> maybe (txt "") (\x -> txt ", " <> dispP x) b
dispP x                      = term x



error :: Error -> Html
error (NotInScope _ var) = toHtml $ unlines
  ["Not in scope:"
  ,"  " <> var
  ]
error (NotEqual _ expected actual) = toHtml $ unlines
  ["Types don't match."
  ,"Expected:"
  ,"  " <> display expected
  ,"Actual:"
  ,"  " <> display actual
  ]
error (LambdaMustHaveFunctionType _ trm t) = toHtml $ unlines
  ["A lambda:"
  ,"  " <> display trm
  ," was expected to have a function type, but instead had:"
  ,"  " <> display t
  ]
error (ExpectedFunctionType _ trm typ) = toHtml $ unlines
  ["Expected a function type for:"
  ,"  " <> display trm
  ,"but instead was:"
  ,"  " <> display typ
  ]
error (ExpectedType _ trm) = toHtml $ unlines
  ["Expected Type for:"
  ,"  " <> display trm
  ]
error (TypesDontMatch _ trm actual expected) = toHtml $ unlines
  ["Types don't match for term:"
  ,"  " <> display trm
  ,"Expected:"
  ,"  " <> display expected
  ,"Actual:"
  ,"  " <> display actual
  ]
error (AppTypesDontMatch _ trm expected actual) = toHtml $ unlines
  ["Application argument type doesn't match the type of the function in:"
  ,"  " <> display trm
  ,"Expected argument type:"
  ,"  " <> display expected
  ,"Actual: parameter type"
  ,"  " <> display actual
  ]
error (CouldNotInferType _ trm) = toHtml $ unlines
  ["Could not infer type for:"
  ,"  " <> display trm
  ]
error (MustAnnotateLambda _ trm) = toHtml $ unlines
  ["Must annotate lambda:"
  ,"  " <> display trm
  ]
