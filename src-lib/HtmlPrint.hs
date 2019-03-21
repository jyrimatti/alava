{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module HtmlPrint where

import Foundation (($),(.),Maybe(Nothing,Just),return,(<>),(==),maybe,Bool(True,False))

import Prelude (Show)
import qualified Prelude as P (show)

import Text.Blaze.Html5 (ToValue(toValue),(!),Html,docTypeHtml,title,script,link,style,body,pre,code,br,span,toHtml,dataAttribute)
import qualified Text.Blaze.Html5 as Blaze (head,html,footer)
import qualified Text.Blaze.Html.Renderer.Text as TB
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.Lazy (Text,unlines,last,pack)
import Data.Foldable (foldMap)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Sigma,Prod),SourcePos(SourcePos),ETerm(EType,EVar,ELam,EApp,EPi,EAnn,ELet,ESig,EDef,ESigma,EProd))
import Error (Error(NotInScope,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedFunctionType,ExpectedType,LambdaMustHaveFunctionType,NotEqual,MustAnnotateLambda))
import PrettyPrint (printTerm,printETerm)

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
            ,".section.menu { width: 13em; display: block; float: none; }"

            ,".a-let, .a-sig, .a-def { display: block; }"
            ,".a-sig, .a-def { margin-left: 1em; }"
            ,".a-let > span { margin-left: 1em; }"

            ,".a-comment { color: #7f7f7f; }"
            ,".a-keyword { color: #BF772F; }"
            ,".a-name { color: #2FBFBF; }"
            ,".a-var { color: #5F7FBF; }"
            ,".a-def, .a-sig { color: #8C8C5E; }"
            ,"h1 > a { padding-left: 1em; opacity: 0.3; white-space: nowrap; }"
          ]

footer :: Html
footer =
  Blaze.footer $
    script ! A.type_ "text/javascript" $
      "tippy('span[title]', {theme: 'menu light'})"

wrap :: Text -> Html -> Html
wrap expressionType = span ! A.class_ (toValue $ "a-" <> expressionType)

wrap2 :: Text -> Term -> Html -> Html
wrap2 expressionType etype = span ! A.class_ (toValue $ "a-" <> expressionType) ! A.title (toValue $ TB.renderHtml $ term etype)

keyword :: Text -> Html
keyword str = toHtml str ! A.class_ "a-keyword"

name :: Text -> Html
name str = toHtml str ! A.class_ "a-name"

term :: Term -> Html
term Type                              = wrap "type" $ keyword "Type"
term (Var tname)                       = wrap "var" $ name tname
term (Lam tname Nothing b)                             = wrap  "lam"   $ keyword "(\\" <> name tname <>                             keyword ". " <> term b <> keyword ")"
term (Lam tname (Just t) b)                            = wrap  "lam"   $ keyword "(\\" <> name tname <> keyword ": " <> term t <> keyword ". " <> term b <> keyword ")"
term (App f param)                     = wrap "app" $ keyword "(" <> term f <> " " <> term param <> keyword ")"
term (Pi Nothing ptype rtype)          = wrap "pi" $ keyword "(" <> term ptype <> keyword " -> " <> term rtype <> keyword ")"
term (Pi (Just tname) ptype rtype)     = wrap "pi" $ keyword "(" <> keyword "(" <> name tname <> keyword ": " <> term ptype <> keyword ") -> " <> term rtype <> keyword ")"

term (Ann tr t)                        = wrap  "ann"   $ keyword "(" <> term tr <> keyword ": " <> term t <> keyword ")"

term (Let decls b)                     = wrap "let" $ keyword "let " <> br <> "\n" <> foldMap term decls <> br <> "\n" <> keyword "in" <> br <> "\n" <> term b

-- FIXME:
term (Sig tname _) | last tname == '_' = ""
term (Sig tname t)                     = wrap "sig" $ "\n" <> name tname <> keyword " : " <> term t <> "\n"
term (Def tname _) | last tname == '_' = ""
term (Def tname b@Lam{}) | isRecord b  = wrap "def" $ name tname <> recordParams b
term (Def tname b)                     = wrap "def" $ name tname <> keyword " = " <> term b

term (Comment text)                    = wrap "comment" $ "{-" <> toHtml text <> "-}"
term (Paren t)                         = wrap "paren" $ keyword "(" <> term t <> keyword ")"
term (Pos (SourcePos line col) t)      = span ! dataAttribute "line" (toValue line) ! dataAttribute "column" (toValue col) $ term t

term s@Sigma{}                         = wrap "sigma" $ "{" <> dispS s <> "}"
term p@(Prod _ _)                      = wrap "prod" $ "{" <> dispP p <> "}"

isRecord :: Term -> Bool
isRecord (Lam _ _ Sigma{}) = True
isRecord (Lam _ _ b)       = isRecord b
isRecord _                 = False
recordParams :: Term -> Html
recordParams (Lam x (Just t) b) = " (" <> name x <> keyword ": " <> term t <> ")" <> recordParams b
recordParams b                  = keyword " = " <> term b

dispS :: Term -> Html
dispS (Sigma Nothing Nothing Nothing) = ""
dispS (Sigma ma (Just b) c)           = maybe "" (\a -> toHtml a <> ": ") ma <> dispS b <> maybe "" (\x -> ", " <> dispS x) c
dispS x                               = term x

dispP :: Term -> Html
dispP (Prod Nothing Nothing) = ""
dispP (Prod (Just a) b)      = dispP a <> maybe "" (\x -> ", " <> dispP x) b
dispP x                      = term x


eterm :: ETerm -> Html
eterm EType                              = wrap "type" $ keyword "Type"
eterm (EVar (Var tname) _)                 = wrap "var" $ name tname
eterm (ELam (Lam tname Nothing b) _ _ _)                            = wrap  "lam"   $ keyword "(\\" <> name tname <>                             keyword ". " <> term b <> keyword ")"
eterm (ELam (Lam tname (Just t) b) _ _ _)                            = wrap  "lam"   $ keyword "(\\" <> name tname <> keyword ": " <> term t <> keyword ". " <> term b <> keyword ")"
eterm (EApp (App f param) _ _ _)                     = wrap "app" $ keyword "(" <> term f <> " " <> term param <> keyword ")"
eterm (EPi (Pi Nothing ptype rtype) _ _)          = wrap "pi" $ keyword "(" <> term ptype <> keyword " -> " <> term rtype <> keyword ")"
eterm (EPi (Pi (Just tname) ptype rtype) _ _)     = wrap "pi" $ keyword "(" <> keyword "(" <> name tname <> keyword ": " <> term ptype <> keyword ") -> " <> term rtype <> keyword ")"

eterm (EAnn (Ann tr t) _ _)              = wrap  "ann"   $ keyword "(" <> term tr <> keyword ": " <> term t <> keyword ")"

eterm (ELet (Let decls b) _ _ _)                     = wrap "let" $ keyword "let " <> br <> "\n" <> foldMap term decls <> br <> "\n" <> keyword "in" <> br <> "\n" <> term b

-- FIXME:
eterm (ESig (Sig tname _) _) | last tname == '_' = ""
eterm (ESig (Sig tname t) _)                     = wrap "sig" $ "\n" <> name tname <> keyword " : " <> term t <> "\n"
eterm (EDef (Def tname _) _ _) | last tname == '_' = ""
eterm (EDef (Def tname b@Lam{}) _ _) | isRecord b  = wrap "def" $ name tname <> recordParams b
eterm (EDef (Def tname b) _ _)                     = wrap "def" $ name tname <> keyword " = " <> term b

eterm (ESigma s@Sigma{} _ _)                         = wrap "sigma" $ "{" <> dispS s <> "}"
eterm (EProd p@(Prod _ _) _ _ _)                      = wrap "prod" $ "{" <> dispP p <> "}"



error :: Error -> Html
error (NotInScope _ var) = toHtml $ unlines
  ["Not in scope:"
  ,"  " <> var
  ]
error (NotEqual _ expected actual) = toHtml $ unlines
  ["Types don't match."
  ,"Expected:"
  ,"  " <> printETerm expected
  ,"Actual:"
  ,"  " <> printTerm actual
  ]
error (LambdaMustHaveFunctionType _ trm t) = toHtml $ unlines
  ["A lambda:"
  ,"  " <> printTerm trm
  ," was expected to have a function type, but instead had:"
  ,"  " <> printETerm t
  ]
error (ExpectedFunctionType _ trm typ) = toHtml $ unlines
  ["Expected a function type for:"
  ,"  " <> printETerm trm
  ,"but instead was:"
  ,"  " <> printETerm typ
  ]
error (ExpectedType _ trm) = toHtml $ unlines
  ["Expected Type for:"
  ,"  " <> printTerm trm
  ]
error (TypesDontMatch _ trm actual expected) = toHtml $ unlines
  ["Types don't match for term:"
  ,"  " <> printTerm trm
  ,"Expected:"
  ,"  " <> printETerm expected
  ,"Actual:"
  ,"  " <> printETerm actual
  ]
error (AppTypesDontMatch _ trm expected actual) = toHtml $ unlines
  ["Application argument type doesn't match the type of the function in:"
  ,"  " <> printTerm trm
  ,"Expected argument type:"
  ,"  " <> printETerm expected
  ,"Actual: parameter type"
  ,"  " <> printTerm actual
  ]
error (CouldNotInferType _ trm) = toHtml $ unlines
  ["Could not infer type for:"
  ,"  " <> printTerm trm
  ]
error (MustAnnotateLambda _ trm) = toHtml $ unlines
  ["Must annotate lambda:"
  ,"  " <> printTerm trm
  ]
