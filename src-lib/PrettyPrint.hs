{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module PrettyPrint where

import Foundation (String,Maybe(Just,Nothing),(<>),show,maybe)

import Data.Foldable (foldMap)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Sigma,Prod,Case),AnnType(Inferred))

class Display x where
  display :: x -> String

instance Display Term where
  display Type = "Type"
  display (Var tname)                       = tname
  display (Lam tname Nothing body)          = "\\" <> tname <> ". " <> display body
  display (Lam tname (Just (Ann (Comment _) t Inferred)) body) = "\\" <> tname <> ":" <> display t <> ". " <> display body
  display (Lam tname (Just t) body)         = "\\" <> tname <> ":" <> display t <> ". " <> display body
  display (App f param)                     = display f <> " " <> display param
  display (Pi Nothing ptype rtype)          = display ptype <> " -> " <> display rtype
  display (Pi (Just name) ptype rtype)      = "(" <> name <> ":" <> display ptype <> ") -> " <> display rtype
  display (Ann term t _)                    = "(" <> display term <> ":" <> display t <> ")"
  display (Let decls body)                  = "let \n" <> foldMap display decls <> "in\n  " <> display body
  display (Sig name t)                      = "\n  " <> name <> " : " <> display t <> "\n"
  display (Def name body)                   = "  " <> name <> " = " <> display body <> "\n"
  display (Comment text)                    = "{-" <> text <> "-}\n"
  display (Paren t)                         = "(" <> display t <> ")"
  display (Pos _ t)                         = display t
  display s@Sigma{}                         = "{" <> dispS s <> "}"
  display p@(Prod _ _)                      = "(" <> dispP p <> ")"
  display (Case a b c)                      = "case " <> display a <> " of\n " <> display b <> " -> " <> display c

dispS :: Term -> String
dispS (Sigma Nothing Nothing Nothing) = ""
dispS (Sigma ma (Just b) c)           = maybe "" (\a -> show a <> ": ") ma <> dispS b <> maybe "" (\x -> ", " <> dispS x) c
dispS x                               = display x

dispP :: Term -> String
dispP (Prod Nothing Nothing) = ""
dispP (Prod (Just a) b)      = dispP a <> maybe "" (\x -> ", " <> dispP x) b
dispP x                      = display x
