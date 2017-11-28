{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module PrettyPrint where

import Foundation (Maybe(Just,Nothing),(<>),(.),maybe)

import Prelude (Show)
import qualified Prelude as P (show)

import Data.Foldable (foldMap)
import Data.Text.Lazy (Text, pack)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Sigma,Prod,Case),ETerm(EType,EVar,ELam,EApp,EPi,EAnn,ELet,ESig,EDef,ESigma,EProd,ECase))

show :: Show a => a -> Text
show = pack . P.show

class Display x where
  display :: x -> Text

instance Display Term where
  display Type = "Type"
  display (Var name)               = name
  display (Lam name Nothing body)  = "\\" <> name <> ". " <> display body
  display (Lam name (Just t) body) = "\\" <> name <> ":" <> display t <> ". " <> display body
  display (App f arg)              = display f <> " " <> display arg
  display (Pi Nothing a b)         = display a <> " -> " <> display b
  display (Pi (Just name) a b)     = "(" <> name <> ":" <> display a <> ") -> " <> display b
  display (Ann term typeAnnot)     = "(" <> display term <> ":" <> display typeAnnot <> ")"
  display (Let xs body)            = "let \n" <> foldMap display xs <> "in\n  " <> display body
  display (Sig name typeSig)       = "\n  " <> name <> " : " <> display typeSig <> "\n"
  display (Def name term)          = "  " <> name <> " = " <> display term <> "\n"
  display (Comment txt)            = "{-" <> txt <> "-}\n"
  display (Paren term)             = "(" <> display term <> ")"
  display (Pos _ term)             = display term
  display s@Sigma{}                = "{" <> dispS s <> "}"
  display p@(Prod _ _)             = "(" <> dispP p <> ")"
  display (Case a b c)             = "case " <> display a <> " of\n " <> display b <> " -> " <> display c

dispS :: Term -> Text
dispS (Sigma Nothing Nothing Nothing) = ""
dispS (Sigma ma (Just b) c)           = maybe "" (\a -> show a <> ": ") ma <> dispS b <> maybe "" (\x -> ", " <> dispS x) c
dispS x                               = display x

dispP :: Term -> Text
dispP (Prod Nothing Nothing) = ""
dispP (Prod (Just a) b)      = dispP a <> maybe "" (\x -> ", " <> dispP x) b
dispP x                      = display x

instance Display ETerm where
  display EType                = display Type
  display (EVar term _)        = display term
  display (ELam term _ _ _)    = display term
  display (EApp term _ _ _)    = display term
  display (EPi term _ _)       = display term
  display (EAnn term _ _)      = display term
  display (ELet term _ _ _)    = display term
  display (ESig term _)        = display term
  display (EDef term _ _)      = display term
  display (ESigma term _ _)    = display term
  display (EProd term _ _ _)   = display term
  display (ECase term _ _ _ _) = display term
