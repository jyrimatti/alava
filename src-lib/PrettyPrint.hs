{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}

module PrettyPrint where

import Foundation (Maybe(Just,Nothing),(<>),($),(.),maybe)

import Prelude (Show)
import qualified Prelude as P (show)

import Data.Foldable (fold, foldMap)
import Data.Text.Lazy (Text, pack)

import Data.Functor.Foldable(cata)

import Syntax (TermF(..),ETermF(..),Term(..),ETerm(..))

printTerm :: Term -> Text
printTerm = cata $ \case
  TypeF                          -> "Type"
  VarF name                      -> name
  LamF name Nothing body         -> "\\" <> name <> ". " <> body
  LamF name (Just t) body        -> "\\" <> name <> ":" <> t <> ". " <> body
  AppF f arg                     -> f <> " " <> arg
  PiF Nothing a b                -> a <> " -> " <> b
  PiF (Just name) a b            -> "(" <> name <> ":" <> a <> ") -> " <> b
  AnnF term typeAnnot            -> "(" <> term <> ":" <> typeAnnot <> ")"
  LetF xs body                   -> "let \n" <> fold xs <> "in\n  " <> body
  SigF name typeSig              -> "\n  " <> name <> " : " <> typeSig <> "\n"
  DefF name term                 -> "  " <> name <> " = " <> term <> "\n"
  CommentF txt                   -> "{-" <> txt <> "-}\n"
  ParenF term                    -> "(" <> term <> ")"
  PosF _ term                    -> term
  SigmaF Nothing Nothing Nothing -> ""
  SigmaF ma (Just b) c           -> "{" <> maybe "" (<> ": ") ma <> b <> maybe "" (\x -> ", " <> x) c <> "}"
  ProdF Nothing Nothing          -> ""
  ProdF (Just a) b               -> "(" <> a <> maybe "" (\x -> ", " <> x) b <> ")"
  CaseF a b c                    -> "case " <> a <> " of\n " <> b <> " -> " <> c

printETerm :: ETerm -> Text
printETerm = \case
  EType              -> printTerm Type
  EVar term _        -> printTerm term
  ELam term _ _ _    -> printTerm term
  EApp term _ _ _    -> printTerm term
  EPi term _ _       -> printTerm term
  EAnn term _ _      -> printTerm term
  ELet term _ _ _    -> printTerm term
  ESig term _        -> printTerm term
  EDef term _ _      -> printTerm term
  ESigma term _ _    -> printTerm term
  EProd term _ _ _   -> printTerm term
  ECase term _ _ _ _ -> printTerm term
