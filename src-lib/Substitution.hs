{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Substitution where

import Foundation (($),(.),(<>),(==),Maybe(Just,Nothing))

import Prelude (Show)
import qualified Prelude as P (show,error)

import Data.Text.Lazy (Text, pack, unpack)

import Syntax (Term(Var),ETerm(EType,EVar,ELam,EApp,EPi,EAnn,ESigma),TName)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Syntax (Term(Var))

show :: Show a => a -> Text
show = pack . P.show

-- poor man's substitution, for now...
-- |
-- >>> subst (Just "x") (EVar (Var "y") EType) (EVar (Var "x") EType)
-- EVar Var "y" EType
subst :: Maybe TName -> ETerm -> ETerm -> ETerm
subst Nothing _ x = x
subst jn@(Just _) param (EAnn term eterm etype)          = EAnn term (subst jn param eterm) (subst jn param etype)
subst    (Just _)    _     x@EType{}                     = x
subst    (Just name) param (EVar (Var n) _) | n == name  = param
subst    (Just _)    _     x@EVar{}                      = x
subst jn@(Just _) param (EApp term f arg etype)          = EApp term (subst jn param f) (subst jn param arg) (subst jn param etype)
subst jn@(Just _) param (EPi term y z)                   = EPi term (subst jn param y) (subst jn param z)
subst jn@(Just _) param (ELam term etype eterm btype)    = ELam term (subst jn param etype) (subst jn param eterm) (subst jn param btype)
subst    (Just _   ) _     (ESigma term Nothing Nothing) = ESigma term Nothing Nothing
subst jn@(Just _) param (ESigma term (Just a) Nothing)   = ESigma term (Just (subst jn param a)) Nothing
subst jn@(Just _) param (ESigma term (Just a) (Just b))  = ESigma term (Just (subst jn param a)) (Just (subst jn param b))
subst a b c = P.error $ unpack $ "subst: " <> show a <> " " <> show b <> " " <> show c
