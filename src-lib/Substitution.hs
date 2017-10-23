{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Substitution where

import Foundation (($),(.),(<>),(==),Maybe(Just,Nothing))

import Prelude (Show)
import qualified Prelude as P (show,error)

import Data.Text.Lazy (Text, pack, unpack)

import Syntax (ETerm(EType,EVar,ELam,EApp,EPi,EAnn,ESigma),TName)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Syntax (Term(Var))

show :: Show a => a -> Text
show = pack . P.show

-- poor man's substitution, for now...
-- |
-- >>> subst (Just "x") (EVar (Var "y") "y" EType) (EVar (Var "x") "x" EType)
-- EVar Var "y" "y" EType
subst :: Maybe TName -> ETerm -> ETerm -> ETerm
subst Nothing _ x = x
subst (Just name) param (EAnn t y z) = EAnn t (subst (Just name) param y) (subst (Just name) param z)
subst (Just _)    _     t@EType{} = t
subst (Just name) param (EVar _ x _) | x == name = param
subst (Just _)    _     x@(EVar _ _ _) = x
subst (Just name) param (EApp t x y z) = EApp t (subst (Just name) param x) (subst (Just name) param y) (subst (Just name) param z)
subst (Just name) param (EPi t n y z) = EPi t n (subst (Just name) param y) (subst (Just name) param z)
subst (Just name) param (ELam t n x y z) = ELam t n (subst (Just name) param x) (subst (Just name) param y) (subst (Just name) param z)
subst (Just name) param (ESigma t Nothing Nothing y) = ESigma t Nothing Nothing (subst (Just name) param y)
subst (Just name) param (ESigma t (Just y) Nothing z) = ESigma t (Just (subst (Just name) param y)) Nothing (subst (Just name) param z)
subst (Just name) param (ESigma t (Just y) (Just z) w) = ESigma t (Just (subst (Just name) param y)) (Just (subst (Just name) param z)) (subst (Just name) param w)
subst a b c = P.error $ unpack $ "subst: " <> show a <> " " <> show b <> " " <> show c