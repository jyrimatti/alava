{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Substitution where

import Foundation (($),(<>),(==),show,Maybe(Just,Nothing),error)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Sigma,Comment),TName)

-- poor man's substitution, for now...
subst :: Maybe TName -> Term -> Term -> Term
subst Nothing _ x = x
subst (Just name) param (Ann x y z) = Ann (subst (Just name) param x) (subst (Just name) param y) z
subst (Just _)    _     Type = Type
subst (Just name) param (Var x) | x == name = param
subst (Just _)    _     x@(Var _) = x
subst (Just name) param (App x y) = App (subst (Just name) param x) (subst (Just name) param y)
subst (Just name) param (Pi x y z) = Pi x (subst (Just name) param y) (subst (Just name) param z)
subst (Just name) param (Lam x Nothing z) = Lam x Nothing (subst (Just name) param z)
subst (Just name) param (Lam x (Just y) z) = Lam x (Just (subst (Just name) param y)) (subst (Just name) param z)
subst (Just _   ) _     s@(Sigma _ Nothing Nothing) = s
subst (Just name) param (Sigma x (Just y) Nothing) = Sigma x (Just (subst (Just name) param y)) Nothing
subst (Just name) param (Sigma x (Just y) (Just z)) = Sigma x (Just (subst (Just name) param y)) (Just (subst (Just name) param z))
subst _ _ x@(Comment _) = x
subst a b c = error $ "subst: " <> show a <> " " <> show b <> " " <> show c