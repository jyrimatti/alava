{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Syntax where

import Foundation (($),String,Int,Maybe(Just,Nothing),Show,show,toList,(<>),fmap)
import Foundation.Collection (intercalate)

import qualified Prelude as P

type TName = String

type TCName = String
type DCName = String

type Type = Term

data AnnType = UserGiven | Inferred deriving Show
--instance Show AnnType where
--    show _ = ""

data Term = 
   -- Core language
     Type
   | Var TName
   | Lam TName (Maybe Type) Term
   | App Term Term
   | Pi (Maybe TName) Type Type
   
   -- Explicit type hints for terms
   | Ann Term Type AnnType

   -- Modules
   | Let [Term] Term
   | Sig TName Type
   | Def TName Term

   -- Syntactic conveniences
   | Comment String
   | Paren Term
   | Pos SourcePos Term

   | Sigma (Maybe TName) (Maybe Term) (Maybe Term)
    
   | Prod (Maybe Term) (Maybe Term)
   | Case Term Term Term
                              

paren :: Term -> String
paren t@Type = show t
paren t@(Pos _ Type) = show t
paren t = "(" <> show t <> ")"

paren2 :: Maybe Term -> String
paren2 Nothing = "Nothing"
paren2 (Just t)  = "(Just " <> paren t <> ")"

print :: Maybe Term -> String
print Nothing = "Nothing"
print (Just a) = "(Just " <> paren a <> ")"

print2 :: Show a => Maybe a -> String
print2 Nothing = "Nothing"
print2 (Just a) = "(Just " <> show a <> ")"

instance Show Term where
  show Type        = "Type"
  show (Var a)     = toList $ "Var " <> show a
  show (Lam a b c) = toList $ "Lam " <> show a <> " " <> paren2 b <> " " <> paren c
  show (App a b)   = toList $ "App " <> paren a <> " " <> paren b
  show (Pi a b c)  = toList $ "Pi " <> print2 a <> " " <> paren b <> " " <> paren c
  show (Ann a b c) = toList $ "Ann " <> paren a <> " " <> paren b <> " " <> show c
  show (Let a b)   = toList $ "Let " <> show a <> " " <> paren b
  show (Sig a b)   = toList $ "Sig " <> show a <> " " <> paren b
  show (Def a b)   = toList $ "Def " <> show a <> " " <> paren b
  show (Comment a) = toList $ "Comment " <> show a
  show (Paren a)   = toList $ "Paren " <> paren a
  show (Pos _ b)   = toList $ "*" <> show b
  show (Sigma a b c)     = toList $ "Sigma " <> print2 a <> " " <> print b <> " " <> print c
  show (Prod a b) = toList $ "Prod " <> paren2 a <> " " <> paren2 b
  show (Case a b c)      = toList $ "Case " <> paren a <> " " <> paren b <> " " <> paren c

data SourcePos = SourcePos Int Int deriving Show