{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Syntax where

import Foundation (($),(.),Int,Maybe(Just,Nothing),toList,(<>),Eq)
import Data.Text.Lazy (Text,pack)

import Prelude (Show)
import qualified Prelude as P (show)

show :: Show a => a -> Text
show = pack . P.show

type TName = Text

type Type = Term
type EType = ETerm

data SourcePos = SourcePos Int Int deriving (Show,Eq)

data Term = 
   -- Core language
     Type
   | Var TName
   | Lam TName (Maybe Type) Term
   | App Term Term
   | Pi (Maybe TName) Type Type
   
   -- Explicit type hints for terms
   | Ann Term Type

   -- Modules
   | Let [Term] Term
   | Sig TName Type
   | Def TName Term

   -- Syntactic conveniences
   | Comment Text
   | Paren Term
   | Pos SourcePos Term

   | Sigma (Maybe TName) (Maybe Type) (Maybe Type)
    
   | Prod (Maybe Term) (Maybe Term)
   | Case Term Term Term
  deriving Eq                  

-- Elaborated term
data ETerm = 
 --         original                           expression
 --         term                               elaborated type
 -------------------------------------------------------------
     EType
   | EVar   Term                               EType
   | ELam   Term   EType ETerm                 EType
   | EApp   Term   ETerm ETerm                 EType
   | EPi    Term   EType EType
   
   | EAnn   Term   ETerm                       EType

   | ELet   Term   [ETerm] ETerm               EType
   | ESig   Term                               EType
   | EDef   Term   ETerm                       EType

   | ESigma Term   (Maybe EType) (Maybe EType)
    
   | EProd  Term   (Maybe ETerm) (Maybe ETerm) EType
   | ECase  Term   ETerm ETerm ETerm           EType

  deriving (Eq,Show)

paren :: Term -> Text
paren t@Type = show t
paren t@(Pos _ Type) = show t
paren t = "(" <> show t <> ")"

parenMaybe :: Maybe Term -> Text
parenMaybe Nothing = "Nothing"
parenMaybe (Just term)  = "(Just " <> paren term <> ")"

printMaybe :: Maybe Text -> Text
printMaybe Nothing = "Nothing"
printMaybe (Just txt) = "(Just " <> show txt <> ")"

instance Show Term where
  show Type                  = "Type"
  show (Var name)            = toList $ "Var " <> show name
  show (Lam name mtype term) = toList $ "Lam " <> show name <> " " <> parenMaybe mtype <> " " <> paren term
  show (App f arg)           = toList $ "App " <> paren f <> " " <> paren arg
  show (Pi mname a b)        = toList $ "Pi " <> printMaybe mname <> " " <> paren a <> " " <> paren b
  show (Ann term typeAnnot)  = toList $ "Ann " <> paren term <> " " <> paren typeAnnot
  show (Let xs body)         = toList $ "Let " <> show xs <> " " <> paren body
  show (Sig name typeSig)    = toList $ "Sig " <> show name <> " " <> paren typeSig
  show (Def name term)       = toList $ "Def " <> show name <> " " <> paren term
  show (Comment txt)         = toList $ "Comment " <> show txt
  show (Paren term)          = toList $ "Paren " <> paren term
  show (Pos _ term)          = toList $ "*" <> show term
  show (Sigma mname ma mb)   = toList $ "Sigma " <> printMaybe mname <> " " <> parenMaybe ma <> " " <> parenMaybe mb
  show (Prod ma mb)          = toList $ "Prod " <> parenMaybe ma <> " " <> parenMaybe mb
  show (Case a b c)          = toList $ "Case " <> paren a <> " " <> paren b <> " " <> paren c
