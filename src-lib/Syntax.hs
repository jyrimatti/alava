{-# LANGUAGE TypeFamilies, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable, DataKinds, NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
module Syntax where

import Foundation (($),(.),Int,Maybe(Just,Nothing),toList,(<>),Eq)
import Data.Text.Lazy (Text,pack,unpack)

import Prelude (Show)
import qualified Prelude as P (show)
import Data.Foldable (fold)
import Data.Functor.Foldable.TH
import Data.Functor.Foldable (cata)

type TName = Text

type Type = Term

data SourcePos = SourcePos Int Int deriving (Show,Eq)

data Term = 
   -- Core language
     Type
   | Var TName
   | Lam TName (Maybe Term) Term
   | App Term Term
   | Pi (Maybe TName) Term Term
   
   -- Explicit type hints for terms
   | Ann Term Term

   -- Modules
   | Let [Term] Term
   | Sig TName Term
   | Def TName Term

   -- Syntactic conveniences
   | Comment Text
   | Paren Term
   | Pos SourcePos Term

   | Sigma (Maybe TName) (Maybe Term) (Maybe Term)
    
   | Prod (Maybe Term) (Maybe Term)
   | Case Term Term Term
  deriving Eq                  

paren :: Text -> Text
paren t = "(" <> t <> ")"

parenMaybe :: Maybe Text -> Text
parenMaybe Nothing = "Nothing"
parenMaybe (Just term)  = "(Just " <> paren term <> ")"

makeBaseFunctor ''Term

instance Show Term where
  show = unpack . cata (\case
    TypeF                -> "Type"
    VarF name            -> "Var " <> name
    LamF name mtype term -> "Lam " <> name <> " " <> parenMaybe mtype <> " " <> paren term
    AppF f arg           -> "App " <> paren f <> " " <> paren arg
    PiF mname a b        -> "Pi " <> parenMaybe mname <> " " <> paren a <> " " <> paren b
    AnnF term typeAnnot  -> "Ann " <> paren term <> " " <> paren typeAnnot
    LetF xs body         -> "Let " <> fold xs <> " " <> paren body
    SigF name typeSig    -> "Sig " <> name <> " " <> paren typeSig
    DefF name term       -> "Def " <> name <> " " <> paren term
    CommentF txt         -> "Comment " <> txt
    ParenF term          -> "Paren " <> paren term
    PosF _ term          -> "*" <> term
    SigmaF mname ma mb   -> "Sigma " <> parenMaybe mname <> " " <> parenMaybe ma <> " " <> parenMaybe mb
    ProdF ma mb          -> "Prod " <> parenMaybe ma <> " " <> parenMaybe mb
    CaseF a b c          -> "Case " <> paren a <> " " <> paren b <> " " <> paren c
   )

type EType = ETerm

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

makeBaseFunctor ''ETerm