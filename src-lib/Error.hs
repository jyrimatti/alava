{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Error where

import Foundation ((.),(<>))

import Prelude (Show)
import qualified Prelude as P (show,String)

import Data.Text.Lazy (Text,pack,unpack,intercalate)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack ()
import Control.Monad.Except ()

import Syntax (Term,SourcePos,Type)
import Environment (Env,getSourceLocation)
import PrettyPrint (display)

show :: Show a => a -> Text
show = pack . P.show

err :: Error -> [(Error,SourcePos)]
err e = [(e, getSourceLocation (getEnv e))]

unlines :: [Text] -> P.String
unlines = unpack . intercalate "\n"

data Error = NotInScope Env Text
           | NotEqual Env Type Type
           | LambdaMustHaveFunctionType Env Term Type
           | ExpectedFunctionType Env Term Type
           | ExpectedType Env Term
           | TypesDontMatch Env Term Type Type
           | AppTypesDontMatch Env Term Type Type
           | CouldNotInferType Env Term

getEnv :: Error -> Env
getEnv (NotInScope e _) = e
getEnv (NotEqual e _ _) = e
getEnv (LambdaMustHaveFunctionType e _ _) = e
getEnv (ExpectedFunctionType e _ _)= e
getEnv (ExpectedType e _)= e
getEnv (TypesDontMatch e _ _ _) = e
getEnv (AppTypesDontMatch e _ _ _) = e
getEnv (CouldNotInferType e _) = e


instance Show Error where
  show (NotInScope env var) = unlines
    ["Not in scope:"
    ,"  " <> var
    ,"Env:"
    ,show env
    ]
  show (NotEqual env expected actual) = unlines
    ["Types don't match."
    ,"Expected:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual:"
    ,"  " <> display actual <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (LambdaMustHaveFunctionType env term t) = unlines
    ["A lambda:"
    ,"  " <> display term <> "   ... " <> show term
    ," was expected to have a function type, but instead had:"
    ,"  " <> display t <> "   ... " <> show t
    ,"Env:"
    ,show env
    ]
  show (ExpectedFunctionType env term typ) = unlines
    ["Expected a function type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"but instead was:"
    ,"  " <> display typ <> "   ... " <> show typ
    ,"Env:"
    ,show env
    ]
  show (ExpectedType env term) = unlines
    ["Expected Type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Env:"
    ,show env
    ]
  show (TypesDontMatch env term actual expected) = unlines
    ["Types don't match for term:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Expected:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual:"
    ,"  " <> display actual <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (AppTypesDontMatch env term expected actual) = unlines
    ["Application argument type doesn't match the type of the function in:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Expected argument type:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual: parameter type"
    ,"  " <> display actual <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (CouldNotInferType env term) = unlines
    ["Could not infer type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Env:"
    ,show env
    ]