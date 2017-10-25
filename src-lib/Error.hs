{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Error where

import Foundation ((.),(<>))

import Prelude (Show)
import qualified Prelude as P (show,String)

import Data.Text.Lazy (Text,pack,unpack,intercalate)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack (MonadLogger)
import Control.Monad.Except (ExceptT,throwError)

import Syntax (Term,SourcePos,Type,ETerm,EType)
import Environment (Env,getSourceLocation)
import PrettyPrint (display)

show :: Show a => a -> Text
show = pack . P.show

type ResultM = ExceptT [(Error,SourcePos)]

err :: Error -> [(Error,SourcePos)]
err e = [(e, getSourceLocation (getEnv e))]

throwErr :: MonadLogger m => Error -> ResultM m a
throwErr = throwError . err

data Error = DefinitionNotFound Env Text ETerm
           | NotInScope Env Text
           | NotEqual Env EType Type
           | LambdaMustHaveFunctionType Env Term EType
           | ExpectedFunctionType Env ETerm EType
           | ExpectedType Env Term
           | TypesDontMatch Env Term EType EType
           | AppTypesDontMatch Env Term EType Type
           | CouldNotInferType Env Term
           | MustAnnotateLambda Env Term

getEnv :: Error -> Env
getEnv (DefinitionNotFound e _ _) = e
getEnv (NotInScope e _) = e
getEnv (NotEqual e _ _) = e
getEnv (LambdaMustHaveFunctionType e _ _) = e
getEnv (ExpectedFunctionType e _ _)= e
getEnv (ExpectedType e _)= e
getEnv (TypesDontMatch e _ _ _) = e
getEnv (AppTypesDontMatch e _ _ _) = e
getEnv (CouldNotInferType e _) = e
getEnv (MustAnnotateLambda e _) = e

unlines :: [Text] -> P.String
unlines = unpack . intercalate "\n"

instance Show Error where
  show (DefinitionNotFound env var expected) = unlines
    ["Definition not found for variable:"
    ,"  " <> var
    ,"Expected:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Env:"
    ,show env
    ]
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
  show (MustAnnotateLambda env term) = unlines
    ["Must annotate lambda:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Env:"
    ,show env
    ]