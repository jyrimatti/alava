{-# LANGUAGE NoImplicitPrelude #-}
module Alava where

import Foundation (($),(.),null,fmap,fst,snd,IO,String,Either,filter)

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Except (runExceptT)

import qualified Prelude as P

import qualified SimpleParser as P (expr,parse)
import TypeCheck  (inferType)
import Environment (emptyEnv)
import Syntax (Term,Type,SourcePos)
import Error (Error)

parse :: String -> [Term]
parse expr = fmap fst . filter (null . snd) $ P.parse P.expr expr

infer :: String -> IO (Either [(Error, SourcePos)] (Term, Type))
infer contents = runStdoutLoggingT $ runExceptT (inferType emptyEnv $ P.head $ parse contents)
