{-# LANGUAGE NoImplicitPrelude #-}
module Alava where

import Foundation (($),(.),fmap,fst,snd,Either,filter)

import Control.Monad.Logger (MonadLogger())
import Control.Monad.Except (runExceptT)

import Data.Text.Lazy (Text,null)

import qualified SimpleParser as P (expr,parse)
import TypeCheck  (inferType)
import Environment (emptyEnv)
import Syntax (Term,Type,SourcePos)
import Error (Error)

parse :: Text -> [Term]
parse expr = fmap fst . filter (null . snd) $ P.parse P.expr expr

infer :: MonadLogger m => Term -> m (Either [(Error, SourcePos)] (Term, Type))
infer expr = runExceptT $ inferType emptyEnv expr