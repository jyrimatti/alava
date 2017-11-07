{-# LANGUAGE NoImplicitPrelude #-}
module Alava where

import Foundation ((.),fmap,fst,snd,Either,filter,Maybe)

import Control.Monad.Logger (MonadLogger())
import Control.Monad.Except (runExceptT)

import Data.Text.Lazy (Text,null)

import qualified SimpleParser as Parser (expr,parse)
import TypeCheck  (inferType)
import Environment (emptyEnv)
import Syntax (Term,EType,ETerm,SourcePos)
import Error (Error)

parse :: Text -> [Term]
parse = fmap fst . filter (null . snd) . Parser.parse Parser.expr

infer :: MonadLogger m => Term -> m (Either [(Error, Maybe SourcePos)] (ETerm, EType))
infer = runExceptT . inferType emptyEnv