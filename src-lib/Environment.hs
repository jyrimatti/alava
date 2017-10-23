{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Environment where

import Foundation (($),(.),Maybe,fmap,(<>),listToMaybe,(==),(/=))
import Foundation.Collection (intercalate)

import Prelude (Show)
import qualified Prelude as P (show,error)
import GHC.Stack (HasCallStack)

import Data.Text.Lazy (Text,pack,unpack)

import PrettyPrint (display)
import Syntax (ETerm(EDef,ESig),SourcePos(SourcePos),TName)

show :: Show a => a -> Text
show = pack . P.show

data Env = Env { ctx :: [ETerm], sourceLocation :: [SourcePos] }

instance Show Env where
  show (Env c pos) = unpack $ intercalate "\n" (fmap (\t -> "\n  " <> display t <> "   ... " <> show t) c) <> "\n" <> show pos

emptyEnv :: Env
emptyEnv = Env {
    ctx = [],
    sourceLocation = []
}

lookupDef :: Env -> TName -> Maybe ETerm
lookupDef env v = listToMaybe [a | EDef _ v' a _ <- ctx env, v == v']

lookupTy :: Env -> TName -> Maybe ETerm
lookupTy env v = listToMaybe [ty | ESig _ v' ty <- ctx env, v == v'] 

extendCtx :: HasCallStack => ETerm -> Env -> Env
extendCtx d@(ESig _ name _) env = case [x | ESig _ x _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> show d)
    _ -> env { ctx = d : ctx env }
extendCtx d@(EDef _ name _ _) env = case [x | EDef _ x _ _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> show d)
    _ -> env { ctx = d : ctx env }

extendSourceLocation :: SourcePos -> Env -> Env
extendSourceLocation pos env = env { sourceLocation = pos : sourceLocation env}

getSourceLocation :: Env -> SourcePos
getSourceLocation e = case sourceLocation e of
    x:_ -> x
    []  -> SourcePos (-1) (-1)