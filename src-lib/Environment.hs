{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Environment where

import Foundation (($),(.),Show,Maybe,fmap,(<>),listToMaybe,(==),(/=),show,toList,Bool(True),error)
import Foundation.Collection (intercalate)

import qualified Prelude as P
import GHC.Stack (HasCallStack)
import Debug.Trace (trace)

import PrettyPrint (display)
import Syntax (Term(Def,Sig),SourcePos(SourcePos),TName)

data Env = Env { ctx :: [Term], sourceLocation :: [SourcePos] }

instance Show Env where
  show (Env c pos) = toList $ (intercalate "\n" $ fmap (\t -> display t <> "   ... " <> show t) $ c) <> "\n" <> show pos

emptyEnv :: Env
emptyEnv = Env {
    ctx = [],
    sourceLocation = []
}

lookupDef :: Env -> TName -> Maybe Term
lookupDef env v = listToMaybe [a | Def v' a <- ctx env, v == v']

lookupTy :: Env -> TName -> Maybe Term
lookupTy env v = listToMaybe [ty | Sig v' ty <- ctx env, v == v'] 

extendCtx :: HasCallStack => Term -> Env -> Env
extendCtx d@(Sig name _) env = trace (toList $ "extendCtx Sig: " <> name) $ case [x | Sig x _ <- ctx env, x == name] of
    [x] | name /= "_" -> P.error (toList ("Already in context: " <> show d))
    _ -> env { ctx = d:(ctx env) }
extendCtx d@(Def name _) env = trace (toList $ "extendCtx Def: " <> name) $ case [x | Def x _ <- ctx env, x == name] of
    [x] | name /= "_" -> P.error (toList ("Already in context: " <> show d))
    _ -> env { ctx = d:(ctx env) }

updateCtx d@(Sig name _) env = env { ctx = d:(ctx env) }
updateCtx d@(Def name _) env = env { ctx = d:(ctx env) }

extendSourceLocation :: SourcePos -> Env -> Env
extendSourceLocation pos env = env { sourceLocation = pos:(sourceLocation env)}

getSourceLocation :: Env -> SourcePos
getSourceLocation e = case sourceLocation e of
	x:_ -> x
	[] -> SourcePos (-1) (-1)