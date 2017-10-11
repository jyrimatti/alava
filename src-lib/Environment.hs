{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Environment where

import Foundation (($),(.),Maybe,fmap,(<>),listToMaybe,(==),(/=),filter,Bool(True,False))
import Foundation.Collection (intercalate)

import Prelude (Show)
import qualified Prelude as P (show,error)
import GHC.Stack (HasCallStack)

import Data.Text.Lazy (Text,pack,unpack)

import PrettyPrint (display)
import Syntax (Term(Def,Sig),SourcePos(SourcePos),TName)

show :: Show a => a -> Text
show = pack . P.show

data Env = Env { ctx :: [Term], sourceLocation :: [SourcePos] }

instance Show Env where
  show (Env c pos) = unpack $ intercalate "\n" (fmap (\t -> display t <> "   ... " <> show t) c) <> "\n" <> show pos

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
extendCtx d@(Sig name _) env = case [x | Sig x _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> show d)
    _ -> env { ctx = d : ctx env }
extendCtx d@(Def name _) env = case [x | Def x _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> show d)
    _ -> env { ctx = d : ctx env }

removeFromCtx :: HasCallStack => Text -> Env -> Env
removeFromCtx name env = env { ctx = filter f (ctx env) }
  where f (Def n _) | n == name = False
        f _ = True

extendSourceLocation :: SourcePos -> Env -> Env
extendSourceLocation pos env = env { sourceLocation = pos : sourceLocation env}

getSourceLocation :: Env -> SourcePos
getSourceLocation e = case sourceLocation e of
    x:_ -> x
    []  -> SourcePos (-1) (-1)