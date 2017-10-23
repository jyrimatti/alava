{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Environment where

import Foundation (($),(.),Maybe,fmap,(<>),listToMaybe,(==),(/=))
import Foundation.Collection (intercalate)

import Prelude (Show)
import qualified Prelude as P (show,error)
import GHC.Stack (HasCallStack)

import Data.Text.Lazy (Text,pack,unpack)

import PrettyPrint (display)
import Syntax (ETerm,EType,SourcePos(SourcePos),TName)

show :: Show a => a -> Text
show = pack . P.show

data EnvElement = Sig Text EType | Def Text ETerm deriving Show

data Env = Env { ctx :: [EnvElement], sourceLocation :: [SourcePos] }

instance Show Env where
  show (Env c pos) = unpack $ intercalate "\n" (fmap foo c) <> "\n" <> show pos
    where foo (Sig name etype) = "\n  " <> name <> " : " <> display etype <> "   ... " <> show etype
          foo (Def name eterm) = "\n  " <> name <> " = " <> display eterm <> "   ... " <> show eterm

emptyEnv :: Env
emptyEnv = Env {
    ctx = [],
    sourceLocation = []
}

lookupDef :: Env -> TName -> Maybe ETerm
lookupDef env v = listToMaybe [a | Def v' a <- ctx env, v == v']

lookupTy :: Env -> TName -> Maybe ETerm
lookupTy env v = listToMaybe [ty | Sig v' ty <- ctx env, v == v'] 

extendCtxSig :: HasCallStack => Text -> EType -> Env -> Env
extendCtxSig name etype env = case [x | Sig x _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> name)
    _ -> env { ctx = Sig name etype : ctx env }
extendCtxDef :: HasCallStack => Text -> ETerm -> Env -> Env
extendCtxDef name eterm env = case [x | Def x _ <- ctx env, x == name] of
    [_] | name /= "_" -> P.error (unpack $ "Already in context: " <> name)
    _ -> env { ctx = Def name eterm : ctx env }

extendSourceLocation :: SourcePos -> Env -> Env
extendSourceLocation pos env = env { sourceLocation = pos : sourceLocation env}

getSourceLocation :: Env -> SourcePos
getSourceLocation e = case sourceLocation e of
    x:_ -> x
    []  -> SourcePos (-1) (-1)