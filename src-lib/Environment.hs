{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Environment where

import Foundation (($),(.),Maybe(Just),fmap,(<>),listToMaybe,(==),(/=),pure)
import Foundation.Collection (intercalate)

import Prelude (Show)
import qualified Prelude as P (show,error)
import GHC.Stack (HasCallStack)

import Data.Text.Lazy (Text,pack,unpack)

import PrettyPrint (printETerm)
import Syntax (ETerm,EType,SourcePos,TName)

show :: Show a => a -> Text
show = pack . P.show

data EnvElement = Sig Text EType | Def Text ETerm deriving Show

data Env = Env { context :: [EnvElement], sourceLocation :: [SourcePos] }

instance Show Env where
  show (Env ctx pos) = unpack $ intercalate "\n" (fmap showElement ctx) <> "\n" <> show pos
    where showElement (Sig name etype) = "\n  " <> name <> " : " <> printETerm etype <> "   ... " <> show etype
          showElement (Def name eterm) = "\n  " <> name <> " = " <> printETerm eterm <> "   ... " <> show eterm

emptyEnv :: Env
emptyEnv = Env {
    context = [],
    sourceLocation = []
}

lookupDef :: Env -> TName -> Maybe ETerm
lookupDef env name = listToMaybe $ do
    elem <- context env
    case elem of
        Def defname value | defname == name -> pure value
        _                                   -> []

lookupSig :: Env -> TName -> Maybe EType
lookupSig env name = listToMaybe $ do
    elem <- context env
    case elem of
        Sig signame value | signame == name -> pure value
        _                                   -> []

extendCtxSig :: HasCallStack => Text -> EType -> Env -> Env
extendCtxSig name etype env = case lookupSig env name of
    Just _ | name /= "_" -> P.error (unpack $ "Already in context: " <> name)
    _                    -> env { context = Sig name etype : context env }

extendCtxDef :: HasCallStack => Text -> ETerm -> Env -> Env
extendCtxDef name eterm env = case lookupDef env name of
    Just _ | name /= "_" -> P.error (unpack $ "Already in context: " <> name)
    _                    -> env { context = Def name eterm : context env }

extendSourceLocation :: SourcePos -> Env -> Env
extendSourceLocation pos env = env { sourceLocation = pos : sourceLocation env}

getSourceLocation :: Env -> Maybe SourcePos
getSourceLocation = listToMaybe . sourceLocation
