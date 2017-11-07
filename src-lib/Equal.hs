{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Equal where

import Foundation (Applicative,($),(.),pure,(<>),Bool(True,False),(==),Maybe(Just,Nothing),compare,undefined)
import qualified Foundation ((&&))

import Prelude (Show)
import qualified Prelude as P (show)

import Control.Applicative (liftA2)
import Control.Monad.Logger.CallStack (logDebug, MonadLogger)

import Data.Foldable (foldl)
import Data.Text.Lazy (Text,pack,toStrict)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack ()

import Syntax (TName,Term(Var,Lam,Pi,Sig,Def),ETerm(EType,EVar,ELam,EApp,EPi,ELet,EDef,ESig,ESigma,EProd),EType)
import Environment (Env,lookupDef,extendCtxSig,extendCtxDef)
import Substitution (subst)
import Error(ResultM,Error(ExpectedFunctionType,DefinitionNotFound),throwErr)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment

show :: Show a => a -> Text
show = pack . P.show

-- lift 'and' to applicatives
(&&) :: Applicative f => f Bool -> f Bool -> f Bool
a && b = liftA2 (Foundation.&&) a b

equate :: MonadLogger m => Env -> ETerm -> ETerm -> ResultM m Bool
equate _ eterm1 eterm2 | eterm1 == eterm2 = pure True
equate env eterm1 eterm2 = let
    equate_ = equate env
    defOrdering (EDef (Def name1 _) _ _) (EDef (Def name2 _) _ _) = compare name1 name2
    recEquate variable expected = case lookupDef env variable of 
       Just def -> equate_ def expected
       Nothing -> throwErr $ DefinitionNotFound env variable expected
    equateMaybe (Just a) (Just b) = equate_ a b
    equateMaybe ma mb             = pure (ma == mb)
  in do
    logDebug $ toStrict $ "Equating\n  " <> show eterm1 <> "\n  " <> show eterm2
    let whnf1 = whnf' env eterm1  
    let whnf2 = whnf' env eterm2
    logDebug $ toStrict $ "in WHNF:\n  " <> show whnf1 <> "\n  " <> show whnf2

    res <- case (whnf1, whnf2) of
        (EType{}                , EType{})                -> pure True
        (EVar (Var name1) _     , EVar (Var name2) _)     -> pure $ name1 == name2
        (EVar (Var name) _      , otherETerm)             -> recEquate name otherETerm
        (otherETerm             , EVar (Var name) _)      -> recEquate name otherETerm
        (ELam _ _ body1 _       , ELam _ _ body2 _)       -> equate_ body1 body2
        (EApp _ f1 arg1 _       , EApp _ f2 arg2 _)       -> equate_ f1 f2 && equate_ arg1 arg2
        (EPi _ type1A type1B    , EPi _ type2A type2B)    -> equate_ type1A type2A && equate_ type1B type2B
        (ESigma _ type1A type1B , ESigma _ type2A type2B) -> equateMaybe type1A type2A && equateMaybe type1B type2B
        (EProd _ val1A val1B _  , EProd _ val2A val2B _)  -> equateMaybe val1A val2A && equateMaybe val1B val2B
        _                                                 -> pure False
    logDebug $ toStrict $ "Equating done: " <> show res
    pure res

ensurePi :: MonadLogger m => Env -> EType -> ResultM m (Maybe TName, EType, EType)
ensurePi env etype = case whnf env etype of 
    Whnf (EPi (Pi mname _ _) typeA typeB) -> pure (mname, typeA, typeB)
    Whnf nf -> throwErr $ ExpectedFunctionType env etype nf

newtype Whnf = Whnf ETerm deriving Show

whnf :: Env -> ETerm -> Whnf
whnf env eterm = Whnf $ whnf' env eterm
  
whnf' :: Env -> ETerm -> ETerm       
whnf' env eterm@(EVar (Var name) _) =
    case lookupDef env name of 
        Just vardef -> whnf' env vardef
        _           -> eterm

whnf' env (EApp term f arg etype) = let
    whnf_f   = whnf' env f
    whnf_arg = whnf' env arg
  in
    case whnf_f of 
        ELam (Lam name _ _) _ body _ -> whnf' env $ subst (Just name) arg body
        EVar (Var name) _            -> case lookupDef env name of
                                      Just vardef -> whnf' env $ EApp term vardef whnf_arg etype
                                      _           ->             EApp term whnf_f whnf_arg etype
        _                    -> EApp term whnf_f whnf_arg etype

whnf' env (ELet _ xs body _) = let
    extendCtx e (ESig (Sig name _) etype)   = extendCtxSig name (whnf' e etype) e
    extendCtx e (EDef (Def name _) eterm _) = extendCtxDef name (whnf' e eterm) e
    newEnv = foldl extendCtx env xs
  in whnf' newEnv body

whnf' _ tm = tm
