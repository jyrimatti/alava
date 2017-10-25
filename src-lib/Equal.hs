{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
module Equal where

import Foundation (Applicative,($),(.),pure,(<>),Bool(True,False),(==),Maybe(Just,Nothing),compare)
import qualified Foundation ((&&))

import Prelude (Show)
import qualified Prelude as P (show)

import Control.Applicative (liftA2)
import Control.Monad.Logger.CallStack (logDebug, MonadLogger)

import Data.Foldable (foldl)
import Data.Text.Lazy (Text,pack,toStrict)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack ()

import Syntax (TName,ETerm(EType,EVar,ELam,EApp,EPi,ELet,EDef,ESig,ESigma,EProd),EType)
import Environment (Env,lookupDef,extendCtxSig,extendCtxDef)
import Substitution (subst)
import Error(ResultM,Error(ExpectedFunctionType,DefinitionNotFound),throwErr)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment

show :: Show a => a -> Text
show = pack . P.show

-- lift 'and' to work with wrappers
(&&) :: Applicative f => f Bool -> f Bool -> f Bool
a && b = liftA2 (Foundation.&&) a b

equate :: MonadLogger m => Env -> ETerm -> ETerm -> ResultM m Bool
equate _ eterm1 eterm2 | eterm1 == eterm2 = pure True
equate env eterm1 eterm2 = let
    equate_ = equate env
    defOrdering (EDef _ name1 _ _) (EDef _ name2 _ _) = compare name1 name2
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
        (EVar _ name1 _         , EVar _ name2 _)         -> pure $ name1 == name2
        (EVar _ name _          , otherETerm)             -> recEquate name otherETerm
        (otherETerm             , EVar _ name _)          -> recEquate name otherETerm
        (ELam _ _ _ body1 _     , ELam _ _ _ body2 _)     -> equate_ body1 body2
        (EApp _ f1 arg1 _       , EApp _ f2 arg2 _)       -> equate_ f1 f2 && equate_ arg1 arg2
        (EPi _ _ type1A type1B  , EPi _ _ type2A type2B)  -> equate_ type1A type2A && equate_ type1B type2B
        (ESigma _ type1A type1B , ESigma _ type2A type2B) -> equateMaybe type1A type2A && equateMaybe type1B type2B
        (EProd _ val1A val1B _  , EProd _ val2A val2B _)  -> equateMaybe val1A val2A && equateMaybe val1B val2B
        _                                                 -> pure False
    logDebug $ toStrict $ "Equating done: " <> show res
    pure res

ensurePi :: MonadLogger m => Env -> EType -> ResultM m (Maybe TName, EType, EType)
ensurePi env etype = case whnf env etype of 
    Whnf (EPi _ mname tyA tyB) -> pure (mname, tyA, tyB)
    Whnf nf -> throwErr $ ExpectedFunctionType env etype nf

newtype Whnf = Whnf ETerm deriving Show

whnf :: Env -> ETerm -> Whnf
whnf env t = Whnf $ whnf' env t
  
whnf' :: Env -> ETerm -> ETerm       
whnf' env t@(EVar _ x _) = case lookupDef env x of 
    (Just d) -> whnf' env d 
    _        -> t

whnf' env (EApp t t1 t2 tt) = case whnf' env t1 of 
    (ELam _ x _ body _)          -> whnf' env $ subst (Just x) t2 body
    nf@(EVar ttt x _) -> let nf2 = whnf' env t2
                  in case lookupDef env x of
                      (Just d) -> whnf' env (EApp ttt d nf2 tt)
                      _        -> EApp ttt nf nf2 tt
    nf      -> EApp t nf t2 tt

whnf' env (ELet _ xs body) = let
    foo e (ESig _ name  ty) = extendCtxSig name (whnf' e ty) e
    foo e (EDef _ name x y) = extendCtxDef name (whnf' e x) e
    newEnv = foldl foo env xs
  in whnf' newEnv body

whnf' _ tm = tm
