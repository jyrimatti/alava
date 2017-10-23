{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists, BangPatterns #-}
module Equal where

import Foundation (($),(.),pure,(<>),Bool(True,False),(==),(&&),Maybe(Just,Nothing),(<),(>),Ordering(LT,EQ,GT),otherwise,uncurry)
import Foundation.Collection (sortBy)

import Prelude (Show)
import qualified Prelude as P (show,error)

import Control.Monad.Logger.CallStack (logDebug,MonadLogger)

import GHC.Stack (HasCallStack)

import Data.Foldable (all,foldl)
import Data.List (zip)
import Data.Text.Lazy (Text,pack,unpack,toStrict)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack ()
import Control.Monad.Except (ExceptT,throwError)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Paren,Let,Def,Sig,Sigma,Prod,Pos),SourcePos,Type,TName,ETerm(EType,EVar,ELam,EApp,EPi,EAnn,ELet,EDef,ESig,ESigma,EProd),EType)
import Environment (Env,lookupDef,extendCtxSig,extendCtxDef)
import Substitution (subst)
import Error(Error(ExpectedFunctionType),err)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment

show :: Show a => a -> Text
show = pack . P.show

type ResultM = ExceptT [(Error,SourcePos)]

equate :: (MonadLogger m, HasCallStack) => Env -> ETerm -> ETerm -> ResultM m Bool
equate env t1 t2 = do
  logDebug $ toStrict $ "Equating\n  " <> show t1 <> "\n  " <> show t2
  let n1 = whnf' env False t1  
  let n2 = whnf' env False t2
  logDebug $ toStrict $ "in WHNF:\n  " <> show n1 <> "\n  " <> show n2
  !ret <- pure $ equate_ env t1 t2
  logDebug $ toStrict $ "Equating done: " <> show ret
  pure ret

equate_ :: HasCallStack => Env -> ETerm -> ETerm -> Bool
equate_ _ t1 t2 | t1 == t2 = True
equate_ env t1 t2 = let
  n1 = whnf' env False t1  
  n2 = whnf' env False t2
  o (EDef _ x1 _ _) (EDef _ x2 _ _)
    | x1 < x2 = LT
    | x1 > x2 = GT
    | otherwise = EQ
  recEquate :: HasCallStack => Text -> ETerm -> Bool
  recEquate x n = case lookupDef env x of 
           Just d -> equate_ env d n
           Nothing -> P.error $ unpack $ "Expected: " <> show n <> " , found: " <> show (Var x) <> " , env: " <> show env
  equateMaybe Nothing Nothing   = True
  equateMaybe (Just a) (Just b) = equate_ env a b
  equateMaybe _ _               = False
  in case (n1, n2) of
    (EType{}, EType{})                     -> True
    (EVar _ x _, EVar _ y _)                   -> x == y
    (ELam _ _ _ b1 _, ELam _ _ _ b2 _)         -> equate_ env b1 b2
    (EApp _ a1 a2 _, EApp _ b1 b2 _)           -> equate_ env a1 b1 && equate_ env a2 b2
    (EPi _ _ tyA1 tyB1, EPi _ _ tyA2 tyB2) -> equate_ env tyA1 tyA2 && equate_ env tyB1 tyB2
    --(Ann (Var x) at1 Inferred, at2)  -> equate_ env (fromMaybe at1 $ lookupDef env x) at2
    --(Ann x at1 _, at2)               -> equate_ env x at2 || equate_ env at1 at2
    --(at1, Ann (Var x) at2 Inferred)  -> equate_ env at1 (fromMaybe at2 $ lookupDef env x)
    --(at1, Ann x at2 _)               -> equate_ env at1 x || equate_ env at1 at2
    (ELet _ xs1 body1, ELet _ xs2 body2)   -> equate_ env body1 body2 && all (uncurry (equate_ env)) (zip (sortBy o xs1) (sortBy o xs2))
    (ESigma _ tyA1 tyB1 _, ESigma _ tyA2 tyB2 _) -> equateMaybe tyA1 tyA2 && equateMaybe tyB1 tyB2
    (EProd _ a1 a2 _, EProd _ b1 b2 _)         -> equateMaybe a1 b1 && equateMaybe a2 b2
    (EVar _ x _, _)                       -> recEquate x n2
    (_, EVar _ x _)                       -> recEquate x n1
    (_,_)                            -> False

ensurePi :: (MonadLogger m, HasCallStack) => Env -> EType -> ResultM m (Maybe TName, EType, EType)
ensurePi env ty = case whnf env ty of 
    Whnf (EPi _ mname tyA tyB) -> pure (mname, tyA, tyB)
    Whnf nf -> throwError $ err $ ExpectedFunctionType env ty nf



newtype Whnf = Whnf ETerm deriving Show

whnf :: HasCallStack => Env -> ETerm -> Whnf
whnf env t = Whnf $ whnf' env False t
  
whnf' :: HasCallStack => Env -> Bool -> ETerm -> ETerm       
whnf' env b t@(EVar _ x _) = case lookupDef env x of 
    (Just d) -> whnf' env b d 
    _        -> t

whnf' env b (EApp t t1 t2 tt) = case whnf' env b t1 of 
    (ELam _ x _ body _)          -> whnf' env b $ subst (Just x) t2 body
    --(Pi (Just name) _ body) -> whnf' (extendCtx (Sig name t2) env) b body
    nf@(EVar t x _) -> let nf2 = whnf' env b t2
                  in case lookupDef env x of
                      (Just d) -> whnf' env False (EApp t d nf2 tt)
                      _        -> EApp t nf nf2 tt
    nf      -> EApp t nf t2 tt

whnf' env b (ELet _ xs body) = let
  foo e (ESig t n ty) = let
      ety = whnf' e b ty
    in extendCtxSig n ety e
  foo e (EDef t name x y) = let
      ety = whnf' e b x
      ety2 = whnf' e b y
    in extendCtxDef name ety e
  newEnv = foldl foo env xs
  in whnf' newEnv b body

whnf' _ _ tm = tm
