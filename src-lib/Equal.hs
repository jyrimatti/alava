{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists, BangPatterns #-}
module Equal where

import Foundation (($),(.),pure,(<>),Bool(True,False),(==),(&&),Maybe(Just,Nothing),(<),(>),Ordering(LT,EQ,GT),IO,otherwise,uncurry)
import Foundation.Collection (sortBy)

import Prelude (Show)
import qualified Prelude as P (show,error)

import Control.Monad.Logger.CallStack (LoggingT,logDebug,MonadLogger)

import GHC.Stack (HasCallStack)

import Data.Foldable (all,foldl)
import Data.List (zip)
import Data.Text.Lazy (Text,pack,unpack,toStrict)

import Control.Monad.Morph ()
import Control.Monad.Logger.CallStack ()
import Control.Monad.Except (ExceptT,throwError)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Paren,Let,Def,Sig,Sigma,Prod,Pos),SourcePos,Type,TName)
import Environment (Env,lookupDef,extendCtx)
import Substitution (subst)
import Error(Error(ExpectedFunctionType),err)

show :: Show a => a -> Text
show = pack . P.show

type ResultM = ExceptT [(Error,SourcePos)]

equate :: (MonadLogger m, HasCallStack) => Env -> Term -> Term -> ResultM m Bool
equate env t1 t2 = do
  logDebug $ toStrict $ "Equating\n  " <> show t1 <> "\n  " <> show t2
  !ret <- pure $ equate_ env t1 t2
  logDebug "Equating done"
  pure ret

equate_ :: HasCallStack => Env -> Term -> Term -> Bool
equate_ env t1 t2 = let
  n1 = whnf' env False t1  
  n2 = whnf' env False t2
  o (Def x1 _) (Def x2 _)
    | x1 < x2 = LT
    | x1 > x2 = GT
    | otherwise = EQ
  recEquate :: HasCallStack => Text -> Term -> Bool
  recEquate x n = case lookupDef env x of 
           Just d -> equate_ env d n
           Nothing -> P.error $ unpack $ "Expected: " <> show n <> " , found: " <> show (Var x) <> " , env: " <> show env
  equateMaybe Nothing Nothing   = True
  equateMaybe (Just a) (Just b) = equate_ env a b
  equateMaybe _ _               = False
  in case (n1, n2) of
    (Type, Type)                     -> True
    (Var x, Var y)                   -> x == y
    (Lam _ _ b1, Lam _ _ b2)         -> equate_ env b1 b2
    (App a1 a2, App b1 b2)           -> equate_ env a1 b1 && equate_ env a2 b2
    (Pi _ tyA1 tyB1, Pi _ tyA2 tyB2) -> equate_ env tyA1 tyA2 && equate_ env tyB1 tyB2
    --(Ann (Var x) at1 Inferred, at2)  -> equate_ env (fromMaybe at1 $ lookupDef env x) at2
    (Ann _ at1 _, at2)               -> equate_ env at1 at2
    --(at1, Ann (Var x) at2 Inferred)  -> equate_ env at1 (fromMaybe at2 $ lookupDef env x)
    (at1, Ann _ at2 _)               -> equate_ env at1 at2
    (Paren at1, at2)                 -> equate_ env at1 at2
    (at1, Paren at2)                 -> equate_ env at1 at2
    (Let xs1 body1, Let xs2 body2)   -> equate_ env body1 body2 && all (uncurry (equate_ env)) (zip (sortBy o xs1) (sortBy o xs2))
    (Sigma _ tyA1 tyB1, Sigma _ tyA2 tyB2) -> equateMaybe tyA1 tyA2 && equateMaybe tyB1 tyB2
    (Prod a1 a2, Prod b1 b2)         -> equateMaybe a1 b1 && equateMaybe a2 b2
    (Var x, _)                       -> recEquate x n2
    (_, Var x)                       -> recEquate x n1
    (Pos _ _,_)                      -> P.error "No pos"
    (_, Pos _ _)                     -> P.error "No pos"
    (_,_)                            -> False

ensurePi :: (MonadLogger m, HasCallStack) => Env -> Type -> ResultM m (Maybe TName, Type, Type)
ensurePi env ty = case whnf env ty of 
    Whnf (Pi mname tyA tyB) -> pure (mname, tyA, tyB)
    Whnf nf -> throwError $ err $ ExpectedFunctionType env ty nf



newtype Whnf = Whnf Term deriving Show

whnf :: HasCallStack => Env -> Term -> Whnf
whnf env t = Whnf $ whnf' env False t
  
whnf' :: HasCallStack => Env -> Bool -> Term -> Term       
whnf' env b (Var x) = case lookupDef env x of 
    (Just d) -> whnf' env b d 
    _        -> Var x

whnf' env b (Ann (Var x) t annType) = case lookupDef env x of 
    --(Just d) -> whnf' env b d 
    --_        -> case lookupTy env x of -- hmm, can/should I do this?
      (Just d) -> whnf' env b d
      _        -> Ann (Var x) t annType

whnf' env b (App t1 t2) = case whnf' env b t1 of 
    (Lam x _ body)          -> let ret = whnf' env b $ subst (Just x) t2 body in ret
    --(Pi (Just name) _ body) -> whnf' (extendCtx (Sig name t2) env) b body
    nf@(Var x) -> let nf2 = whnf' env b t2
                  in case lookupDef env x of
                      (Just d) -> whnf' env False (App d nf2)
                      _        -> App nf nf2
    nf      -> App nf t2

whnf' env b (Let xs body) = let
  foo e (Sig n ty) = let
      ety = whnf' e b ty
    in extendCtx (Sig n ety) e
  foo e (Def name x) = let
      ety = whnf' e b x
    in extendCtx (Def name ety) e
  newEnv = foldl foo env xs
  in whnf' newEnv b body

--whnf' env b (Paren t) = whnf' env b t

whnf' _ _ t@(Ann _ _ _) = P.error $ unpack $ "Unexpected arg to whnf: " <> show t
whnf' _ _ t@(Paren _)   = P.error $ unpack $ "Unexpected arg to whnf: " <> show t
whnf' _ _ t@(Pos _ _)   = P.error $ unpack $ "Unexpected arg to whnf: " <> show t

whnf' _ _ tm = tm
