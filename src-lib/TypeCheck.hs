{-# LANGUAGE NoImplicitPrelude, ViewPatterns, TupleSections, OverloadedStrings, DeriveAnyClass #-}
module TypeCheck where

import Foundation (($),Maybe(Just,Nothing),fst,(<$>),pure,not,fromMaybe,(<>),show,toList,error)
import Foundation.Collection (reverse)
import Control.Monad (foldM)

import GHC.Stack (HasCallStack)
import Debug.Trace (trace)

import qualified Prelude as P
--import Debug.Trace (trace)

import Syntax (Type,Term(Var,Type,Pi,Lam,App,Ann,Pos,Paren,Let,Sig,Def,Comment,Sigma,Prod),AnnType(Inferred))
import Environment (Env,lookupTy,extendCtx,updateCtx,extendSourceLocation)
import Equal (Result(Success,Failure),Whnf(Whnf),whnf,err,Error(NotInScope,NotEqual,LambdaMustHaveFunctionType,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedType),with,equate,ensurePi)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment (emptyEnv)

-- |
-- >>> inferType (extendCtx (Sig "x" Type) emptyEnv) $ Var "x"
-- Success ...,Type)
--
-- >>> inferType emptyEnv $ Lam "x" (Just Type) (Var "x")
-- Success ...,Pi (Just "x") Type Type)
inferType :: HasCallStack => Env -> Term -> Result (Term,Type)
inferType env term = tcTerm env term Nothing

checkType :: HasCallStack => Env -> Term -> Type -> Result (Term,Type)
checkType env term expectedType = tcTerm env term $ Just $ whnf env expectedType

tcType :: HasCallStack => Env -> Term -> Result Term
tcType env tm = case checkType env tm Type of
    Failure es -> with es $ err $ ExpectedType env tm
    Success (a, b) -> pure a

tcTerm :: HasCallStack => Env -> Term -> Maybe Whnf -> Result (Term,Type)

tcTerm env t@(Var x) Nothing = case lookupTy env x of
  Just tyx -> pure (Ann t tyx Inferred, tyx)
  Nothing -> err $ NotInScope env x
 

tcTerm _ t@(Type) Nothing = pure (t,Type)

tcTerm env (Pi x tyA tyB) Nothing = do
  atyA <- tcType env tyA
  atyB <- tcType (case x of Just xx -> extendCtx (Sig xx atyA) env; Nothing -> env) tyB
  pure (Pi x atyA atyB, Type)
      
-- Check the type of a function    
tcTerm env (Lam x ma body) (Just (Whnf p@(Pi _ tyA tyB))) = do
    -- check tyA matches type annotation on binder, if present
    ea <- case ma of
        Just a  -> if not (equate env tyA a) then err (NotEqual env tyA a) else pure tyA
        Nothing -> pure tyA
    -- check the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x ea) env
    (ebody, _) <- checkType newEnv body tyB
    pure (Lam x (Just $ Ann (fromMaybe (Comment "") ma) ea Inferred) ebody, p)

--tcTerm env t@(Lam "_" s1 s2) (Just (Whnf Type)) = do
--  todo: --pure (t, Type)

tcTerm env t@(Lam _ _ _) (Just (Whnf nf)) = err $ LambdaMustHaveFunctionType env t nf

-- infer the type of a lambda expression, when an annotation
-- on the binder is present
tcTerm env (Lam x (Just annot) body) Nothing = do
    -- check that the type annotation is well-formed
    atyA          <- tcType env annot
    -- infer the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x atyA) env
    (ebody, atyB) <- inferType newEnv body
    pure (Lam x (Just $ Ann annot atyA Inferred) ebody, Pi (Just x) atyA atyB)

tcTerm env t@(App t1 t2) Nothing = do
    (at1, ty1)      <- inferType env t1
    (_, ty2)        <- inferType env t2
    (mname, argType, resType) <- ensurePi env ty1
    (at2, _) <- case checkType env t2 argType of
         Failure es -> with es $ err $ AppTypesDontMatch env t argType ty2
         s -> s
    let newEnv      = extendCtx (Def (fromMaybe "_" mname) $ at2) env
    (_, ty12)       <- inferType newEnv t1
    (_, _, etyB)    <- ensurePi newEnv ty12
    pure (App at1 at2, etyB)
                     


tcTerm env (Ann tm ty _) Nothing = do
    ty'         <- tcType env ty
    (tm', ty'') <- checkType env tm ty'
    pure (tm', ty'')   
  
tcTerm env (Pos p tm) mTy = tcTerm (extendSourceLocation p env) tm mTy
  
tcTerm env (Paren tm) mTy = tcTerm env tm mTy
  
--tcTerm t@(TrustMe ann1) ann2 = let  
--     expectedTy = matchAnnots t ann1 ann2
--  in (TrustMe (Annot (Just expectedTy)), expectedTy)

--tcTerm (TyUnit) Nothing = (TyUnit, Type)

--tcTerm (LitUnit) Nothing = (LitUnit, TyUnit)

--tcTerm (TyBool) Nothing = err [DS "unimplemented"]
  
--tcTerm (LitBool b) Nothing = err [DS "unimplemented"]
  
--tcTerm t@(If t1 t2 t3 ann1) ann2 = err [DS "unimplemented"]      
  
tcTerm env (Let xs body) ann = do
  let 
    foo a          (Pos _ b) = foo a b
    --foo a          (Sig n (Pos _ ty)) = foo a (Sig n ty)
    foo (e,exs)    (Sig n ty) = do
        (ety,_)    <- inferType e ty
        let newEnv = extendCtx (Sig n ety) e
        pure (newEnv, (Sig n ety):exs)
    --foo a          (Def n (Pos _ x)) = foo a (Def n x)
    foo (e,exs)    (Def name x) = case lookupTy e name of
        Nothing -> err $ NotInScope e name
        Just ty -> do
                    --let newEnv = extendCtx (Sig name ty) e
                    --(et2, ety2) <- checkType newEnv x ty
                    --let newEnv2 = extendCtx (Sig name ety2) e
                    --(eTy,_) <- inferType e ty
                    let newEnv = extendCtx (Def name x) e
                    (et2, ety2) <- checkType newEnv x ty
                    let newEnv2 = updateCtx (Def name et2) $ updateCtx (Sig name ety2) e
                    pure (newEnv2, (Def name et2):exs)
  (newEnv, exs)  <- foldM foo (env,[]) xs
  (ebody, etype) <- tcTerm newEnv body ann
  pure (Let (reverse exs) ebody, etype)
  
             
           
  
tcTerm _ s@(Sig _ t) _ = pure (s, t)

tcTerm env (Def name body) (Just (Whnf (Sigma _ (Just ann) Nothing))) = do
   (ebody, etype) <- checkType env body ann
   pure (Def name ebody, etype)

tcTerm env (Def name body) ann = do
   (ebody, etype) <- tcTerm env body ann
   pure (Def name ebody, etype)
      

    
tcTerm env (Sigma (Just x) (Just tyA) (Just tyB)) Nothing = do        
  atyA <- tcType env tyA
  atyB <- tcType (extendCtx (Sig x atyA) env) tyB
  pure (Sigma (Just x) (Just atyA) (Just atyB), Type)

tcTerm env (Sigma (Just x) (Just tyA) Nothing) Nothing = do        
  atyA <- tcType env tyA
  pure (Sigma (Just x) (Just atyA) Nothing, Type)

tcTerm env t@(Sigma Nothing Nothing Nothing) Nothing = pure (t, Type)

tcTerm env (Prod Nothing Nothing Nothing) Nothing = pure (Prod Nothing Nothing (Just (Sigma Nothing Nothing Nothing)), Sigma Nothing Nothing Nothing)

tcTerm env (Prod (Just a) Nothing mt) Nothing = do
  (ea,tyA) <- trace "prod1" $ inferType env a
  let ann = Sigma Nothing (Just tyA) Nothing
  et <- case mt of
        Just aa -> if not (equate env ann aa) then err (NotEqual env ann aa) else pure ann
        Nothing -> pure ann
  pure (Prod (Just ea) Nothing (Just et), et)

tcTerm env (Prod (Just a) Nothing mt) (Just (Whnf ann@(Sigma _ (Just aaa) Nothing))) = do
  (ea,tyA) <- trace "prod11" $ checkType env a aaa
  et <- case mt of
        Just aa -> if not (equate env ann aa) then err (NotEqual env ann aa) else pure ann
        Nothing -> pure ann
  pure (Prod (Just ea) Nothing (Just et), et)

tcTerm env (Prod (Just a@(Pos _ (Def x _))) (Just b) mt) Nothing = do
  (ea,tyA) <- trace "prod2" $ inferType env a
  (eb,tyB) <- inferType (extendCtx (Sig x tyA) env) b
  let ann = Sigma Nothing (Just tyA) (Just tyB)
  et <- case mt of
        Just aa -> if not (equate env ann aa) then err (NotEqual env ann aa) else pure ann
        Nothing -> pure ann
  pure (Prod (Just ea) (Just eb) (Just et), et)

tcTerm env (Prod (Just a@(Pos _ (Def x _))) (Just b) mt) (Just (Whnf ann@(Sigma _ (Just aaa) (Just bbb)))) = do
  (ea,tyA) <- trace (toList $ "prod3" <> show b <> " : " <> show bbb) $ checkType env a aaa
  (eb,tyB) <- checkType (extendCtx (Sig x tyA) env) b bbb
  et <- case mt of
        Just aa -> if not (equate env ann aa) then err (NotEqual env ann aa) else pure ann
        Nothing -> pure ann
  pure (Prod (Just ea) (Just eb) (Just et), et)

--tcTerm t@(Pcase p bnd ann1) ann2 = err [DS "unimplemented"]
      
tcTerm env tm (Just (Whnf ty)) = case inferType env tm of
    Success (atm, ty') -> if (equate env ty' ty) then pure (atm, ty) else err $ TypesDontMatch env tm ty' ty
    Failure es -> with es $ err $ CouldNotInferType env tm
                         
tcTerm env tm tt = P.error $ toList $ show tm <> " " <> show tt
