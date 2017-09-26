{-# LANGUAGE NoImplicitPrelude, ViewPatterns, TupleSections, OverloadedStrings, DeriveAnyClass #-}
module TypeCheck where

import Foundation (($),Maybe(Just,Nothing),fst,(<$>),pure,not,fromMaybe,(<>),show,toList,error,(==))
import Foundation.Collection (reverse)
import Control.Monad (foldM)

import GHC.Stack (HasCallStack)
import Debug.Trace(trace)
import Control.Monad.Logger.CallStack
import Control.Monad.Except

import Data.Text (pack)

import qualified Prelude as P

import Syntax (Type,Term(Var,Type,Pi,Lam,App,Ann,Pos,Paren,Let,Sig,Def,Comment,Sigma,Prod),AnnType(Inferred))
import Environment (Env,lookupTy,extendCtx,updateCtx,extendSourceLocation,emptyEnv)
import Equal (subst,ResultM,Whnf(Whnf),whnf,err,Error(NotInScope,NotEqual,LambdaMustHaveFunctionType,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedType),equate,ensurePi)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment (emptyEnv)

inferType :: HasCallStack => Env -> Term -> ResultM (Term,Type)
inferType env term = do
  logDebug $ pack $ toList $ "inferType " <> show term
  tcTerm env term Nothing

checkType :: HasCallStack => Env -> Term -> Type -> ResultM (Term,Type)
checkType env term expectedType = do
  logDebug $ pack $ toList $ "checkType " <> show term <> " " <> show expectedType
  tcTerm env term $ Just $ whnf env expectedType

tcType :: HasCallStack => Env -> Term -> ResultM Term
tcType env tm = withExceptT ((<>) (err $ ExpectedType env tm)) $ do
  logDebug $ pack $ toList $ "tcType " <> show tm
  (t,_) <- checkType env tm Type
  pure t

logRet str eTerm eType = do
    logDebug $ pack $ toList $ "tcTerm " <> str <> ": returning elaborated term:\n  " <> show eTerm <> "\nand elaborated type:\n  " <> show eType
    pure (eTerm, eType)

tcTerm :: HasCallStack => Env -> Term -> Maybe Whnf -> ResultM (Term,Type)

tcTerm env t@(Var x) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  case lookupTy env x of
    --Just tyx -> logRet "Var" (Ann t tyx Inferred) tyx
    Just tyx -> logRet "Var" t tyx
    Nothing -> throwError $ err $ NotInScope env x

--tcTerm env t@(Ann term _ Inferred) Nothing = inferType env term
--tcTerm env t@(Ann term _ Inferred) (Just (Whnf ann)) = checkType env term ann


tcTerm _ t@(Type) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  logRet "Type" t Type

tcTerm env t@(Pi x tyA tyB) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  atyA <- tcType env tyA
  atyB <- tcType (case x of Just xx -> extendCtx (Sig xx atyA) env; Nothing -> env) tyB
  logRet "Pi" (Pi x atyA atyB) Type
      
-- Check the type of a function    
tcTerm env t@(Lam x ma body) a@(Just (Whnf p@(Pi _ tyA tyB))) = do
    logDebug $ pack $ toList $ "tcTerm " <> show t <> " " <> show a
    -- check tyA matches type annotation on binder, if present
    ea <- case ma of
        Just a  -> do
          res <- equate env tyA a
          if res then pure tyA
                 else throwError $ err $ NotEqual env tyA a
        Nothing -> pure tyA
    -- check the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x ea) env
    (ebody, _) <- checkType newEnv body tyB
    logRet "Lam1" (Lam x (Just $ Ann (fromMaybe (Comment "") ma) ea Inferred) ebody) p

--tcTerm env t@(Lam "_" s1 s2) (Just (Whnf Type)) = do
--  todo: --pure (t, Type)

tcTerm env t@(Lam _ _ _) (Just (Whnf nf)) = throwError $ err $ LambdaMustHaveFunctionType env t nf

-- infer the type of a lambda expression, when an annotation
-- on the binder is present
tcTerm env t@(Lam x (Just annot) body) Nothing = do
    logDebug $ pack $ toList $ "tcTerm " <> show t
    -- check that the type annotation is well-formed
    atyA          <- tcType env annot
    -- infer the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x atyA) env
    (ebody, atyB) <- inferType newEnv body
    logRet "Lam2" (Lam x (Just $ Ann annot atyA Inferred) ebody) (Pi (Just x) atyA atyB)

tcTerm env t@(App t1 t2) Nothing = do
    logDebug $ pack $ toList $ "tcTerm App: " <> show t
    (at1, ty1)      <- inferType env t1
    (mname, argType, resType) <- ensurePi env ty1
    logDebug $ pack $ toList $ "Checking if application parameter (" <> show t2 <> ") matches function argument type (" <> show argType <> ")"
    (at2, _) <- withExceptT ((<>) $ err $ AppTypesDontMatch env t argType t2) $ checkType env t2 argType
    let newEnv      = extendCtx (Def (fromMaybe "_" mname) $ at2) $ extendCtx (Sig (fromMaybe "_" mname) $ argType) env
    logRet "App" (App at1 at2) $ subst mname at2 resType

tcTerm env t@(Ann tm ty _) Nothing = do
    logDebug $ pack $ toList $ "tcTerm " <> show t
    ty'         <- tcType env ty
    (tm', ty'') <- checkType env tm ty'
    logRet "Ann" tm' ty''
  
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
  
tcTerm env t@(Let xs body) ann = do
  logDebug $ pack $ toList $ "tcTerm " <> show t <> " " <> show ann
  let 
    foo a          (Pos _ b) = foo a b
    --foo a          (Sig n (Pos _ ty)) = foo a (Sig n ty)
    foo (e,exs)    (Sig n ty) = do
        (ety,_)    <- inferType e ty
        let newEnv = extendCtx (Sig n ety) e
        pure (newEnv, (Sig n ety):exs)
    --foo a          (Def n (Pos _ x)) = foo a (Def n x)
    foo (e,exs)    (Def name x) = case lookupTy e name of
        Nothing -> throwError $ err $ NotInScope e name
        Just ty -> do
                    (et2, ety2) <- checkType e x ty
                    let newEnv2 = extendCtx (Def name et2) e
                    pure (newEnv2, (Def name et2):exs)
  (newEnv, exs)  <- foldM foo (env,[]) xs
  (ebody, etype) <- tcTerm newEnv body ann
  logRet "Let" (Let (reverse exs) ebody) etype
  
             
           
  
tcTerm _ s@(Sig _ t) a = do
  logDebug $ pack $ toList $ "tcTerm " <> show s <> " " <> show a
  logRet "Sig" s t

--tcTerm env t@(Def name body) a@(Just (Whnf (Sigma _ (Just ann) Nothing))) = do
--   logDebug $ pack $ toList $ "tcTerm " <> show t <> show a
--   (ebody, etype) <- checkType env body ann
--   logRet "Def1" (Def name ebody) etype

tcTerm env t@(Def name body) ann = do
   logDebug $ pack $ toList $ "tcTerm " <> show t <> show ann
   (ebody, etype) <- tcTerm env body ann
   logRet "Def2" (Def name ebody) etype
      

    
tcTerm env t@(Sigma (Just x) (Just tyA) (Just tyB)) Nothing = do        
  logDebug $ pack $ toList $ "tcTerm " <> show t
  atyA <- tcType env tyA
  atyB <- tcType (extendCtx (Sig x atyA) env) tyB
  logRet "Sigma1" (Sigma (Just x) (Just atyA) (Just atyB)) Type

tcTerm env t@(Sigma (Just x) (Just tyA) Nothing) Nothing = do        
  logDebug $ pack $ toList $ "tcTerm " <> show t
  atyA <- tcType env tyA
  logRet "Sigma2" (Sigma (Just x) (Just atyA) Nothing) Type

tcTerm env t@(Sigma Nothing Nothing Nothing) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  logRet "Sigma3" t Type

tcTerm env t@(Prod (Just a) b) ann@(Just (Whnf w@(Sigma _ (Just aa) bb))) = do
  logDebug $ pack $ toList $ "tcTerm " <> show t <> show ann
  (ea,tyA) <- checkType env a aa
  (meb, mtyB) <- case (b,bb) of
      (Just b, Just bb)  -> do
        (eb, tyB) <- checkType env b bb
        pure (Just eb, Just tyB)
      (Nothing, Nothing) -> pure (Nothing,Nothing)
      _                  -> throwError $ err $ NotEqual env t w
  logRet "Prod5" (Prod (Just ea) meb) (Sigma Nothing (Just tyA) mtyB)

{-
tcTerm env t@(Prod Nothing Nothing Nothing) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  logRet "Prod1" (Prod Nothing Nothing (Just (Sigma Nothing Nothing Nothing))) (Sigma Nothing Nothing Nothing)


tcTerm env t@(Prod (Just a) Nothing mt) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t
  (ea,tyA) <- inferType env a
  let ann = Sigma Nothing (Just tyA) Nothing
  et <- case mt of
        Just aa -> do
          res <- equate env ann aa 
          if res then pure ann
                 else throwError $ err $ NotEqual env ann aa
        Nothing -> pure ann
  logRet "Prod2" (Prod (Just ea) Nothing (Just et)) et

tcTerm env t@(Prod (Just a) Nothing mt) aa@(Just (Whnf ann@(Sigma _ (Just aaa) Nothing))) = do
  logDebug $ pack $ toList $ "tcTerm " <> show t <> show aa
  (ea,tyA) <- checkType env a aaa
  et <- case mt of
        Just aa -> do
          res <- equate env ann aa
          if res then pure ann
                 else throwError $ err $ NotEqual env ann aa
        Nothing -> pure ann
  logRet "Prod3" (Prod (Just ea) Nothing (Just et)) et

tcTerm env t@(Prod (Just a@(Pos _ (Def x _))) ann@(Just b) mt) Nothing = do
  logDebug $ pack $ toList $ "tcTerm " <> show t <> show ann
  (ea,tyA) <- inferType env a
  (eb,tyB) <- inferType (extendCtx (Sig x tyA) env) b
  let ann = Sigma Nothing (Just tyA) (Just tyB)
  et <- case mt of
        Just aa -> do
          res <- equate env ann aa
          if res then pure ann
                 else throwError $ err $ NotEqual env ann aa
        Nothing -> pure ann
  logRet "Prod4" (Prod (Just ea) (Just eb) (Just et)) et

tcTerm env t@(Prod (Just a@(Pos _ (Def x _))) (Just b) mt) aa@(Just (Whnf ann@(Sigma _ (Just aaa) (Just bbb)))) = do
  logDebug $ pack $ toList $ "tcTerm " <> show t <> show aa
  (ea,tyA) <- checkType env a aaa
  (eb,tyB) <- checkType (extendCtx (Sig x tyA) env) b bbb
  et <- case mt of
        Just aa -> do
          res <- equate env ann aa
          if res then pure ann
                 else throwError $ err $ NotEqual env ann aa
        Nothing -> pure ann
  logRet "Prod5" (Prod (Just ea) (Just eb) (Just et)) et
  -}

--tcTerm t@(Pcase p bnd ann1) ann2 = err [DS "unimplemented"]
      
tcTerm env tm a@(Just (Whnf ty)) = do
  logDebug $ pack $ toList $ "tcTerm " <> show tm <> show a
  withExceptT ((<>) $ err $ CouldNotInferType env tm) $ do
    (atm, ty') <- inferType env tm
    res <- equate env ty' ty
    if res then logRet "_" atm ty
           else throwError $ err $ TypesDontMatch env tm ty' ty
                         
tcTerm env tm tt = P.error $ toList $ "(" <> show tm <> ") (" <> show tt <> ")"
