{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module TypeCheck where

import Foundation (($),(.),Maybe(Just,Nothing),pure,fromMaybe,(<>))
import Foundation.Collection (reverse)

import Prelude (Show)
import qualified Prelude as P (show,error)

import GHC.Stack (HasCallStack)

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logDebug)
import Control.Monad.Except (withExceptT,throwError)
import Control.Monad (foldM)

import Data.Text (Text,pack,unpack)

import Syntax (Type,EType,Term(Var,Type,Pi,Lam,App,Ann,Pos,Paren,Let,Sig,Def,Comment,Sigma,Prod),AnnType(Inferred),ETerm(EVar,EType,EPi,ELam,EApp,EAnn,ELet,ESig,EDef,ESigma,EProd))
import Environment (Env,lookupTy,extendCtxSig,extendCtxDef,extendSourceLocation)
import Equal (ResultM,Whnf(Whnf),whnf,equate,ensurePi)
import Substitution (subst)
import Error (err, Error(NotInScope,NotEqual,LambdaMustHaveFunctionType,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedType,MustAnnotateLambda))

show :: Show a => a -> Text
show = pack . P.show


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment (emptyEnv)

inferType :: (MonadLogger m, HasCallStack) => Env -> Term -> ResultM m (ETerm,EType)
inferType env term = do
  logDebug $ "inferType: " <> show term
  tcTerm env term Nothing

checkType :: (MonadLogger m, HasCallStack) => Env -> Term -> EType -> ResultM m (ETerm,EType)
checkType env term expectedType = do
  logDebug $ "checkType: " <> show term <> "\n                   " <> show expectedType
  tcTerm env term $ Just $ whnf env expectedType

tcType :: (MonadLogger m, HasCallStack) => Env -> Term -> ResultM m (ETerm,EType)
tcType env tm = withExceptT ((err $ ExpectedType env tm) <>) $ do
  logDebug $ "tcType:    " <> show tm
  checkType env tm EType

logRet :: MonadLogger m => Text -> ETerm -> EType -> ResultM m (ETerm, EType)
logRet str eTerm eType = do
    logDebug $ "tcTerm " <> str <> ": returning elaborated term and type:\n  " <> show eTerm <> "\n  " <> show eType
    pure (eTerm, eType)

tcTerm :: (MonadLogger m, HasCallStack) => Env -> Term -> Maybe Whnf -> ResultM m (ETerm,EType)

tcTerm env t@(Var x) Nothing = do
  logDebug $ "tcTerm:    " <> show t
  case lookupTy env x of
    --Just tyx -> logRet "Var" (Ann t tyx Inferred) tyx
    Just tyx -> logRet "Var" (EVar t x tyx) tyx
    Nothing -> throwError $ err $ NotInScope env x

--tcTerm env t@(Ann term _ Inferred) Nothing = inferType env term
--tcTerm env t@(Ann term _ Inferred) (Just (Whnf ann)) = checkType env term ann


tcTerm _ t@Type Nothing = do
  logDebug $ "tcTerm:    " <> show t
  logRet "Type" EType EType

tcTerm env t@(Pi x tyA tyB) Nothing = do
  logDebug $ "tcTerm:    " <> show t
  (atyA,_) <- tcType env tyA
  (atyB,_) <- tcType (case x of Just xx -> extendCtxSig xx atyA env; Nothing -> env) tyB
  logRet "Pi" (EPi t x atyA atyB) EType
      
-- Check the type of a function
tcTerm env t@(Lam x ma body) a@(Just (Whnf p@(EPi _ _ tyA tyB))) = do
    logDebug $ "tcTerm:    " <> show t <> "\n                   " <> show a
    -- check tyA matches type annotation on binder, if present
    ea <- case ma of
        Just aa  -> do
          (aat,_) <- tcType env aa
          res <- equate env tyA aat
          if res then pure tyA
                 else throwError $ err $ NotEqual env tyA aa
        Nothing -> pure tyA
    -- check the type of the body of the lambda expression
    let newEnv = extendCtxSig x ea env    
    (ebody, tbody) <- checkType newEnv body tyB
    logRet "Lam1" (ELam t x ea ebody tbody) p

--tcTerm env t@(Lam "_" s1 s2) (Just (Whnf Type)) = do
--  todo: --pure (t, Type)

tcTerm env t@Lam{} (Just (Whnf nf)) = throwError $ err $ LambdaMustHaveFunctionType env t nf

tcTerm env t@(Lam _ Nothing _) Nothing = throwError $ err $ MustAnnotateLambda env t

-- infer the type of a lambda expression, when an annotation
-- on the binder is present
tcTerm env t@(Lam x (Just annot) body) Nothing = do
    logDebug $ "tcTerm:    " <> show t
    -- check that the type annotation is well-formed
    (atyA,_)          <- tcType env annot
    -- infer the type of the body of the lambda expression
    let newEnv     = extendCtxSig x atyA env
    (ebody, atyB) <- inferType newEnv body
    logRet "Lam2" (ELam t x atyA ebody atyB) (EPi Type (Just x) atyA atyB)

tcTerm env t@(App t1 t2) Nothing = do
    logDebug $ "tcTerm:    " <> show t
    (at1, ty1)      <- inferType env t1
    (mname, argType, resType) <- ensurePi env ty1
    logDebug $ "Checking if application parameter (" <> show t2 <> ") matches function argument type (" <> show argType <> ")"
    (at2, tt2) <- withExceptT ((<>) $ err $ AppTypesDontMatch env t argType t2) $ checkType env t2 argType
    let ttt = subst mname at2 resType
    logRet "App" (EApp t at1 at2 ttt) ttt

tcTerm env t@(Ann tm ty _) Nothing = do
    logDebug $ "tcTerm:    " <> show t
    (ty',_)     <- tcType env ty
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
  logDebug $ "tcTerm:    " <> show t <> "\n                   " <> show ann
  let 
    foo a          (Pos _ b) = foo a b
    --foo a          (Sig n (Pos _ ty)) = foo a (Sig n ty)
    foo (e,exs)    (Sig n ty) = do
        (ety,_)    <- inferType e ty
        let newEnv = extendCtxSig n ety e
        pure (newEnv, ESig Type n ety : exs)
    --foo a          (Def n (Pos _ x)) = foo a (Def n x)
    foo (e,exs)    d@(Def name x) = case lookupTy e name of
        Nothing -> throwError $ err $ NotInScope e name
        Just ty -> do
                    (et2, tt2) <- checkType e x ty
                    let newEnv2 = extendCtxDef name et2 e
                    pure (newEnv2, EDef Type name et2 tt2 : exs)
  (newEnv, exs)  <- foldM foo (env,[]) xs
  (ebody, etype) <- tcTerm newEnv body ann
  logRet "Let" (ELet t (reverse exs) ebody) etype
  
             
           
  
tcTerm env s@(Sig x t) a = do
  logDebug $ "tcTerm:    " <> show s <> "\n                   " <> show a
  (ee, _) <- inferType env t
  logRet "Sig" (ESig s x ee) ee

--tcTerm env t@(Def name body) a@(Just (Whnf (Sigma _ (Just ann) Nothing))) = do
--   logDebug $ "tcTerm " <> show t <> show a
--   (ebody, etype) <- checkType env body ann
--   logRet "Def1" (Def name ebody) etype

tcTerm env t@(Def name body) ann = do
   logDebug $ "tcTerm:    " <> show t <> "\n                   " <> show ann
   (ebody, etype) <- tcTerm env body ann
   logRet "Def2" (EDef t name ebody etype) etype
      

    
tcTerm env t@(Sigma (Just x) (Just tyA) (Just tyB)) Nothing = do        
  logDebug $ "tcTerm:    " <> show t
  (atyA,_) <- tcType env tyA
  (atyB,_) <- tcType (extendCtxSig x atyA env) tyB
  logRet "Sigma1" (ESigma t (Just atyA) (Just atyB) EType) EType

tcTerm env t@(Sigma (Just _) (Just tyA) Nothing) Nothing = do        
  logDebug $ "tcTerm:    " <> show t
  (atyA,_) <- tcType env tyA
  logRet "Sigma2" (ESigma t (Just atyA) Nothing EType) EType

tcTerm _ t@(Sigma Nothing Nothing Nothing) Nothing = do
  logDebug $ "tcTerm:    " <> show t
  logRet "Sigma3" (ESigma t Nothing Nothing EType) EType

tcTerm _ t@(Prod Nothing Nothing) Nothing = do
  logDebug $ "tcTerm:    " <> show t
  let etype = ESigma Type Nothing Nothing EType
  logRet "Prod" (EProd t Nothing Nothing etype) etype

tcTerm env t@(Prod (Just a) b) Nothing = do
  logDebug $ "tcTerm:    " <> show t
  (ea,tyA) <- inferType env a
  (meb, mtyB) <- case b of
    Nothing -> pure (Nothing, Nothing)
    Just c -> do
      (mec,tyC) <- inferType env c
      pure (Just mec, Just tyC)
  let etype = ESigma Type (Just tyA) mtyB EType
  logRet "Prod" (EProd t (Just ea) meb etype) etype

tcTerm env t@(Prod (Just a) b) ann@(Just (Whnf w@(ESigma _ (Just aa) bb _))) = do
  logDebug $ "tcTerm:    " <> show t <> "\n                   " <> show ann
  (ea,tyA) <- checkType env a aa
  (meb, mtyB) <- case (b,bb) of
      (Just c, Just cc)  -> do
        (eb, tyB) <- checkType env c cc
        pure (Just eb, Just tyB)
      (Nothing, Nothing) -> pure (Nothing,Nothing)
      _                  -> throwError $ err $ NotEqual env w t
  let etype = ESigma Type (Just tyA) mtyB EType
  logRet "Prod" (EProd t (Just ea) meb etype) etype
      
tcTerm env tm a@(Just (Whnf ty)) = do
  logDebug $ "tcTerm:    " <> show tm <> "\n                   " <> show a
  withExceptT ((<>) $ err $ CouldNotInferType env tm) $ do
    (atm, ty') <- inferType env tm
    res <- equate env ty' ty
    if res then logRet "_" atm ty
           else throwError $ err $ TypesDontMatch env tm ty' ty
                         
tcTerm _ tm tt = P.error $ unpack $ "(" <> show tm <> ") (" <> show tt <> ")"
