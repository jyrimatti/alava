{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module TypeCheck where

import Foundation (($),(.),Maybe(Just,Nothing),pure,fromMaybe,(<>))
import Foundation.Collection (reverse)

import Prelude (Show)
import qualified Prelude as P (show,error)

import GHC.Stack (HasCallStack)

import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger.CallStack as LOG (logDebug)
import Control.Monad.Except (withExceptT,throwError)
import Control.Monad (foldM)

import Data.Text.Lazy (Text,pack,unpack,toStrict)

import Syntax (Type,Term(Var,Type,Pi,Lam,App,Ann,Pos,Paren,Let,Sig,Def,Comment,Sigma,Prod),AnnType(Inferred))
import Environment (Env,lookupTy,extendCtx,extendSourceLocation)
import Equal (ResultM,Whnf(Whnf),whnf,equate,ensurePi)
import Substitution (subst)
import Error (err, Error(NotInScope,NotEqual,LambdaMustHaveFunctionType,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedType))

show :: Show a => a -> Text
show = pack . P.show

logDebug :: (MonadLogger m) => Text -> ResultM m ()
logDebug = LOG.logDebug . toStrict 

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Environment (emptyEnv)

inferType :: (MonadLogger m, HasCallStack) => Env -> Term -> ResultM m (Term,Type)
inferType env term = do
  logDebug $ "inferType " <> show term
  tcTerm env term Nothing

checkType :: (MonadLogger m, HasCallStack) => Env -> Term -> Type -> ResultM m (Term,Type)
checkType env term expectedType = do
  logDebug $ "checkType " <> show term <> " " <> show expectedType
  tcTerm env term $ Just $ whnf env expectedType

tcType :: (MonadLogger m, HasCallStack) => Env -> Term -> ResultM m Term
tcType env tm = withExceptT ((err $ ExpectedType env tm) <>) $ do
  logDebug $ "tcType " <> show tm
  (t,_) <- checkType env tm Type
  pure t

logRet :: MonadLogger m => Text -> Term -> Type -> ResultM m (Term, Type)
logRet str eTerm eType = do
    logDebug $ "tcTerm " <> str <> ": returning elaborated term:\n  " <> show eTerm <> "\nand elaborated type:\n  " <> show eType
    pure (eTerm, eType)

tcTerm :: (MonadLogger m, HasCallStack) => Env -> Term -> Maybe Whnf -> ResultM m (Term,Type)

tcTerm env t@(Var x) Nothing = do
  logDebug $ "tcTerm " <> show t
  case lookupTy env x of
    --Just tyx -> logRet "Var" (Ann t tyx Inferred) tyx
    Just tyx -> logRet "Var" t tyx
    Nothing -> throwError $ err $ NotInScope env x

--tcTerm env t@(Ann term _ Inferred) Nothing = inferType env term
--tcTerm env t@(Ann term _ Inferred) (Just (Whnf ann)) = checkType env term ann


tcTerm _ t@Type Nothing = do
  logDebug $ "tcTerm " <> show t
  logRet "Type" t Type

tcTerm env t@(Pi x tyA tyB) Nothing = do
  logDebug $ "tcTerm " <> show t
  atyA <- tcType env tyA
  atyB <- tcType (case x of Just xx -> extendCtx (Sig xx atyA) env; Nothing -> env) tyB
  logRet "Pi" (Pi x atyA atyB) Type
      
-- Check the type of a function    
tcTerm env t@(Lam x ma body) a@(Just (Whnf p@(Pi _ tyA tyB))) = do
    logDebug $ "tcTerm " <> show t <> " " <> show a
    -- check tyA matches type annotation on binder, if present
    ea <- case ma of
        Just aa  -> do
          res <- equate env tyA aa
          if res then pure tyA
                 else throwError $ err $ NotEqual env tyA aa
        Nothing -> pure tyA
    -- check the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x ea) env
    (ebody, _) <- checkType newEnv body tyB
    logRet "Lam1" (Lam x (Just $ Ann (fromMaybe (Comment "") ma) ea Inferred) ebody) p

--tcTerm env t@(Lam "_" s1 s2) (Just (Whnf Type)) = do
--  todo: --pure (t, Type)

tcTerm env t@Lam{} (Just (Whnf nf)) = throwError $ err $ LambdaMustHaveFunctionType env t nf

-- infer the type of a lambda expression, when an annotation
-- on the binder is present
tcTerm env t@(Lam x (Just annot) body) Nothing = do
    logDebug $ "tcTerm " <> show t
    -- check that the type annotation is well-formed
    atyA          <- tcType env annot
    -- infer the type of the body of the lambda expression
    let newEnv     = extendCtx (Sig x atyA) env
    (ebody, atyB) <- inferType newEnv body
    logRet "Lam2" (Lam x (Just $ Ann annot atyA Inferred) ebody) (Pi (Just x) atyA atyB)

tcTerm env t@(App t1 t2) Nothing = do
    logDebug $ "tcTerm App: " <> show t
    (at1, ty1)      <- inferType env t1
    (mname, argType, resType) <- ensurePi env ty1
    logDebug $ "Checking if application parameter (" <> show t2 <> ") matches function argument type (" <> show argType <> ")"
    (at2, _) <- withExceptT ((<>) $ err $ AppTypesDontMatch env t argType t2) $ checkType env t2 argType
    let _      = extendCtx (Def (fromMaybe "_" mname) at2) $ extendCtx (Sig (fromMaybe "_" mname) argType) env
    logRet "App" (App at1 at2) $ subst mname at2 resType

tcTerm env t@(Ann tm ty _) Nothing = do
    logDebug $ "tcTerm " <> show t
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
  logDebug $ "tcTerm " <> show t <> " " <> show ann
  let 
    foo a          (Pos _ b) = foo a b
    --foo a          (Sig n (Pos _ ty)) = foo a (Sig n ty)
    foo (e,exs)    (Sig n ty) = do
        (ety,_)    <- inferType e ty
        let newEnv = extendCtx (Sig n ety) e
        pure (newEnv, Sig n ety : exs)
    --foo a          (Def n (Pos _ x)) = foo a (Def n x)
    foo (e,exs)    (Def name x) = case lookupTy e name of
        Nothing -> throwError $ err $ NotInScope e name
        Just ty -> do
                    (et2, _) <- checkType e x ty
                    let newEnv2 = extendCtx (Def name et2) e
                    pure (newEnv2, Def name et2 : exs)
  (newEnv, exs)  <- foldM foo (env,[]) xs
  (ebody, etype) <- tcTerm newEnv body ann
  logRet "Let" (Let (reverse exs) ebody) etype
  
             
           
  
tcTerm _ s@(Sig _ t) a = do
  logDebug $ "tcTerm " <> show s <> " " <> show a
  logRet "Sig" s t

--tcTerm env t@(Def name body) a@(Just (Whnf (Sigma _ (Just ann) Nothing))) = do
--   logDebug $ "tcTerm " <> show t <> show a
--   (ebody, etype) <- checkType env body ann
--   logRet "Def1" (Def name ebody) etype

tcTerm env t@(Def name body) ann = do
   logDebug $ "tcTerm " <> show t <> show ann
   (ebody, etype) <- tcTerm env body ann
   logRet "Def2" (Def name ebody) etype
      

    
tcTerm env t@(Sigma (Just x) (Just tyA) (Just tyB)) Nothing = do        
  logDebug $ "tcTerm " <> show t
  atyA <- tcType env tyA
  atyB <- tcType (extendCtx (Sig x atyA) env) tyB
  logRet "Sigma1" (Sigma (Just x) (Just atyA) (Just atyB)) Type

tcTerm env t@(Sigma (Just x) (Just tyA) Nothing) Nothing = do        
  logDebug $ "tcTerm " <> show t
  atyA <- tcType env tyA
  logRet "Sigma2" (Sigma (Just x) (Just atyA) Nothing) Type

tcTerm _ t@(Sigma Nothing Nothing Nothing) Nothing = do
  logDebug $ "tcTerm " <> show t
  logRet "Sigma3" t Type

tcTerm env t@(Prod (Just a) b) ann@(Just (Whnf w@(Sigma _ (Just aa) bb))) = do
  logDebug $ "tcTerm " <> show t <> show ann
  (ea,tyA) <- checkType env a aa
  (meb, mtyB) <- case (b,bb) of
      (Just c, Just cc)  -> do
        (eb, tyB) <- checkType env c cc
        pure (Just eb, Just tyB)
      (Nothing, Nothing) -> pure (Nothing,Nothing)
      _                  -> throwError $ err $ NotEqual env t w
  logRet "Prod5" (Prod (Just ea) meb) (Sigma Nothing (Just tyA) mtyB)
      
tcTerm env tm a@(Just (Whnf ty)) = do
  logDebug $ "tcTerm " <> show tm <> show a
  withExceptT ((<>) $ err $ CouldNotInferType env tm) $ do
    (atm, ty') <- inferType env tm
    res <- equate env ty' ty
    if res then logRet "_" atm ty
           else throwError $ err $ TypesDontMatch env tm ty' ty
                         
tcTerm _ tm tt = P.error $ unpack $ "(" <> show tm <> ") (" <> show tt <> ")"
