{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module TypeCheck where

import Foundation (($),(.),Maybe(Just,Nothing),pure,(<>))
import Foundation.Collection (reverse)

import Prelude (Show)
import qualified Prelude as P (show,error)

import GHC.Stack (HasCallStack)

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logDebug)
import Control.Monad.Except (withExceptT)
import Control.Monad (foldM)

import Data.Text (Text,pack,unpack)
import Data.Maybe (maybe)

import Syntax (EType,Term(Var,Type,Pi,Lam,App,Ann,Pos,Paren,Let,Sig,Def,Sigma,Prod),ETerm(EVar,EType,EPi,ELam,EApp,ELet,ESig,EDef,ESigma,EProd,EAnn,ECase))
import Environment (Env,lookupSig,extendCtxSig,extendCtxDef,extendSourceLocation)
import Equal (Whnf(Whnf),whnf,equate,ensurePi)
import Substitution (subst)
import Error (ResultM,throwErr,err,Error(NotInScope,NotEqual,LambdaMustHaveFunctionType,TypesDontMatch,AppTypesDontMatch,CouldNotInferType,ExpectedType,MustAnnotateLambda))

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
tcType env term = withExceptT ((err $ ExpectedType env term) <>) $ do
  logDebug $ "tcType:    " <> show term
  checkType env term EType

logAndReturn :: MonadLogger m => ETerm -> EType -> ResultM m (ETerm, EType)
logAndReturn eTerm eType = do
    logDebug $ "tcTerm " <> typeName eTerm <> ": returning elaborated term and type:\n  " <> show eTerm <> "\n  " <> show eType
    pure (eTerm, eType)
  where typeName eTerm = case eTerm of
          EType{}  -> "Type"
          EVar{}   -> "Var"
          ELam{}   -> "Lam"
          EApp{}   -> "App"
          EPi{}    -> "Pi"
          EAnn{}   -> "Ann"
          ELet{}   -> "Let"
          ESig{}   -> "Sig"
          EDef{}   -> "Def"
          ESigma{} -> "Sigma"
          EProd{}  -> "Prod"
          ECase{}  -> "Case"

tcTerm :: (MonadLogger m, HasCallStack) => Env -> Term -> Maybe Whnf -> ResultM m (ETerm,EType)

tcTerm env term@(Var name) Nothing = do
  logDebug $ "tcTerm:    " <> show term
  case lookupSig env name of
    Just sig -> logAndReturn (EVar term sig) sig
    Nothing -> throwErr $ NotInScope env name

tcTerm _ term@Type Nothing = do
  logDebug $ "tcTerm:    " <> show term
  logAndReturn EType EType

tcTerm env term@(Pi mname a b) Nothing = do
  logDebug $ "tcTerm:    " <> show term
  (eTermA,_) <- tcType env a
  (eTermB,_) <- tcType (maybe env (\name -> extendCtxSig name eTermA env) mname) b
  logAndReturn (EPi term eTermA eTermB) EType
      
tcTerm env term@(Lam name mtype body) w@(Just (Whnf expected@(EPi _ a b))) = do
    logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show w
    aa <- case mtype of
        Just typeAnnot -> do
          (eTerm,_) <- tcType env typeAnnot
          res <- equate env a eTerm
          if res then pure a
                 else throwErr $ NotEqual env a typeAnnot
        Nothing -> pure a
    (eBody, tBody) <- checkType (extendCtxSig name aa env) body b
    logAndReturn (ELam term aa eBody tBody) expected

tcTerm env term@Lam{} (Just (Whnf expected)) = throwErr $ LambdaMustHaveFunctionType env term expected

tcTerm env term@(Lam _ Nothing _) Nothing = throwErr $ MustAnnotateLambda env term

tcTerm env term@(Lam name (Just typeAnnot) body) Nothing = do
    logDebug $ "tcTerm:    " <> show term
    (eTerm,_) <- tcType env typeAnnot
    (eBody, tBody) <- inferType (extendCtxSig name eTerm env) body
    logAndReturn (ELam term eTerm eBody tBody) (EPi Type eTerm tBody)

tcTerm env term@(App f arg) Nothing = do
    logDebug $ "tcTerm:    " <> show term
    (eF, tF) <- inferType env f
    (mname, eA, eB) <- ensurePi env tF
    logDebug $ "Checking if application parameter (" <> show arg <> ") matches function argument type (" <> show eA <> ")"
    (eArg, _) <- withExceptT ((<>) $ err $ AppTypesDontMatch env term eA arg) $ checkType env arg eA
    let substitutedB = subst mname eArg eB
    logAndReturn (EApp term eF eArg substitutedB) substitutedB

tcTerm env term@(Ann annotatedTerm typeAnnot) Nothing = do
    logDebug $ "tcTerm:    " <> show term
    (eTypeAnnot,_)     <- tcType env typeAnnot
    (eAnnotatedTerm, tAnnotatedTerm) <- checkType env annotatedTerm eTypeAnnot
    logAndReturn eAnnotatedTerm tAnnotatedTerm
  
tcTerm env (Pos sourcePos term) expected = tcTerm (extendSourceLocation sourcePos env) term expected
  
tcTerm env (Paren term) expected = tcTerm env term expected
  
tcTerm env term@(Let exprs body) expected = do
  logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show expected
  let 
    tcLetExpr :: MonadLogger m => (Env, [ETerm]) -> Term -> ResultM m (Env,[ETerm])
    
    tcLetExpr a                 (Pos _ trm)        = tcLetExpr a trm
    
    tcLetExpr (env,eExprs) orig@(Sig name typeSig) = do
          (eTypeSig,_) <- inferType env typeSig
          pure (extendCtxSig name eTypeSig env, ESig orig eTypeSig : eExprs)
    
    tcLetExpr (env,eExprs) orig@(Def name trm) = case lookupSig env name of
        Nothing -> throwErr $ NotInScope env name
        Just typeSig -> do
          (eTerm, tTerm) <- checkType env trm typeSig
          pure (extendCtxDef name eTerm env, EDef orig eTerm tTerm : eExprs)

  (newEnv, eExprs)  <- foldM tcLetExpr (env,[]) exprs
  (eBody, tBody) <- tcTerm newEnv body expected
  logAndReturn (ELet term (reverse eExprs) eBody tBody) tBody
  
             
           
  
tcTerm env term@(Sig _ typeSig) expected = do
  logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show expected
  (eTypeSig, _) <- inferType env typeSig
  logAndReturn (ESig term eTypeSig) eTypeSig

tcTerm env term@(Def _ trm) expected = do
   logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show expected
   (eTrm, tTrm) <- tcTerm env trm expected
   logAndReturn (EDef term eTrm tTrm) tTrm
      

    
tcTerm env term@(Sigma (Just name) (Just aType) (Just bType)) Nothing = do        
  logDebug $ "tcTerm:    " <> show term
  (eAType,_) <- tcType env aType
  (eBType,_) <- tcType (extendCtxSig name eAType env) bType
  logAndReturn (ESigma term (Just eAType) (Just eBType)) EType

tcTerm env term@(Sigma _ (Just aType) Nothing) Nothing = do        
  logDebug $ "tcTerm:    " <> show term
  (eAType,_) <- tcType env aType
  logAndReturn (ESigma term (Just eAType) Nothing) EType

tcTerm _ term@(Sigma Nothing Nothing Nothing) Nothing = do
  logDebug $ "tcTerm:    " <> show term
  logAndReturn (ESigma term Nothing Nothing) EType

tcTerm _ term@(Prod Nothing Nothing) Nothing = do
  logDebug $ "tcTerm:    " <> show term
  let eType = ESigma (Sigma Nothing Nothing Nothing) Nothing Nothing
  logAndReturn (EProd term Nothing Nothing eType) eType

tcTerm env term@(Prod (Just a) mb) Nothing = do
  logDebug $ "tcTerm:    " <> show term
  (eA,tA) <- inferType env a
  (meB, mtB) <- case mb of
    Nothing -> pure (Nothing, Nothing)
    Just b -> do
      (eB,tB) <- inferType env b
      pure (Just eB, Just tB)
  let eType = ESigma Type (Just tA) mtB
  logAndReturn (EProd term (Just eA) meB eType) eType

tcTerm env term@(Prod (Just a) mb) (Just (Whnf expected@(ESigma _ (Just aType) mbType))) = do
  logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show expected
  (eA,tA) <- checkType env a aType
  (meB, mtB) <- case (mb,mbType) of
      (Just b, Just bType) -> do
         (eB, tB) <- checkType env b bType
         pure (Just eB, Just tB)
      (Nothing, Nothing) -> pure (Nothing,Nothing)
      _                  -> throwErr $ NotEqual env expected term
  let eType = ESigma Type (Just tA) mtB
  logAndReturn (EProd term (Just eA) meB eType) eType
      
tcTerm env term (Just (Whnf expected)) = do
  logDebug $ "tcTerm:    " <> show term <> "\n                   " <> show expected
  withExceptT ((<>) $ err $ CouldNotInferType env term) $ do
    (eTerm, tTerm) <- inferType env term
    res <- equate env tTerm expected
    if res then logAndReturn eTerm expected
           else throwErr $ TypesDontMatch env term expected tTerm
                         
tcTerm _ term expected = P.error $ unpack $ "(" <> show term <> ") (" <> show expected <> ")"
