{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists, DeriveFunctor, DeriveAnyClass #-}
module Equal where

import Foundation (($),Functor,fmap,pure,(<*>),(>>=),(<>),Applicative,Monad,String,Show,show,Monoid,Bool(True,False),(==),(&&),Either,Maybe(Just,Nothing),(<),(>),Ordering(LT,EQ,GT),error,toList,fromMaybe,IO)
import Foundation.Collection (sortBy, intercalate)

import qualified Prelude as P
import Debug.Trace(trace)
import Control.Monad.Logger.CallStack

import GHC.Stack (HasCallStack)

import Data.Foldable (all,foldl)
import Data.List (zip)
import Data.Text (pack)

import Control.Monad.Morph
import Control.Monad.Logger.CallStack
import Control.Monad.Except

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Paren,Let,Def,Sig,Sigma,Prod,Pos,Comment),SourcePos,Type,TName,AnnType(Inferred))
import Environment (Env,getSourceLocation,lookupDef,lookupTy,extendCtx)
import PrettyPrint (display)

unlines :: [String] -> P.String
unlines xs = toList $ intercalate "\n" xs

data Error = NotInScope Env String
           | NotEqual Env Type Type
           | LambdaMustHaveFunctionType Env Term Type
           | ExpectedFunctionType Env Term Type
           | ExpectedType Env Term
           | TypesDontMatch Env Term Type Type
           | AppTypesDontMatch Env Term Type Type
           | CouldNotInferType Env Term
           | RequiresTypeAnnotation Env Term

getEnv (NotInScope e _) = e
getEnv (NotEqual e _ _) = e
getEnv (LambdaMustHaveFunctionType e _ _) = e
getEnv (ExpectedFunctionType e _ _)= e
getEnv (ExpectedType e _)= e
getEnv (TypesDontMatch e _ _ _) = e
getEnv (AppTypesDontMatch e _ _ _) = e
getEnv (CouldNotInferType e _) = e
getEnv (RequiresTypeAnnotation e _) = e

instance Show Error where
  show (NotInScope env var) = unlines
    ["Not in scope:"
    ,"  " <> var
    ,"Env:"
    ,show env
    ]
  show (NotEqual env expected actual) = unlines
    ["Types don't match."
    ,"Expected:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual:"
    ,"  " <> display expected <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (LambdaMustHaveFunctionType env term t) = unlines
    ["A lambda:"
    ,"  " <> display term <> "   ... " <> show term
    ," was expected to have a function type, but instead had:"
    ,"  " <> display t <> "   ... " <> show t
    ,"Env:"
    ,show env
    ]
  show (ExpectedFunctionType env term typ) = unlines
    ["Expected a function type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"but instead was:"
    ,"  " <> display typ <> "   ... " <> show typ
    ,"Env:"
    ,show env
    ]
  show (ExpectedType env term) = unlines
    ["Expected Type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Env:"
    ,show env
    ]
  show (TypesDontMatch env term actual expected) = unlines
    ["Types don't match for term:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Expected:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual:"
    ,"  " <> display actual <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (AppTypesDontMatch env term expected actual) = unlines
    ["Application argument type doesn't match the type of the function in:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Expected argument type:"
    ,"  " <> display expected <> "   ... " <> show expected
    ,"Actual: parameter type"
    ,"  " <> display actual <> "   ... " <> show actual
    ,"Env:"
    ,show env
    ]
  show (CouldNotInferType env term) = unlines
    ["Could not infer type for:"
    ,"  " <> display term <> "   ... " <> show term
    ,"Env:"
    ,show env
    ]

--instance Functor Result where
  --fmap f (Success a) = Success (f a)
  --fmap _ (Failure es) = Failure es

--instance Applicative Result where
  --pure = Success
  --Success f <*> r = fmap f r
  --Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  --Failure es <*> _ = Failure es

--instance Monad Result where
  --Success a >>= f = f a
  --Failure es >>= _ = Failure es

type ResultM = ExceptT [(Error,SourcePos)] (LoggingT IO)

err :: Error -> [(Error,SourcePos)]
err e = [(e, getSourceLocation (getEnv e))]

--with :: [(Error, SourcePos)] -> ResultM t -> ResultM t
--with es rm = do
--  lift $ case rm of
--    (Failure e) -> Failure $ es <> e
--    _           -> error "Should not be here"

equate :: HasCallStack => Env -> Term -> Term -> ResultM Bool
equate env t1 t2 = do
  logDebug $ pack $ toList $ "Equating\n  " <> show t1 <> "\n  " <> show t2
  pure $ equate_ env t1 t2

equate_ :: HasCallStack => Env -> Term -> Term -> Bool
equate_ env t1 t2 = let
  n1 = trace (toList $ "equate_:" <> show t1 <> " " <> show t2) $ whnf' env False t1  
  n2 = whnf' env False t2
  o (Def x1 _) (Def x2 _) = if x1 < x2 then LT else if x1 > x2 then GT else EQ
  recEquate :: HasCallStack => String -> Term -> Bool
  recEquate x n = case lookupDef env x of 
           Just d -> equate_ env d n
           Nothing -> error $ "Expected: " <> show n <> " , found: " <> show (Var x) <> " , env: " <> show env
  equateMaybe _ Nothing Nothing     = True
  equateMaybe env (Just a) (Just b) = equate_ env a b
  equateMaybe _ _ _                 = False
  in trace (toList $ "---> " <> show n1 <> " ---> " <> show n2) $ case (n1, n2) of
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
    (Let xs1 body1, Let xs2 body2)   -> equate_ env body1 body2 && (all (\(x1,x2) -> equate_ env x1 x2) $ zip (sortBy o xs1) (sortBy o xs2))
    (Sigma _ tyA1 tyB1, Sigma _ tyA2 tyB2) -> equateMaybe env tyA1 tyA2 && equateMaybe env tyB1 tyB2
    (Prod a1 a2, Prod b1 b2)         -> equateMaybe env a1 b1 && equateMaybe env a2 b2
    (Var x, _)                       -> recEquate x n2
    (_, Var x)                       -> recEquate x n1
    (Pos _ _,_)                      -> error "No pos"
    (_, Pos _ _)                     -> error "No pos"
    (_,_)                            -> False

ensurePi :: HasCallStack => Env -> Type -> ResultM (Maybe TName, Type, Type)
ensurePi env ty = case whnf env ty of 
    Whnf (Pi mname tyA tyB) -> pure $ (mname, tyA, tyB)
    Whnf nf -> throwError $ err $ ExpectedFunctionType env ty nf

-- poor man's substitution, for now...
subst Nothing param x = x
subst (Just name) param (Ann x y z) = Ann (subst (Just name) param x) (subst (Just name) param y) z
subst (Just name) param Type = Type
subst (Just name) param (Var x) | x == name = trace (toList $ "Substituted " <> name <> " with " <> show param) param
subst (Just name) param x@(Var _) = x
subst (Just name) param (App x y) = App (subst (Just name) param x) (subst (Just name) param y)
subst (Just name) param (Pi x y z) = Pi x (subst (Just name) param y) (subst (Just name) param z)
subst (Just name) param (Lam x Nothing z) = Lam x Nothing (subst (Just name) param z)
subst (Just name) param (Lam x (Just y) z) = Lam x (Just (subst (Just name) param y)) (subst (Just name) param z)
subst (Just name) param s@(Sigma x Nothing Nothing) = s
subst (Just name) param (Sigma x (Just y) Nothing) = Sigma x (Just (subst (Just name) param y)) Nothing
subst (Just name) param (Sigma x (Just y) (Just z)) = Sigma x (Just (subst (Just name) param y)) (Just (subst (Just name) param z))
subst _ _ x@(Comment _) = x
subst a b c = error $ "subst: " <> show a <> " " <> show b <> " " <> show c

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

whnf' env b (App t1 t2) = trace "App" $ case whnf' env b t1 of 
    (Lam x _ body)          -> let ret = trace "Lam" $ whnf' env b $ trace (toList $ "Substituting " <> x <> " with " <> show t2 <> " in " <> show body) $ subst (Just x) t2 body in trace (toList $ " ...done: " <> show ret) ret
    --(Pi (Just name) _ body) -> whnf' (extendCtx (Sig name t2) env) b body
    nf@(Var x) -> trace "Var" $ let nf2 = whnf' env b t2
                  in case lookupDef env x of
                      (Just d) -> trace (toList $ "Just " <> show d) $ whnf' env False (App d nf2)
                      _        -> trace (toList $ "Nothing: " <> show x) $ App nf nf2
    nf      -> trace "->App" $ App nf t2

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

whnf' _ _ t@(Ann _ _ _) = error $ "Unexpected arg to whnf: " <> show t
whnf' _ _ t@(Paren _)   = error $ "Unexpected arg to whnf: " <> show t
whnf' _ _ t@(Pos _ _)   = error $ "Unexpected arg to whnf: " <> show t

whnf' _ _ tm = tm
