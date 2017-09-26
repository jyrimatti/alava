{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs, FlexibleContexts #-}
module Main where

import Foundation (($),(.),null,show,getArgs,putStrLn,fmap,fst,snd,IO,(<>),Either(Left,Right),pure,mconcat,String,not,Bool(True,False),toList,filter)
import Foundation.IO (readFile)
import Foundation.VFS ((</>),FileName)
import Foundation.String (fromBytes,Encoding(UTF8))

import qualified Prelude as P

import Control.Monad (sequence,return,(>>=))
import Data.Traversable (Traversable)
import Data.Foldable (foldMap)

import Control.Monad.Except
import Control.Monad.Logger

import Test.Tasty (TestTree,withResource,defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,assertBool)

import Syntax (SourcePos)
import Environment (emptyEnv)
import Equal (Error)
import SimpleParser (expr,parse)
import TypeCheck  (inferType)

files :: Traversable t => t FileName -> (IO (t String) -> TestTree) -> TestTree
files names = withResource (sequence $ fmap file names) (\_ -> return ())
  where file name = fmap (fst . fromBytes UTF8) $ readFile $ "tests" </> (name <> ".alava")

main :: IO ()
main = defaultMain $ files ["t1", "t2", "t3", "basic_syntax", "dependent", "records", "conditionals"] $ tests

tests :: IO [String] -> TestTree
tests fs = testGroup "Tests" [parserTests fs]

parserTests :: IO [String] -> TestTree
parserTests fs =
    testGroup "Parser tests" $
      [ testCase "t1"           $ fs >>= \[f,_,_,_,_,_,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      , testCase "t2"           $ fs >>= \[_,f,_,_,_,_,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      , testCase "t3"           $ fs >>= \[_,_,f,_,_,_,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e))
      , testCase "basic_syntax" $ fs >>= \[_,_,_,f,_,_,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      , testCase "dependent"    $ fs >>= \[_,_,_,_,f,_,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      , testCase "records"      $ fs >>= \[_,_,_,_,_,f,_] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      , testCase "conditionals" $ fs >>= \[_,_,_,_,_,_,f] -> let e = parse expr f in ((assertBool ("returned: " <> P.show e) $ not $ null e) >> doTest e)
      ]

doTest ast = do
  e <- runStdoutLoggingT $ filterLogger (\s l -> False) $ runExceptT (inferType emptyEnv $ P.head $ fmap fst $ filter (null . snd) $ ast)
  case e of
    Right _ -> assertBool "" True
    Left errors -> assertBool (showErrors errors) False

showErrors :: [(Error,SourcePos)] -> P.String
showErrors errors = toList $ foldMap (\(err,pos) -> show pos <> ": " <> show err <> "\n") errors