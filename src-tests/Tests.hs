{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs #-}
module Main where

import Foundation (($),(.),IO,String,fmap,fst,(<>),not,null)
import Foundation.IO (readFile)
import Foundation.VFS ((</>),FileName)
import Foundation.String (fromBytes,Encoding(UTF8))

import qualified Prelude as P

import Control.Monad (sequence,return,(>>=))
import Data.Traversable (Traversable)

import Test.Tasty (TestTree,withResource,defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,assertBool)

import Syntax ()
import SimpleParser (expr,parse)

files :: Traversable t => t FileName -> (IO (t String) -> TestTree) -> TestTree
files names = withResource (sequence $ fmap file names) (\_ -> return ())
  where file name = fmap (fst . fromBytes UTF8) $ readFile $ "tests" </> (name <> ".alava")

main :: IO ()
main = defaultMain $ files ["t1", "t2", "t3"] $ tests

tests :: IO [String] -> TestTree
tests fs = testGroup "Tests" [parserTests fs]

parserTests :: IO [String] -> TestTree
parserTests fs =
    testGroup "Parser tests" $
      [ testCase "t1" $ fs >>= \[f,_,_] -> let e = parse expr f in (assertBool ("returned: " <> P.show e) $ not $ null e)
      , testCase "t2" $ fs >>= \[_,f,_] -> let e = parse expr f in (assertBool ("returned: " <> P.show e) $ not $ null e)
      , testCase "t3" $ fs >>= \[_,_,f] -> let e = parse expr f in (assertBool ("returned: " <> P.show e) $ not $ null e)
      ]