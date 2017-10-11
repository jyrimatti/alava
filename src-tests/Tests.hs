{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs, FlexibleContexts #-}
module Main where

import Foundation (($),(.),null,getArgs,putStrLn,fmap,fst,snd,IO,(<>),Either(Left,Right),pure,mconcat,not,Bool(True,False),filter,toList)
import Foundation.IO (readFile)
import Foundation.VFS ((</>),FileName)
import Foundation.String (fromBytes,Encoding(UTF8))

import qualified Prelude as P (show,head)

import Control.Monad (sequence,return,(>>=))
import Data.Traversable (Traversable)
import Data.Foldable (foldMap)
import Data.Text.Lazy (Text,pack,unpack)

import Control.Monad.Except
import Control.Monad.Logger

import Test.Tasty (TestTree,withResource,defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,assertBool)

import Syntax (SourcePos)
import Environment (emptyEnv)
import Error (Error)
import Alava (parse,infer)

files :: Traversable t => t FileName -> (IO (t Text) -> TestTree) -> TestTree
files names = withResource (sequence $ fmap file names) (\_ -> return ())
  where file :: FileName -> IO Text
        file name = fmap (pack . toList . fst . fromBytes UTF8) $ readFile $ "tests" </> (name <> ".alava")

main :: IO ()
main = defaultMain $ files ["t1", "t2", "t3", "basic_syntax", "dependent", "records", "conditionals"] $ tests

tests :: IO [Text] -> TestTree
tests fs = testGroup "Tests" [parserTests fs]

parserTests :: IO [Text] -> TestTree
parserTests fs =
    testGroup "Parser tests" $
      [ testCase "t1"           $ fs >>= \[f,_,_,_,_,_,_] -> doTest f
      , testCase "t2"           $ fs >>= \[_,f,_,_,_,_,_] -> doTest f
      , testCase "basic_syntax" $ fs >>= \[_,_,_,f,_,_,_] -> doTest f
      , testCase "dependent"    $ fs >>= \[_,_,_,_,f,_,_] -> doTest f
      , testCase "records"      $ fs >>= \[_,_,_,_,_,f,_] -> doTest f
      , testCase "conditionals" $ fs >>= \[_,_,_,_,_,_,f] -> doTest f
      ]

doTest :: Text -> IO ()
doTest content = do
  e <- runNoLoggingT . infer $ P.head $ parse content
  case e of
    Right _ -> assertBool "" True
    Left errors -> assertBool (unpack $ showErrors errors) False

showErrors :: [(Error,SourcePos)] -> Text
showErrors errors = foldMap (\(err,pos) -> pack $ P.show pos <> ": " <> P.show err <> "\n") errors