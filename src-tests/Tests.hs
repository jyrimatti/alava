{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs, FlexibleContexts #-}
module Main where

import Foundation (($),(.),fmap,fst,IO,(<>),Either(Left,Right),Bool(True,False),toList,uncurry)
import Foundation.IO (readFile)
import Foundation.VFS ((</>),FileName,FilePath)
import Foundation.String (fromBytes,Encoding(UTF8))

import qualified Prelude as P (show,head,(!!))

import Control.Monad (return,(>>=))
import Data.Traversable (traverse)
import Data.Foldable (foldMap)
import Data.Text.Lazy (Text,pack,unpack)

import Control.Monad.Except ()
import Control.Monad.Logger (runNoLoggingT)

import Test.Tasty (TestTree,withResource,defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,assertBool)

import Syntax (SourcePos)
import Error (Error)
import Alava (parse,infer)

files :: [FileName] -> [FileName] -> (IO [Text] -> TestTree) -> TestTree
files names1 names2 = withResource (traverse (uncurry file) (fmap (\n -> ("www",n)) names1 <> fmap (\n -> ("tests",n)) names2)) (\_ -> return ())
  where file :: FilePath -> FileName -> IO Text
        file dir name = fmap (pack . toList . fst . fromBytes UTF8) $ readFile $ dir </> (name <> ".alava")

main :: IO ()
main = defaultMain $ files ["basic_syntax", "dependent"]
                           ["t1", "t2", "records", "conditionals"] tests

tests :: IO [Text] -> TestTree
tests fs = testGroup "Tests" $ fmap (\i -> testCase (P.show i) $ fs >>= (\ts -> doTest $ ts P.!! i)) $ [0..5]

doTest :: Text -> IO ()
doTest content = do
  e <- runNoLoggingT . infer $ P.head $ parse content
  case e of
    Right _ -> assertBool "" True
    Left errors -> assertBool (unpack $ showErrors errors) False

showErrors :: [(Error,SourcePos)] -> Text
showErrors = foldMap (\(err,pos) -> pack $ P.show pos <> ": " <> P.show err <> "\n")