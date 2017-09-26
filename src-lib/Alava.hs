{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Alava where

import Foundation (($),(.),null,show,getArgs,putStrLn,fmap,fst,snd,IO,(<>),Either(Left,Right),pure,mconcat)
import Foundation.Collection (filter,intercalate)
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import qualified Data.Text.Lazy.IO as DT (putStrLn)
import Control.Monad (mapM,mapM_)

import Control.Monad.Logger
import Control.Monad.Except

import qualified Prelude as P

import SimpleParser (expr,parse)
import TypeCheck  (inferType)
import PrettyPrint (display)
import HtmlPrint (renderHtml)
import Environment (emptyEnv)


main :: IO ()
main = do
    [mode,filepath] <- getArgs
    contents <- readFile $ unsafeFilePath Relative $ [unsafeFileName $ toBytes UTF8 filepath]
    case mode of
        "1" -> do
            foo <- sequence $ fmap (\term -> do
                e <- runStdoutLoggingT $ runExceptT $ inferType emptyEnv term
                pure $ case e of 
                    Right (a,_) -> display a
                    Left errors -> intercalate "\n" $ showErrors errors
             ) . fmap fst . filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents
            mapM_ putStrLn foo
        "2" -> do
            e <- runStdoutLoggingT $ runExceptT (inferType emptyEnv $ P.head $ fmap fst $ filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents)
            case e of
                Right t -> DT.putStrLn $ renderHtml $ fst t
                Left errors -> mapM_ putStrLn $ showErrors errors
        "3" -> do
            putStrLn $ show . fst . P.head . filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents
        _ -> putStrLn "1/2/3"

    where showErrors errors = fmap (\(err,pos) -> show pos <> ": " <> show err <> "\n") errors