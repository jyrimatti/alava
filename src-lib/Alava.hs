{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Alava where

import Foundation (($),(.),null,show,getArgs,putStrLn,fmap,fst,snd,IO,(<>))
import Foundation.Collection (filter,intercalate)
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import qualified Data.Text.Lazy.IO as DT (putStrLn)
import Control.Monad (mapM_)

import qualified Prelude as P

import Equal (Result(Success, Failure))
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
            mapM_ putStrLn $ fmap (\term -> case inferType emptyEnv term of
                Success (a,_) -> display a
                Failure errors -> intercalate "\n" $ showErrors errors
             ) . fmap fst . filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents
        "2" -> case (inferType emptyEnv $ P.head $ fmap fst $ filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents) of
                Success t -> DT.putStrLn $ renderHtml $ fst t
                Failure errors -> mapM_ putStrLn $ showErrors errors
        "3" -> do
            putStrLn $ show . fst . P.head . filter (null . snd) $ parse expr $ fst $ fromBytes UTF8 contents
        _ -> putStrLn "1/2/3"

    where showErrors errors = fmap (\(err,pos) -> show pos <> ": " <> show err <> "\n") errors