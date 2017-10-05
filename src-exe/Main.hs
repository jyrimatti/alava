{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Foundation (($),show,getArgs,putStrLn,fst,IO,(<$>),fromString,Either(Left,Right),String,fmap,(<>))
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import Data.Text.Lazy (unpack)
import Control.Monad (mapM_)

import qualified Prelude as P

import Alava (parse, infer)
import PrettyPrint (display)
import HtmlPrint (renderHtml)
import Syntax (SourcePos)
import Error (Error)

main :: IO ()
main = do
    [mode,filepath] <- getArgs
    contents <- fst <$> fromBytes UTF8 <$> readFile (unsafeFilePath Relative [unsafeFileName $ toBytes UTF8 filepath])
    case mode of
        "text" -> do
            e <- infer contents
            mapM_ putStrLn $ case e of
                Right (t,_) -> [display t]
                Left errors -> showErrors errors
        "html" -> do
            e <- infer contents
            mapM_ putStrLn $ case e of
                Right (t,_) -> [fromString $ unpack $ renderHtml t]
                Left errors -> showErrors errors
        "parse" -> putStrLn $ show $ P.head $ parse contents
        _ -> putStrLn "1/2/3"

showErrors :: [(Error, SourcePos)] -> [String]
showErrors = fmap (\ (err, pos) -> show pos <> ": " <> show err <> "\n")
