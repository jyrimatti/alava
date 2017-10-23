{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Foundation (($),(.),getArgs,fst,IO,(<$>),Either(Left,Right),fmap,(<>),toList)
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import Control.Monad.Logger (runStdoutLoggingT)

import Data.Text.Lazy (Text,pack)
import Data.Text.Lazy.IO (putStrLn)
import Control.Monad (mapM_)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Prelude (Show)
import qualified Prelude as P (show,head)

import Alava (parse, infer)
import PrettyPrint (display)
import qualified HtmlPrint (html,eterm)
import Syntax (SourcePos)
import Error (Error)

show :: Show a => a -> Text
show = pack . P.show

main :: IO ()
main = do
    [mode,filepath] <- getArgs
    contents <- (pack . toList . fst) <$> fromBytes UTF8 <$> readFile (unsafeFilePath Relative [unsafeFileName $ toBytes UTF8 filepath])
    case mode of
        "text" -> do
            e <- runStdoutLoggingT . infer . P.head . parse $ contents
            mapM_ putStrLn $ case e of
                Right (t,_) -> [display t]
                Left errors -> showErrors errors
        "html" -> do
            e <- runStdoutLoggingT . infer . P.head . parse $ contents
            mapM_ putStrLn $ case e of
                Right (t,_) -> [renderHtml $ HtmlPrint.html (HtmlPrint.eterm t)]
                Left errors -> showErrors errors
        "parse" -> putStrLn $ show $ P.head $ parse contents
        _ -> putStrLn "text/html/parse"

showErrors :: [(Error, SourcePos)] -> [Text]
showErrors = fmap (\ (err, pos) -> show pos <> ": " <> show err <> "\n")
